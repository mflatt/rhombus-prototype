#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "expression.rkt"
         (submod "syntax-class-primitive.rkt" for-quasiquote)
         (submod "dot.rkt" for-dot-provider)
         "dollar.rkt"
         "repetition.rkt"
         "static-info.rkt"
         (submod "syntax-object.rkt" for-quasiquote)
         "syntax-wrap.rkt"
         "dot-provider-key.rkt")

(provide (for-syntax make-pattern-variable-bind
                     deepen-pattern-variable-bind
                     extract-pattern-variable-bind-id-and-depth
                     make-syntax-class-dot-provider))

(define-for-syntax (make-pattern-variable-bind name-id temp-id unpack* depth
                                               #:attribs [attrib-lists '()]
                                               #:key [key #f]
                                               #:dot-provider-id [dot-provider-id #f]
                                               #:bind-dot? [bind-dot? #f])
  (define no-repetition?
    (and (eqv? 0 depth)
         (for/and ([a (in-list attrib-lists)])
           (eqv? 0 (pattern-variable-depth (list->pattern-variable a))))))
  (define ids (append
               (if no-repetition?
                   (list name-id)
                   (list name-id (in-repetition-space name-id)))
               (if bind-dot?
                   (list dot-provider-id)
                   null)))
  #`[#,ids (make-pattern-variable-syntaxes
             (quote-syntax #,name-id)
             (quote-syntax #,temp-id)
             (quote-syntax #,unpack*)
             #,depth
             (quote-syntax #,attrib-lists)
             #,no-repetition?
             (quote #,key)
             #,(and dot-provider-id
                    #`(quote-syntax #,dot-provider-id))
             #,bind-dot?)])

(define-for-syntax (deepen-pattern-variable-bind sidr)
  (syntax-parse sidr
    [(ids (make-pattern-variable-syntaxes self-id temp-id unpack* depth attrs expr? key dot-provider-id bind-dot?))
     (define new-ids
       (syntax-parse #'ids
         [(id) #`(id #,(in-repetition-space #'id))]
         [(id dot-id)
          #:when (syntax-e #'bind-dot?)
          #`(id #,(in-repetition-space #'id) dot-id)]
         [_ #'ids]))
     #`(#,new-ids (make-pattern-variable-syntaxes self-id temp-id unpack* #,(add1 (syntax-e #'depth)) attrs #f key dot-provider-id bind-dot?))]))

(define-for-syntax (extract-pattern-variable-bind-id-and-depth sids sid-ref)
  (list (car (syntax-e sids))
        (syntax-parse sid-ref
          [(make-pattern-variable-syntaxes _ _ _ depth . _) #'depth])))

(define-for-syntax (make-pattern-variable-syntaxes name-id temp-id unpack* depth attributes no-repetition? key dot-provider-id bind-dot?)
  (define (lookup-attribute stx var-id attr-id want-repet?)
    (define attr (for/or ([var (in-list (syntax->list attributes))])
                   (and (eq? (syntax-e attr-id) (syntax-e (car (syntax-e var))))
                        (syntax-list->pattern-variable var))))
    ;; complain if a repetition field is not being used as such, but
    ;; don't complain if a field is not found, because maybe the dot is
    ;; an access of a `Syntax` method
    (when attr
      (unless (eq? want-repet? (not (eqv? 0 (+ depth (pattern-variable-depth attr)))))
        (raise-syntax-error #f
                            (format
                             (string-append (if want-repet?
                                                "field is not a repetition\n"
                                                "field is a repetition\n")
                                            "  pattern: ~a\n"
                                            "  field: ~a")
                             (syntax-e var-id)
                             (syntax-e attr-id))
                            stx)))
    attr)
  (define (get-static-infos)
    (define stx-si (get-syntax-static-infos))
    (if dot-provider-id
        #`((#%dot-provider ((#,dot-provider-id #,(get-syntax-instances)) #,(get-syntax-instances))))
        stx-si))
  (define expr-handler
    (lambda (stx fail)
      (syntax-parse stx
        #:datum-literals (op |.|)
        [(var-id (op |.|) attr-id . tail)
         #:do [(define attr (lookup-attribute stx #'var-id #'attr-id #f))]
         #:when attr
         (values (wrap-static-info* (pattern-variable-val-id attr)
                                    (get-syntax-static-infos))
                 #'tail)]
        [_
         (if (eqv? depth 0)
             (id-handler stx)
             (fail))])))
  (define id-handler
    (lambda (stx)
      (syntax-parse stx
        [(_ . tail) (values (if (null? (syntax-e attributes))
                                (wrap-static-info* temp-id (get-syntax-static-infos))
                                (wrap-static-info* #`(syntax-wrap
                                                      #,temp-id
                                                      (quote #,key)
                                                      (hasheq
                                                       #,@(apply
                                                           append
                                                           (for/list ([var (in-list (syntax->list attributes))])
                                                             (define attr (syntax-list->pattern-variable var))
                                                             (list #`(quote #,(car (syntax-e var)))
                                                                   (pattern-variable-val-id attr))))))
                                                   (get-static-infos)))
                            #'tail)])))
  (define dot (and bind-dot? (make-syntax-class-dot-provider attributes)))
  (cond
    [no-repetition?
     (define e
       (if (null? (syntax-e attributes))
           (expression-repeatable-transformer
            id-handler)
           (expression-repeatable-transformer
            (lambda (stx)
              (expr-handler stx
                            (lambda ()
                              (id-handler stx)))))))
     (if bind-dot?
         (values e dot)
         e)]
    [else
     (define attrs (syntax->list attributes))
     (define attr-tmps (generate-temporaries attrs))
     (define-values (e r)
       (make-expression+repetition
        (cond
          [(= depth 0)
           #'()]
          [(null? attrs)
           #`(([(elem) (in-list (#,unpack* #'$ #,temp-id #,depth))])
              #,@(for/list ([i (in-range (sub1 depth))])
                   #`([(elem) (in-list elem)])))]
          [else
           #`(([(elem) (in-list (#,unpack* #'$ #,temp-id #,depth))]
               #,@(for/list ([var (in-list attrs)]
                             [tmp (in-list attr-tmps)])
                    (define attr (syntax-list->pattern-variable var))
                    #`[(#,tmp) (in-list #,(pattern-variable-val-id attr))]))
              #,@(for/list ([i (in-range (sub1 depth))])
                   #`([(elem) (in-list elem)]
                      #,@(for/list ([tmp (in-list attr-tmps)])
                           #`[(#,tmp) (in-list #,tmp)]))))])
        (cond
          [(= depth 0)
           (let-values ([(e tail) (id-handler #'(x))])
             e)]
          [(null? attrs)
           #'elem]
          [else
           #`(syntax-wrap
              elem
              (quote #,key)
              (hasheq
               #,@(apply
                   append
                   (for/list ([var (in-list attrs)]
                              [tmp (in-list attr-tmps)])
                     (list #`(quote #,(car (syntax-e var)))
                           tmp)))))])
        get-static-infos
        #:repet-handler (lambda (stx next)
                          (syntax-parse stx
                            #:datum-literals (op |.|)
                            [(var-id (~and dot-op (op |.|)) attr-id . tail)
                             #:do [(define attr (lookup-attribute stx #'var-id #'attr-id #t))]
                             #:when attr
                             (define var-depth (+ (pattern-variable-depth attr) depth))
                             (values (make-repetition-info (respan #'(var-id dot-op attr-id))
                                                           #`(([(elem) (in-list
                                                                        (#,(pattern-variable-unpack* attr)
                                                                         #'$
                                                                         #,(pattern-variable-val-id attr)
                                                                         #,var-depth))])
                                                              #,@(for/list ([i (in-range (sub1 var-depth))])
                                                                   #`([(elem) (in-list elem)])))
                                                           #'elem
                                                           (get-syntax-static-infos)
                                                           0)
                                     #'tail)]
                            [_ (next)]))
        #:expr-handler expr-handler))
     (if bind-dot?
         (values e r dot)
         (values e r))]))

(define-for-syntax (make-syntax-class-dot-provider syntax-class-id-or-attributes)
  (dot-provider
   (lambda (form1 dot field-id
                  tail
                  more-static?
                  repetition?
                  success-k fail-k)
     (define attributes (if (identifier? syntax-class-id-or-attributes)
                            (rhombus-syntax-class-attributes
                             (syntax-local-value (in-syntax-class-space syntax-class-id-or-attributes)) )
                            syntax-class-id-or-attributes))
     (define attr (for/or ([var (in-list (syntax->list attributes))])
                    (and (eq? (syntax-e field-id) (syntax-e (car (syntax-e var))))
                         (syntax-list->pattern-variable var))))
     (define depth (if repetition?
                       (syntax-parse form1
                         [rep-info::repetition-info (length (syntax->list #'rep-info.for-clausess))])
                       0))
     (cond
       [attr
        (unless (eq? (and repetition? #t)
                     (not (eqv? 0 (+ depth (pattern-variable-depth attr)))))
          (raise-syntax-error #f
                              (if repetition?
                                  "field is not a repetition"
                                  "field is a repetition")
                              (datum->syntax #f (list form1 dot field-id))
                              field-id))
        (values
         (cond
           [repetition?
            (define var-depth (pattern-variable-depth attr))
            (syntax-parse form1
              [form-rep-info::repetition-info
               (define e #`(#,(pattern-variable-unpack* attr)
                            #'$
                            (hash-ref (syntax-wrap-attribs form-rep-info.body)
                                      (quote #,(pattern-variable-sym attr)))                                                
                            #,var-depth))
               (make-repetition-info (respan (datum->syntax #f (list form1 dot field-id)))
                                     (cond
                                       [(= var-depth 0)
                                        #'form-rep-info.for-clausess]
                                       [else
                                        #`(#,@#'form-rep-info.for-clausess
                                           ([(elem) (in-list #,e)])
                                           #,@(for/list ([i (in-range (sub1 var-depth))])
                                                #`([(elem) (in-list elem)])))])
                                     (cond
                                       [(= var-depth 0) e]
                                       [else #'elem])
                                     (get-syntax-static-infos)
                                     0)])]
           [else
            (wrap-static-info* #`(hash-ref (syntax-wrap-attribs #,form1)
                                           (quote #,(pattern-variable-sym attr)))
                               (get-syntax-static-infos))])
         tail)]
       [else
        (fail-k)]))))
