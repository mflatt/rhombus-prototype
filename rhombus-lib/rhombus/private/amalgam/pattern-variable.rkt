#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/hier-name-parse
                     enforest/syntax-local
                     "name-path-op.rkt"
                     "srcloc.rkt"
                     "dotted-sequence.rkt"
                     "pack.rkt")
         "expression.rkt"
         (submod "annotation.rkt" for-class)
         (submod "syntax-class-primitive.rkt" for-quasiquote)
         (submod "dot.rkt" for-dot-provider)
         "dollar.rkt"
         "repetition.rkt"
         "static-info.rkt"
         (submod "syntax-object.rkt" for-quasiquote)
         "syntax-wrap.rkt"
         "dot-provider-key.rkt"
         "syntax-class-attributes-key.rkt"
         "name-root-space.rkt"
         "name-root-ref.rkt"
         "parens.rkt")

(provide (for-syntax make-pattern-variable-bind
                     deepen-pattern-variable-bind
                     extract-pattern-variable-bind-id-and-depth))

(define-for-syntax (make-pattern-variable-bind name-id temp-id unpack* depth
                                               #:attribs [attrib-lists '()]
                                               #:key [key #f])
  (define no-repetition?
    (and (eqv? 0 depth)
         (null? attrib-lists)))
  (define ids (if no-repetition?
                  (list name-id)
                  (list name-id (in-repetition-space name-id))))
  #`[#,ids (make-pattern-variable-syntaxes
             (quote-syntax #,name-id)
             (quote-syntax #,temp-id)
             (quote-syntax #,unpack*)
             #,depth
             (quote-syntax #,attrib-lists)
             #,no-repetition?
             (quote #,key))
     #,@(get-static-infos (datum->syntax #f attrib-lists))])

(define-for-syntax (deepen-pattern-variable-bind sidr)
  (syntax-parse sidr
    [(ids (make-pattern-variable-syntaxes self-id temp-id unpack* depth attrs expr? key) . sis)
     (define new-ids
       (syntax-parse #'ids
         [(id) #`(id #,(in-repetition-space #'id))]
         [_ #'ids]))
     #`(#,new-ids (make-pattern-variable-syntaxes self-id temp-id unpack* #,(add1 (syntax-e #'depth)) attrs #f key) . sis)]))

(define-for-syntax (extract-pattern-variable-bind-id-and-depth sids sid-ref)
  (list (car (syntax-e sids))
        (syntax-parse sid-ref
          [(make-pattern-variable-syntaxes _ _ _ depth . _) #'depth])))


(define-for-syntax (get-static-infos attributes)
  (define stx-si (get-syntax-static-infos))
  (if (null? (syntax-e attributes))
      stx-si
      #`((#%dot-provider ((pattern-variable-dot-provider #,(get-syntax-instances)) #,(get-syntax-instances)))
         (#%syntax-class-attributes #,attributes))))

(define-for-syntax (make-pattern-variable-syntaxes name-id temp-id unpack* depth attributes no-repetition? key)
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
                                (wrap-static-info* #`(if (syntax*? #,temp-id)
                                                         (syntax-wrap
                                                          (syntax-unwrap #,temp-id)
                                                          (quote #,key)
                                                          (hasheq
                                                           #,@(apply
                                                               append
                                                               (for/list ([var (in-list (syntax->list attributes))])
                                                                 (define attr (syntax-list->pattern-variable var))
                                                                 (list #`(quote #,(car (syntax-e var)))
                                                                       (pattern-variable-val-id attr))))))
                                                         #,temp-id)
                                                   (get-static-infos attributes)))
                            #'tail)])))
  (cond
    [no-repetition?
     (if (null? (syntax-e attributes))
         (expression-repeatable-transformer
          id-handler)
         (expression-repeatable-transformer
          (lambda (stx)
            (expr-handler stx
                          (lambda ()
                            (id-handler stx))))))]
    [else
     (define attrs (syntax->list attributes))
     (define attr-tmps (generate-temporaries attrs))
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
         #`(if (syntax*? elem)
               (syntax-wrap
                (syntax-unwrap elem)
                (quote #,key)
                (hasheq
                 #,@(apply
                     append
                     (for/list ([var (in-list attrs)]
                                [tmp (in-list attr-tmps)])
                       (list #`(quote #,(car (syntax-e var)))
                             tmp)))))
               elem)])
      (lambda () (get-static-infos attributes))
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
      #:expr-handler expr-handler)]))

(define-syntax pattern-variable-dot-provider
  (dot-provider
   (lambda (form1 dot field-id
                  tail
                  more-static?
                  repetition?
                  success-k fail-k)
     (define-values (attributes depth)
       (cond
         [repetition?
          (syntax-parse form1
            [rep-info::repetition-info
             (values (static-info-lookup #'rep-info.element-static-infos #'#%syntax-class-attributes)
                     (length (syntax->list #'rep-info.for-clausess)))])]
         [else
          (values
           (or (syntax-local-static-info form1 #'#%syntax-class-attributes)
               #'())
           0)]))
     (define attr (for/or ([var (in-list (syntax->list attributes))])
                    (and (eq? (syntax-e field-id) (syntax-e (car (syntax-e var))))
                         (syntax-list->pattern-variable var))))
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

(begin-for-syntax
  (set-parse-syntax-of-annotation!
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::parens (group seq::dotted-identifier-sequence)) . tail)
        (define (bad [constraint ""])
          (raise-syntax-error #f (string-append "not a syntax class" constraint) stx #'seq))
        (syntax-parse #'seq
          [(~var name (:hier-name-seq in-name-root-space in-syntax-class-space name-path-op name-root-ref))
           (define rsc (syntax-local-value* (in-syntax-class-space #'name.name) syntax-class-ref))
           (cond
             [rsc
              (unless (rhombus-syntax-class-key rsc)
                (bad " with fields"))
              (define (root-swap attrs)
                (cond
                  [(rhombus-syntax-class-root-swap rsc)
                   => (lambda (swap)
                        (datum->syntax
                         #f
                         (cons
                          (pattern-variable->list
                           (pattern-variable (cdr swap)
                                             'dummy 'dummy
                                             0
                                             (case (rhombus-syntax-class-kind rsc)
                                               [(group) #'unpack-group*]
                                               [(term) (if (rhombus-syntax-class-splicing? rsc)
                                                           #'unpack-element*
                                                           #'unpack-term*)]
                                               [else #'unpack-element*])))
                          (for/list ([attr (in-list (syntax->list attrs))]
                                     #:do [(define pv (syntax-list->pattern-variable attr))]
                                     #:unless (eq? (car swap) (pattern-variable-sym pv)))
                            attr))))]
                  [else attrs]))
              (values
               (annotation-predicate-form #`(lambda (v)
                                              (and (syntax-wrap? v)
                                                   (eq? (syntax-wrap-key v) (quote #,(rhombus-syntax-class-key rsc)))))
                                          (get-static-infos (root-swap (rhombus-syntax-class-attributes rsc))))
               #'tail)]
             [else (bad)])]
          [else (bad)])]))))
