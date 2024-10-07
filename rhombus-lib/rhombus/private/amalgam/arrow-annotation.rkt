#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     enforest/name-parse
                     "annotation-string.rkt"
                     "srcloc.rkt")
         racket/unsafe/undefined
         "treelist.rkt"
         (only-in "annotation.rkt" ::)
         (submod "annotation.rkt" for-class)
         (submod "annotation.rkt" for-arrow)
         "binding.rkt"
         (submod "equal.rkt" for-parse)
         "op-literal.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "function-arity.rkt"
         (submod "define-arity.rkt" for-info)
         "values-key.rkt"
         "static-info.rkt"
         "if-blocked.rkt"
         "parens.rkt"
         "realm.rkt"
         "sorted-list-subset.rkt")

(provide (for-space rhombus/annot
                    ->
                    #%parens))

(module+ for-arrow-annot
  (provide (for-syntax parse-arrow-all-of)))

(define-annotation-syntax #%parens
  (annotation-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (~and head (_::parens . args)) . tail)
        (let ([args (syntax->list #'args)])
          (syntax-parse #'tail
            [(arrow::name . _)
             #:when (free-identifier=? (in-annotation-space #'arrow.name) (annot-quote ->))
             (parens-arrow-annotation #'arrow.name args #'head #'tail)]
            [_
             (cond
               [(null? args)
                (raise-syntax-error #f "empty annotation" #'head)]
               [(pair? (cdr args))
                (raise-syntax-error #f "too many annotations" #'head)]
               [else
                (syntax-parse (car args)
                  [c::annotation
                   (values (relocate+reraw #'head #'c.parsed) #'tail)])])]))]))))

(define-annotation-syntax ->
  (annotation-infix-operator
   (lambda () `((default . stronger)))
   'macro
   (lambda (lhs stx)
     (arrow-annotation (list (list #f #f #f lhs)) #f #f #f #f lhs stx))
   'right))

(begin-for-syntax
  (define-syntax-class ::-bind
    #:attributes ()
    #:description "`::` operator"
    #:opaque
    (pattern ::name
             #:when (free-identifier=? (in-binding-space #'name)
                                       (bind-quote ::)))))

(define-for-syntax (parens-arrow-annotation arrow-name args head tail)
  (define-values (non-rest-args rest-name+ann rest-ann-whole? kw-rest-name+ann kw-rest-first?)
    (extract-rest-args arrow-name args))
  (arrow-annotation (let loop ([args non-rest-args] [has-opt? #f])
                      (cond
                        [(null? args) null]
                        [else
                         (define arg (car args))
                         (define (non-opt)
                           (when has-opt?
                             (raise-syntax-error #f "non-optional argument follows an optional by-position argument"
                                                 arrow-name
                                                 arg)))
                         (define-values (a opt?)
                           (syntax-parse arg
                             #:datum-literals (group op)
                             [(group kw:keyword (_::block (group name:identifier _:::-bind c ... _::equal _::_-bind)))
                              (values (list #'kw #'name #t (syntax-parse #'(group c ...)
                                                             [c::annotation #'c.parsed]))
                                      #f)]
                             [(group kw:keyword (_::block (group c ... _::equal _::_-bind)))
                              (values (list #'kw #f #t (syntax-parse #'(group c ...)
                                                         [c::annotation #'c.parsed]))
                                      #f)]
                             [(group kw:keyword (_::block (group name:identifier _:::-bind c ...)))
                              (values (list #'kw #'name #f (syntax-parse #'(group c ...)
                                                             [c::annotation #'c.parsed]))
                                      #f)]
                             [(group kw:keyword (_::block c::annotation))
                              (values (list #'kw #f #f #'c.parsed)
                                      #f)]
                             [(group name:identifier _:::-bind c ... _::equal _::_-bind)
                              (values (list #f #'name #t (syntax-parse #'(group c ...)
                                                           [c::annotation #'c.parsed]))
                                      #t)]
                             [(group c ... _::equal _::_-bind)
                              (values (list #f #f #t (syntax-parse #'(group c ...)
                                                       [c::annotation #'c.parsed]))
                                      #t)]
                             [(group name:identifier _:::-bind c ...)
                              (non-opt)
                              (values (list #f #'name #f (syntax-parse #'(group c ...)
                                                           [c::annotation #'c.parsed]))
                                      #f)]
                             [c::annotation
                              (non-opt)
                              (values (list #f #f #f #'c.parsed)
                                      #f)]))
                         (cons a (loop (cdr args) (or has-opt? opt?)))]))
                    rest-name+ann rest-ann-whole?
                    kw-rest-name+ann kw-rest-first?
                    head tail))

(define-for-syntax (extract-rest-args arrow-name args)
  (let loop ([args args] [non-rest-accum '()] [rest-name+ann #f] [rest-ann-whole? #f]
                         [kw-rest-name+ann #f] [kw-rest-first? #f])
    (define (parse-ann c)
      (syntax-parse #`(group . #,c)
        #:datum-literals (group)
        [(group name:identifier _:::-bind . c)
         (syntax-parse #'(group . c)
           [c::annotation (list #'name #'c.parsed)])]
        [c::annotation (list #f #'c.parsed)]))
    (syntax-parse args
      #:datum-literals (group)
      [() (values (reverse non-rest-accum) rest-name+ann rest-ann-whole? kw-rest-name+ann kw-rest-first?)]
      [((group dots::...-bind) . _)
       (raise-syntax-error #f "misplaced ellipsis" arrow-name #'dots)]
      [(g (group dots::...-bind) . args)
       (when rest-name+ann
         (raise-syntax-error #f "second rest argument not allowed" arrow-name #'dots))
       (syntax-parse #'g
         [c::annotation
          (loop #'args non-rest-accum (list #f #'c.parsed) #f kw-rest-name+ann kw-rest-first?)])]
      [((group amp::&-bind . c) . args)
       (when rest-name+ann
         (raise-syntax-error #f "second rest argument not allowed" arrow-name #'amp))
       (define name+ann (parse-ann #'c))
       (loop #'args non-rest-accum name+ann #t kw-rest-name+ann kw-rest-first?)]
      [((group amp::~&-bind . c) . args)
       (when kw-rest-name+ann
         (raise-syntax-error #f "second keyword-rest argument not allowed" arrow-name #'amp))
       (define name+ann (parse-ann #'c))
       (loop #'args non-rest-accum rest-name+ann rest-ann-whole? name+ann (not rest-name+ann))]
      [(g . args)
       (when (or rest-name+ann kw-rest-name+ann)
         (raise-syntax-error #f "non-rest argument not allowed after rest argument" arrow-name #'g))
       (loop #'args (cons #'g non-rest-accum) rest-name+ann rest-ann-whole? kw-rest-name+ann kw-rest-first?)])))

(define-for-syntax (arrow-annotation multi-kw+name+opt+lhs
                                     rest-name+ann rest-ann-whole?
                                     kw-rest-name+ann kw-rest-first?
                                     head stx)
  (define arrow (syntax-parse stx [(a . _) #'a]))
  (define-values (multi-name+rhs loc tail)
    (syntax-parse stx
      [(_ (~and p-res (_::parens res ...)) . tail)
       (values (for/list ([res (in-list (syntax->list #'(res ...)))])
                 (syntax-parse res
                   #:datum-literals (group)
                   [(group name:identifier _:::-bind c ...)
                    (list #'name (syntax-parse #'(group c ...)
                                   [c::annotation #'c.parsed]))]
                   [c::annotation (list #f #'c.parsed)]))
               (datum->syntax #f (list head arrow #'p-res))
               #'tail)]
      [(_ . tail)
       #:with (~var res (:annotation-infix-op+form+tail arrow)) #`(group . tail)
       (values (list (list #f #'res.parsed))
               (datum->syntax #f (list head arrow #'res.parsed))
               #'res.tail)]))
  (define multi-kws (map car multi-kw+name+opt+lhs))
  (define multi-names (map cadr multi-kw+name+opt+lhs))
  (define multi-opts (map caddr multi-kw+name+opt+lhs))
  (define multi-lhs (map cadddr multi-kw+name+opt+lhs))
  (define multi-rhs (map cadr multi-name+rhs))
  (define arity (summarize-arity (datum->syntax #f multi-kws) (datum->syntax #f multi-opts)
                                 (and rest-name+ann #t) (and kw-rest-name+ann #t)))
  (syntax-parse (list multi-lhs multi-name+rhs)
    [((l::annotation-binding-form ...) ((rhs-name r::annotation-binding-form) ...))
     #:with (lhs-i::binding-form ...) #'(l.binding ...)
     #:with (lhs-impl::binding-impl ...) #'((lhs-i.infoer-id () lhs-i.data) ...)
     #:with (lhs::binding-info ...) #'(lhs-impl.info ...)
     #:with (arg-id ...) (for/list ([name-id (in-list (syntax->list #'(lhs.name-id ...)))])
                           ((make-syntax-introducer) (datum->syntax #f (syntax-e name-id))))
     #:with (lhs-str ...) (map shrubbery-syntax->string multi-lhs)
     #:with (rhs-str ...) (map shrubbery-syntax->string multi-rhs)
     #:with (lhs-kw ...) multi-kws
     #:with (lhs-name ...) multi-names
     #:with (lhs-opt ...) multi-opts
     #:with who 'function
     (define static-infos
       #`((#%function-arity #,arity)
          (#%call-result #,(let ([sis #'(r.static-infos ...)])
                             (define sis-l (syntax->list sis))
                             (if (= 1 (length sis-l))
                                 (car sis-l)
                                 #`((#%values #,sis)))))
          #,@(indirect-get-function-static-infos)))
     (values
      (relocate+reraw
       loc
       (annotation-binding-form
        (binding-form
         #'arrow-infoer
         #`[who #,arity
                ([lhs l.body l.static-infos lhs-str lhs-kw lhs-name lhs-opt] ...)
                #,(and rest-name+ann
                       (syntax-parse rest-name+ann
                         [(name a::annotation-binding-form)
                          #`(name a.binding a.body a.static-infos #,rest-ann-whole? #,(shrubbery-syntax->string #'a))]
                         [_ #f]))
                #,(and kw-rest-name+ann
                       (syntax-parse kw-rest-name+ann
                         [(name a::annotation-binding-form)
                          #`(name a.binding a.body a.static-infos #,(shrubbery-syntax->string #'a))]
                         [_ #f]))
                #,kw-rest-first?
                ([r.binding r.body r.static-infos rhs-name rhs-str] ...)
                #,static-infos])
        (if (and (andmap not multi-kws)
                 (andmap not multi-opts)
                 (not rest-name+ann)
                 (not kw-rest-name+ann))
            ;; simple case, indirection through `lambda` to get name from context
            #'(lambda (arg-id ...) (who arg-id ...))
            ;; complex case:
            #'who)
        static-infos))
      tail)]))

(define-for-syntax (parse-arrow-all-of stx)
  (syntax-parse stx
    [(form-id (~and args (_::parens g ...)) . tail)
     #:with (a::annotation ...) #'(g ...)
     (define loc (datum->syntax #f (list #'form-id #'args)))
     (for ([a (in-list (syntax->list #'(a.parsed ...)))]
           [g (in-list (syntax->list #'(g ...)))])
       (unless (syntax-parse a
                 [a::annotation-binding-form
                  #:with b::binding-form #'a.binding
                  (free-identifier=? #'b.infoer-id #'arrow-infoer)]
                 [_ #f])
         (raise-syntax-error #f "not a function annotation" loc g)))
     (syntax-parse #'(a.parsed ...)
       [(a::annotation-binding-form ...)
        #:with (ab::binding-form ...) #'(a.binding ...)
        #:with ([_ arity . _] ...) #'(ab.data ...)
        #:with who 'function
        (define all-arity (union-arity-summaries (syntax->datum #'(arity ...))))
        (define static-infos
          #`((#%function-arity #,all-arity)
             (#%call-result (#:at_arities
                             #,(for/list ([sis (syntax->list #'(a.static-infos ...))]
                                          [arity (syntax->list #'(arity ...))])
                                 #`[#,arity
                                    #,(static-info-lookup sis #'#%call-result)])))
             #,@(indirect-get-function-static-infos)))
        (values
         (relocate+reraw
          loc
          (annotation-binding-form
           (binding-form #'all-of-infoer
                         #`(who #,all-arity ([ab.infoer-id ab.data] ...) #,static-infos))
           #'who
           static-infos))
         #'tail)])]))

(define-syntax (arrow-infoer stx)
  (syntax-parse stx
    [(_ in-static-infos (result-id arity lhss rest kw-rest kw-rest-first? rhs static-infos))
     (binding-info "function"
                   #'function
                   #'static-infos
                   #'((result (0) . static-infos))
                   #'arrow-matcher
                   #'arrow-committer
                   #'arrow-binder
                   #'(result-id arity lhss rest kw-rest kw-rest-first? rhs))]))

(define-syntax (arrow-matcher stx)
  (syntax-parse stx
    [(_ arg-id (result-id arity lhss _ _ _ rhs) IF success fail)
     #`(IF (and (procedure? arg-id)
                #,(let ([a (syntax-e #'arity)])
                    (if (and (integer? a)
                             (positive? a)
                             (zero? (bitwise-and a (sub1 a))))
                        #`(procedure-arity-includes? arg-id #,(sub1 (integer-length a)))
                        #`(function-arity-match? arg-id 'arity))))
           success
           fail)]))

(define-syntax (arrow-committer stx)
  (syntax-parse stx
    [(_ arg-id data)
     #'(begin)]))

(define-syntax (arrow-binder stx)
  (syntax-parse stx
    [(_ arg-id data)
     (do-arrow-binder #'arg-id #'data)]))

(define-for-syntax (do-arrow-binder arg-id data)
  (syntax-parse data
    [(result-id arity
                ([lhs::binding-info lhs-body lhs-static-infos lhs-str lhs-kw lhs-name lhs-opt] ...)
                rest
                kw-rest
                kw-rest-first?
                ([rhs-i::binding-form rhs-body rhs-static-infos rhs-name rhs-str] ...))
     #:with (rhs-impl::binding-impl ...) #`((rhs-i.infoer-id () rhs-i.data) ...)
     #:with (rhs::binding-info ...) #'(rhs-impl.info ...)
     #:with (((lhs-bind-id lhs-bind-use . lhs-bind-static-infos) ...) ...) #'(lhs.bind-infos ...)
     #:with (((rhs-bind-id rhs-bind-use . rhs-bind-static-infos) ...) ...) #'(rhs.bind-infos ...)
     #:with (lhs-arg-id ...) (for/list ([name-id (in-list (syntax->list #'(lhs.name-id ...)))])
                               ((make-syntax-introducer) (datum->syntax #f (syntax-e name-id))))
     #:with (left-id ...) (for/list ([name (in-list (syntax->list #'(lhs-name ...)))]
                                     [default-id (in-list (generate-temporaries #'(lhs-arg-id ...)))])
                            (if (syntax-e name)
                                name
                                default-id))
     #:with (res-in-id ...)  (for/list ([name-id (in-list (syntax->list #'(rhs.name-id ...)))])
                               ((make-syntax-introducer) (datum->syntax #f (syntax-e name-id))))
     #:with (res-id ...)  (for/list ([name (in-list (syntax->list #'(rhs-name ...)))]
                                     [default-id (in-list (generate-temporaries #'(res-in-id ...)))])
                            (if (syntax-e name)
                                name
                                default-id))
     #:with (check-not-undefined ...) (for/list ([opt (in-list (syntax->list #'(lhs-opt ...)))])
                                        (if (syntax-e #'p)
                                            #'(lambda (v) (not (eq? v unsafe-undefined)))
                                            #'(lambda (v) #t)))
     (with-syntax ([((lhs-arg ...) ...)
                    (for/list ([arg-id (in-list (syntax->list #'(lhs-arg-id ...)))]
                               [kw (in-list (syntax->list #'(lhs-kw ...)))]
                               [opt (in-list (syntax->list #'(lhs-opt ...)))]
                               ;; If there's a keyword-rest arg, we'll have to
                               ;; extract arguments manually
                               #:unless (and (syntax-e #'kw-rest) (syntax-e kw)))
                      (define var (if (not (syntax-e opt))
                                      arg-id
                                      (list arg-id #'unsafe-undefined)))
                      (if (syntax-e kw)
                          (list kw var)
                          (list var)))]
                   [((left-kw+id ...) ...)
                    (for/list ([left-id (in-list (syntax->list #'(left-id ...)))]
                               [kw (in-list (syntax->list #'(lhs-kw ...)))])
                      (if (syntax-e kw)
                          (list kw left-id)
                          (list left-id)))]
                   [(f-apply rest-arg-id rest-id (rest-bind ...))
                    (syntax-parse #'rest
                      [#f (list #'#%app '() (if (syntax-e #'kw-rest) '(null) '()) '())]
                      [(name a-i::binding-form a-body a-static-infos whole? a-str)
                       #:with a-impl::binding-impl #`(a-i.infoer-id () a-i.data)
                       #:with a::binding-info #'a-impl.info
                       #:with ((a-bind-id a-bind-use . a-bind-static-infos) ...) #'a.bind-infos
                       #:with rest-list-id (or (and (syntax-e #'name) #'name) #'rest-list)
                       (define a-block
                         #'(let ()
                             (a.matcher-id rest-arg-id a.data if/flattened (void)
                                           (raise-rest-argument-annotation-failure 'result-id rest-arg-id 'a-str whole?))
                             (a.committer-id rest-arg-id a.data)
                             (a.binder-id rest-arg-id a.data)
                             (define-static-info-syntax/maybe a-bind-id . a-bind-static-infos)
                             ...
                             a-body))
                       (list #'apply #'rest-arg-id #'(rest-id)
                             (if (syntax-e #'whole?)
                                 #`([(rest-list-id)
                                     a-static-infos
                                     (let ([rest-arg-id (list->treelist rest-arg-id)])
                                       #,a-block)]
                                    [(rest-id) () (rest-treelist->list rest-list-id)])
                                 #`([(rest-id)
                                     ()
                                     (for/list ([rest-arg-id (in-list rest-arg-id)])
                                       #,a-block)])))])])
       (with-syntax ([((maybe-make-keyword-procedure ...) (kw-arg-id ...) (kw-id ...) f/kw-apply (kw-preamble ...) (kw-rest-bind ...))
                      (syntax-parse #'kw-rest
                        [#f (list #'(begin) '() '() #'f-apply '() '())]
                        [(name a-i::binding-form a-body a-static-infos a-str)
                         #:with a-impl::binding-impl #`(a-i.infoer-id () a-i.data)
                         #:with a::binding-info #'a-impl.info
                         #:with ((a-bind-id a-bind-use . a-bind-static-infos) ...) #'a.bind-infos
                         #:with kw-rest-map-id (or (and (syntax-e #'name) #'name) #'kw-rest-map)
                         (define kws (for/list ([kw (in-list (syntax->list #'(lhs-kw ...)))]
                                                #:when (syntax-e kw))
                                       kw))
                         (define req-kws (for/list ([kw (in-list (syntax->list #'(lhs-kw ...)))]
                                                    [opt (in-list (syntax->list #'(lhs-opt ...)))]
                                                    #:when (and (syntax-e kw)
                                                                (not (syntax-e opt))))
                                           kw))
                         (define arity-mask (car (syntax-e #'arity)))
                         (list #`(make-keyword-procedure/reduce-arity #,req-kws #,arity-mask)
                               #'(kws-arg-id kw-vals-arg-id) #'(kws-id kw-vals-id) #'keyword-apply
                               (if (null? kws)
                                   (list #'(define kw-map (keywords->map kws-arg-id kw-vals-arg-id)))
                                   (append
                                    (list #'(define kw-map/all (keywords->map kws-arg-id kw-vals-arg-id)))
                                    (for/list ([arg-id (in-list (syntax->list #'(lhs-arg-id ...)))]
                                               [kw (in-list (syntax->list #'(lhs-kw ...)))]
                                               [opt (in-list (syntax->list #'(lhs-opt ...)))]
                                               ;; If there's a keyword-rest arg, we'll have to
                                               ;; extract arguments manually
                                               #:when (syntax-e kw))
                                      #`(define #,arg-id (hash-ref kw-map/all '#,kw unsafe-undefined)))
                                    (list #`(define kw-map (drop-keywords kw-map/all '#,kws)))))
                               #'([(kw-rest-map-id)
                                   a-static-infos
                                   (let ()
                                      (a.matcher-id kw-map a.data
                                                    if/flattened
                                                    (void)
                                                    (raise-keyword-rest-argument-annotation-failure 'result-id kw-map 'a-str))
                                      (a.committer-id kw-map a.data)
                                      (a.binder-id kw-map a.data)
                                      (define-static-info-syntax/maybe a-bind-id . a-bind-static-infos)
                                      ...
                                      a-body)]
                                  [(kws-id kw-vals-id)
                                   ()
                                   (rest-map->keywords kw-rest-map-id)]))])])
         (with-syntax ([(rest-bind ...) (if (syntax-e #'kw-rest-first?)
                                            #'(kw-rest-bind ... rest-bind ...)
                                            #'(rest-bind ... kw-rest-bind ...))])
           (define inner-proc
             #`(lambda (kw-arg-id ... lhs-arg ... ... . rest-arg-id)
                 kw-preamble ...
                 (let*-values-with-static-infos
                  ([(left-id)
                    lhs-static-infos
                    (cond
                      [(check-not-undefined lhs-arg-id)
                       (lhs.matcher-id lhs-arg-id lhs.data
                                       if/flattened
                                       (void)
                                       (raise-argument-annotation-failure 'result-id lhs-arg-id 'lhs-str))
                       (lhs.committer-id lhs-arg-id lhs.data)
                       (lhs.binder-id lhs-arg-id lhs.data)
                       (define-static-info-syntax/maybe lhs-bind-id . lhs-bind-static-infos)
                       ...
                       lhs-body]
                      [else lhs-arg-id])]
                   ...
                   rest-bind ...)
                  (call-with-values
                   (lambda ()
                     ;; At first by-position argument that's `undefined`, stop passing by-position arguments
                     (cond
                       #,@(let loop ([args (syntax->list #'((left-kw+id ...) ...))]
                                     [kws (syntax->list #'(lhs-kw ...))]
                                     [opts (syntax->list #'(lhs-opt ...))]
                                     [accum '()])
                            (cond
                              [(null? args) '()]
                              [(or (not (syntax-e (car opts)))
                                   (syntax-e (car kws)))
                               (loop (cdr args) (cdr kws) (cdr opts)
                                     (cons (car args) accum))]
                              [else
                               (cons
                                #`[(eq? #,(car (syntax-e (car args))) unsafe-undefined)
                                   #,(with-syntax ([((left-kw+id ...) ...)
                                                    (append (reverse accum)
                                                            (for/list ([arg (in-list (cdr args))]
                                                                       [kw (in-list (cdr kws))]
                                                                       #:when (syntax-e kw))
                                                              arg))])
                                       #`(f/kw-apply f kw-id ... left-kw+id ... ... . rest-id))]
                                (loop (cdr args) (cdr kws) (cdr opts) (cons (car args) accum)))]))
                       [else
                        (f/kw-apply f kw-id ... left-kw+id ... ... . rest-id)]))
                   (case-lambda
                     [(res-in-id ...)
                      (let*-values-with-static-infos
                       ([(res-id)
                         rhs-static-infos
                         (let ()
                           (rhs.matcher-id res-in-id rhs.data
                                           if/flattened
                                           (void)
                                           (raise-result-annotation-failure 'result-id res-in-id 'rhs-str))
                           (rhs.committer-id result rhs.data)
                           (rhs.binder-id res-in-id rhs.data)
                           (define-static-info-syntax/maybe rhs-bind-id . rhs-bind-static-infos)
                           ...
                           rhs-body)]
                        ...)
                       (values res-id ...))]
                     [args
                      (raise-result-arity-error 'result-id '#,(length (syntax->list #'(res-in-id ...))) args)])))))
           (if (not arg-id)
               inner-proc
               #`(define result-id
                   (let ([f #,arg-id])
                     (maybe-make-keyword-procedure
                      ...
                      #,inner-proc)))))))]))

(define-syntax (all-of-infoer stx)
  (syntax-parse stx
    [(_ in-static-infos (result-id all-arity cases static-infos))
     (binding-info "function"
                   #'function
                   #'static-infos
                   #'((result (0) . static-infos))
                   #'all-of-matcher
                   #'all-of-committer
                   #'all-of-binder
                   #'(result-id all-arity cases))]))

(define-syntax (all-of-matcher stx)
  (syntax-parse stx
    [(_ arg-id (result-id all-arity ([a-infoer (~and a-data (_ arity . _))] ...)) IF success fail)
     #`(IF (and (procedure? arg-id)
                #,@(for/list ([arity (in-list (syntax->list #'(arity ...)))]
                              [a-infoer (in-list (syntax->list #'(a-infoer ...)))]
                              [a-data (in-list (syntax->list #'(a-data ...)))])
                     (let ([a (syntax-e arity)])
                       (if (and (integer? a)
                                (positive? a)
                                (zero? (bitwise-and a (sub1 a))))
                           #`(procedure-arity-includes? arg-id #,(sub1 (integer-length a)))
                           #`(function-arity-match? arg-id '#,arity)))))
           success
           fail)]))

(define-syntax (all-of-committer stx)
  (syntax-parse stx
    [(_ arg-id data)
     #'(begin)]))

(define-syntax (all-of-binder stx)
  (syntax-parse stx
    [(_ arg-id (result-id all-arity ([a-infoer (~and a-data (_ arity . _))] ...)))
     (define arities (syntax->list #'(arity ...)))
     (define no-keywords? (andmap (lambda (a) (integer? (syntax-e a))) arities))
     (cond
       [(and no-keywords?
             (andmap (lambda (a)
                       (let* ([a (abs (syntax-e a))])
                         (zero? (bitwise-and a (sub1 a)))))
                     arities))
        ;; No keywords, no optional arguments other than rests, so
        ;; generate a `case-lambda` for the dispatch
        #`(define result-id
            (let ([f arg-id])
              (case-lambda
                #,@(for/list ([arity (in-list arities)]
                              [infoer (in-list (syntax->list #'(a-infoer ...)))]
                              [data (in-list (syntax->list #'(a-data ...)))])
                     (syntax-parse #`(#,infoer () #,data)
                       [a-impl::binding-impl 
                        #:with a::binding-info #'a-impl.info
                        #:with ((a-bind-id a-bind-use . a-bind-static-infos) ...) #'a.bind-infos
                        ;; Since we started with `->` annotations, we know that we can
                        ;; skip the matcher and committer
                        (define mask (syntax-e arity))
                        (define args (let loop ([a mask] [n 0])
                                       (cond
                                         [(= a 1) '()]
                                         [(= a -1) 'rest-args]
                                         [else (cons (string->symbol (format "arg~a" n))
                                                     (loop (arithmetic-shift a -1)
                                                           (add1 n)))])))
                        (if (mask . < . 0)
                            #`[#,args (apply #,(do-arrow-binder #f #'a.data) #,@(let loop ([args args])
                                                                                  (if (pair? args)
                                                                                      (cons (car args) (loop (cdr args)))
                                                                                      (list args))))]
                            #`[#,args (#,(do-arrow-binder #f #'a.data) #,@args)])])))))]
       [else
        ;; Some keywords or optional arguments
        (define body
          #`(let ([len (length args)])
              (cond
                #,@(for/list ([arity (in-list arities)]
                              [infoer (in-list (syntax->list #'(a-infoer ...)))]
                              [data (in-list (syntax->list #'(a-data ...)))])
                     (syntax-parse #`(#,infoer () #,data)
                       [a-impl::binding-impl 
                        #:with a::binding-info #'a-impl.info
                        #:with ((a-bind-id a-bind-use . a-bind-static-infos) ...) #'a.bind-infos
                        ;; Since we started with `->` annotations, we know that we can
                        ;; skip the matcher and committer
                        (define-values (mask required-kws allowed-kws)
                          (syntax-parse arity
                            [(mask required allowed) (values (syntax-e #'mask)
                                                             (syntax->datum #'required)
                                                             (syntax->datum #'allowed))]
                            [_ (values (syntax-e arity) null null)]))
                        #`[(and (bitwise-bit-set? #,mask len)                                
                                #,(if (null? required-kws)
                                      #'#t
                                      #`(sorted-list-subset? '#,required-kws kws))
                                #,(if (or no-keywords?
                                          (eq? allowed-kws #f))
                                      #'#t
                                      #`(sorted-list-subset? kws '#,allowed-kws)))
                           #,(if no-keywords?
                                 #`(apply #,(do-arrow-binder #f #'a.data) args)
                                 #`(keyword-apply #,(do-arrow-binder #f #'a.data) kws kw-args args))]]))
                [else (error 'result-id "no matching case for arguments")])))
        (if no-keywords?
            #`(define result-id
                (let ([f arg-id])
                  (procedure-reduce-arity-mask
                   (lambda args #,body)
                   'all-arity)))
            (syntax-parse #'all-arity
              [(mask req allow)
               #`(define result-id
                   (let ([f arg-id])
                     (procedure-reduce-keyword-arity-mask
                      (make-keyword-procedure (lambda (kws kw-args . args) #,body))
                      'mask
                      'req
                      'allow)))]))])]))

(define-syntax (let*-values-with-static-infos stx)
  (syntax-parse stx
    [(_ () body)
     #'body]
    [(_ ([ids () rhs] . binds) body)
     #'(let-values ([ids rhs])
         (let*-values-with-static-infos
          binds
          body))]
    [(_ ([(id) static-infos rhs] . binds) body)
     #'(let-values ([(tmp) (let ([id  rhs]) id)])
         (define id tmp)
         (define-static-info-syntax/maybe id . static-infos)
         (let*-values-with-static-infos
          binds
          body))]))

(define (raise-argument-annotation-failure who val ctc)
  (raise-binding-failure who "argument" val ctc))

(define (raise-result-annotation-failure who val ctc)
  (raise-binding-failure who "result" val ctc))

(define (raise-rest-argument-annotation-failure who val ctc whole?)
  (if whole?
      (raise-binding-failure who "rest-argument list" val ctc)
      (raise-binding-failure who "argument" val ctc)))

(define (raise-keyword-rest-argument-annotation-failure who val ctc)
  (raise-binding-failure who "keyword rest-argument map" val ctc))

(define (raise-result-arity-error who n args)
  (apply raise-result-arity-error*
         who rhombus-realm
         n
         #f
         args))

(define (function-arity-match? f arity)
  (define-values (f-req-kws f-allow-kws) (procedure-keywords f))
  (cond
    [(number? arity)
     (define mask arity)
     (and (null? f-req-kws)
          (= mask (bitwise-and mask (procedure-arity-mask f))))]
    [else
     (define mask (car arity))
     (define req-kws (cadr arity))
     (define allow-kws (caddr arity))
     (and (= mask (bitwise-and mask (procedure-arity-mask f)))
          (if (not allow-kws)
              (not f-allow-kws)
              (or (not f-allow-kws)
                  (for/and ([allow-kw (in-list allow-kws)])
                    (memq allow-kw f-allow-kws))))
          (for/and ([f-req-kw (in-list f-req-kws)])
            (memq f-req-kw req-kws)))]))

(define (rest-treelist->list l)
  (cond
    [(treelist? l)
     (treelist->list l)]
    [else (error "rest annotation converted to a non-List value")]))

(define (keywords->map kws kw-vals)
  (for/hashalw ([kw (in-list kws)]
                [kw-val (in-list kw-vals)])
    (values kw kw-val)))

(define (rest-map->keywords h)
  (cond
    [(hash? h)
     (define kws (hash-keys h #t))
     (values kws
             (for/list ([kw (in-list kws)])
               (hash-ref h kw)))]
    [else (error "keyword-rest annotation converted to a non-Map value")]))

(define (drop-keywords h kws)
  (for/fold ([h h]) ([kw (in-list kws)])
    (hash-remove h kw)))

(define-syntax (make-keyword-procedure/reduce-arity stx)
  (syntax-parse stx
    [(_ () _ e) #'(make-keyword-procedure e)]
    [(_ (kw ...) arity-mask e) #'(procedure-reduce-keyword-arity-mask
                                  (make-keyword-procedure e)
                                  arity-mask
                                  '(kw ...)
                                  #f)]))
