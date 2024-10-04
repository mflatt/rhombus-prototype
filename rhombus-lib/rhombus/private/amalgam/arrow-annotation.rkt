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
         "values-key.rkt"
         "static-info.rkt"
         "if-blocked.rkt"
         "parens.rkt"
         "realm.rkt")

(provide (for-space rhombus/annot
                    ->
                    #%parens))

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
  (arrow-annotation (for/list ([arg (in-list non-rest-args)])
                      (syntax-parse arg
                        #:datum-literals (group op)
                        [(group kw:keyword (_::block (group name:identifier _:::-bind c ... _::equal _::_-bind)))
                         (list #'kw #'name #t (syntax-parse #'(group c ...)
                                                [c::annotation #'c.parsed]))]
                        [(group kw:keyword (_::block (group c ... _::equal _::_-bind)))
                         (list #'kw #f #t (syntax-parse #'(group c ...)
                                            [c::annotation #'c.parsed]))]
                        [(group kw:keyword (_::block (group name:identifier _:::-bind c ...)))
                         (list #'kw #'name #f (syntax-parse #'(group c ...)
                                                [c::annotation #'c.parsed]))]
                        [(group kw:keyword (_::block c::annotation))
                         (list #'kw #f #f #'c.parsed)]
                        [(group name:identifier _:::-bind c ... _::equal _::_-bind)
                         (list #f #'name #t (syntax-parse #'(group c ...)
                                              [c::annotation #'c.parsed]))]
                        [(group c ... _::equal _::_-bind)
                         (list #f #f #t (syntax-parse #'(group c ...)
                                          [c::annotation #'c.parsed]))]
                        [(group name:identifier _:::-bind c ...)
                         (list #f #'name #f (syntax-parse #'(group c ...)
                                              [c::annotation #'c.parsed]))]
                        [c::annotation (list #f #f #f #'c.parsed)]))
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
     (define arity (summarize-arity (datum->syntax #f multi-kws) (datum->syntax #f multi-opts)
                                    (and rest-name+ann #t) (and kw-rest-name+ann #t)))
     (define static-infos
       #`((#%function-arity #,arity)
          (#%call-result #,(let ([sis #'(r.static-infos ...)])
                             (define sis-l (syntax->list sis))
                             (if (= 1 (length sis-l))
                                 (car sis-l)
                                 #`((#%values #,sis)))))))
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
    [(_ arg-id (result-id arity
                          ([lhs::binding-info lhs-body lhs-static-infos lhs-str lhs-kw lhs-name lhs-opt] ...)
                          rest
                          kw-rest
                          kw-rest-first?
                          ([rhs-i::binding-form rhs-body rhs-static-infos rhs-name rhs-str] ...)))
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
           #'(define result-id
               (let ([f arg-id])
                 (maybe-make-keyword-procedure
                  ...
                  (lambda (kw-arg-id ... lhs-arg ... ... . rest-arg-id)
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
                      (lambda () (f/kw-apply f kw-id ... left-kw+id ... ... . rest-id))
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
                         (raise-result-arity-error 'result-id '#,(length (syntax->list #'(res-in-id ...))) args)]))))))))))]))

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
