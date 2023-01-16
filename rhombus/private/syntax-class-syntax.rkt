#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt")
         syntax/parse/pre
         (submod "quasiquote.rkt" convert)
         (submod "syntax-class.rkt" for-quasiquote)
         (submod "syntax-class.rkt" for-syntax-class-syntax)
         "syntax-class-clause.rkt"
         "definition.rkt"
         "name-root.rkt"
         "parse.rkt"
         "parsed.rkt"
         "pack.rkt"
         "parens.rkt"
         (only-in "def+let.rkt" def)
         (rename-in "ellipsis.rkt"
                    [... rhombus...])
         (rename-in "equal.rkt"
                    [= rhombus=])
         "rest-marker.rkt"
         "function-arity.rkt")

(provide (rename-out [rhombus-syntax syntax])
         (for-space rhombus/syntax_class_clause
                    pattern
                    description
                    kind
                    error_mode))

(define-simple-name-root rhombus-syntax
  class
  only)

(define-name-root only
  #:fields
  [class only-class])

;; should this be in "definition.rkt"?
(define-syntax parsed-defn
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        [(_ (_ d)) #'(d)]))))

(begin-for-syntax
  (define-syntax-class :attribute-lhs
    #:datum-literals (brackets group op)
    #:literals (rhombus...)
    (pattern id:identifier
             #:attr depth #'0)
    (pattern (brackets (group a::attribute-lhs) (group (op rhombus...)))
             #:attr id #'a.id
             #:attr depth #`#,(+ 1 (syntax-e #'a.depth)))))

(define-for-syntax (generate-pattern-and-attributes orig-stx stx kind splicing?)
  (define (generate pat body)
    (define in-quotes
      (cond
        [(eq? kind 'multi)
         (syntax-parse pat
           [(_ g ...) #'(multi g ...)])]
        [(eq? kind 'block)
         (syntax-parse pat
           #:datum-literals (block)
           [(_ (~and g (group (block . _)))) #'g]
           [_ (raise-syntax-error #f
                                  "not a block pattern"
                                  orig-stx
                                  pat)])]
        [else
         (syntax-parse pat
           [(_ g) #'g]
           [(_)
            (if (and (eq? kind 'term) splicing?)
                #'(group)
                (raise-syntax-error #f
                                    "no groups in pattern"
                                    orig-stx
                                    pat))]
           [_ (raise-syntax-error #f
                                  "multiple groups in pattern"
                                  orig-stx
                                  pat)])]))
    (define-values (p idrs sidrs vars can-be-empty?)
      (convert-pattern #:splice? (not (eq? kind 'group))
                       #:splice-pattern (and (not (eq? kind 'group))
                                             (not splicing?)
                                             (lambda (ps)
                                               (cond
                                                 [(eq? kind 'multi)
                                                  #`(_ #,@ps)]
                                                 [(= 1 (length ps))
                                                  (car ps)]
                                                 [else
                                                  (raise-syntax-error #f
                                                                      "not a single-term pattern"
                                                                      orig-stx
                                                                      pat)])))
                       in-quotes))
    (define-values (pattern-body explicit-attrs)
      (for/fold ([body-forms null]
                 [attrs null]
                 #:result
                 (values
                  (reverse body-forms)
                  (reverse attrs)))
                ([g (in-list (syntax->list body))])
        (syntax-parse g
          #:datum-literals (group block)
          [(group #:attr attr::attribute-lhs (block in-block ...))
           (values
            (cons #`(group def attr.id (block in-block ...)) body-forms)
            (cons (pattern-variable (syntax-e #'attr.id) #'attr.id (syntax-e #'attr.depth) (quote-syntax unpack-element*)) attrs))]
          [other
           (values (cons #'other body-forms) attrs)])))
    (define all-attrs (append vars explicit-attrs))
    (with-syntax ([((attr ...) ...)
                   (map (lambda (var)
                          #`(#:attr
                             (#,(pattern-variable-sym var) #,(pattern-variable-depth var))
                             (#,(pattern-variable-unpack*-id var)
                              (quote-syntax dots)
                              #,(pattern-variable-val-id var)
                              #,(pattern-variable-depth var))))
                        all-attrs)]
                  [(body-form ...) pattern-body]
                  [([val-id val-rhs] ...) idrs]
                  [([stx-id stx-rhs] ...) sidrs])
      (values #`(pattern #,p #:do [(define val-id val-rhs)
                                   ...
                                   (define-syntax stx-id stx-rhs)
                                   ...
                                   (define-values #,(map pattern-variable-val-id explicit-attrs)
                                     (rhombus-body
                                      body-form ...
                                      (group (parsed (values #,@(map pattern-variable-val-id explicit-attrs))))))]
                         attr ... ...)
              all-attrs)))
  (syntax-parse stx
    #:datum-literals (alts group quotes block)
    [(block (group (~and pat (quotes . _))))
     (generate #'pat #'())]
    [(block (group (~and pat (quotes . _))
                   (block body ...)))
     (generate #'pat #'(body ...))]))

(define-for-syntax (generate-syntax-class stx define-syntax-id class-name class-formals class-arity
                                          kind-kw alts description-expr opaque?)
  (define-values (kind splicing?)
    (let ([kind (string->symbol (keyword->string kind-kw))])
      (cond
        [(eq? kind 'sequence) (values 'term #t)]
        [else (values kind #f)])))
  (define-values (patterns attributes)
    (for/lists (patterns attributess
                         #:result (values patterns (intersect-attributes stx attributess)))
        ([alt-stx (in-list alts)])
      (generate-pattern-and-attributes stx alt-stx kind splicing?)))
  (define define-class (if splicing?
                           #'define-splicing-syntax-class
                           #'define-syntax-class))
  (list
   #`(#,define-class #,(if (syntax-e class-formals)
                           #`(#,class-name . #,class-formals)
                           class-name)
      #:description #,(or description-expr #f)
      #:datum-literals (block group quotes)
      #,@(if opaque? '(#:opaque) '())
      #,@patterns)
   #`(#,define-syntax-id #,(in-syntax-class-space class-name)
      (rhombus-syntax-class '#,kind
                            #'#,class-name
                            (quote-syntax #,(map pattern-variable->list attributes))
                            #,splicing?
                            '#,class-arity))))

(define-for-syntax (intersect-attributes stx attributess)
  (cond
    [(null? attributess) '()]
    [(null? (cdr attributess)) (car attributess)]
    [else
     ;; start with initial set
     (define ht0
       (for/hasheq ([var (in-list (car attributess))])
         (values (pattern-variable-sym var) var)))
     ;; intersect by pruning set
     (define ht
       (for/fold ([ht0 ht0]) ([attributes (in-list (cdr attributess))])
         (for/fold ([ht #hasheq()]) ([var (in-list attributes)])
           (define prev-var (hash-ref ht0 (pattern-variable-sym var) #f))
           (if prev-var
               (hash-set ht
                         (pattern-variable-sym var)
                         (intersect-var stx var prev-var))
               ht))))
     ;; convert back to list
     (hash-values ht #t)]))

(define-for-syntax (intersect-var stx a b)
  (unless (eqv? (pattern-variable-depth a) (pattern-variable-depth b))
    (raise-syntax-error #f
                        "attribute with different depths in different clauses"
                        stx
                        (pattern-variable-sym a)))
  ;; keeping the same unpack, if possible, enables optimizations for
  ;; tail repetitions; otherwise, the term is sufficiently normalized
  ;; by matching that we can just use `unpack-term*`
  (if (free-identifier=? (pattern-variable-unpack*-id a) (pattern-variable-unpack*-id b))
      a
      (let ([special? (lambda (unpack*-id)
                        (or (free-identifier=? unpack*-id #'unpack-tail-list*)
                            (free-identifier=? unpack*-id #'unpack-multi-tail-list*)))])
        (cond
          [(or (special? (pattern-variable-unpack*-id a))
               (special? (pattern-variable-unpack*-id b)))
           (raise-syntax-error #f
                               "attribute with incompatible tail modes in different clauses"
                               stx
                               (pattern-variable-sym a))]
          [else
           (struct-copy pattern-variable a
                        [unpack*-id #'unpack-term*])]))))

(begin-for-syntax
  (define (kw->symbol kw-stx)
    (datum->syntax kw-stx
                   (string->symbol
                    (keyword->string
                     (syntax-e kw-stx)))
                   kw-stx))

  (define-splicing-syntax-class :class-arg
    #:attributes ([formal 1] kw def?)
    #:datum-literals (group op)
    #:literals (rhombus=)
    (pattern (group id:identifier)
             #:attr [formal 1] (list #'id)
             #:attr kw #'#f
             #:attr def? #'#f)
    (pattern (group kw:keyword)
             #:attr [formal 1] (list #'kw (kw->symbol #'kw))
             #:attr def? #'#f)
    (pattern (group kw:keyword (_::block (group id:identifier)))
             #:attr [formal 1] (list #'kw #'id)
             #:attr def? #'#f)
    (pattern (group id:identifier = rhs ...+)
             #:attr [formal 1] (list #`[id (rhombus-expression (#,group-tag rhs ...))])
             #:attr kw #'#f
             #:attr def? #'#t)
    (pattern (group id:identifier (tag::block body ...+))
             #:attr [formal 1] (list #`[id (rhombus-body-at tag body ...)])
             #:attr kw #'#f
             #:attr def? #'#t)
    (pattern (group kw:keyword = rhs ...+)
             #:attr [formal 1] (list #'kw #`[#,(kw->symbol #'kw) (rhombus-expression (#,group-tag rhs ...))])
             #:attr def? #'#f)
    (pattern (group kw:keyword (_::block (group id:identifier = rhs ...+)))
             #:attr [formal 1] (list #'kw #`[id (rhombus-expression (#,group-tag rhs ...))])
             #:attr def? #'#t)
    (pattern (group kw:keyword (_::block (group id:identifier (tag::block body ...+))))
             #:attr [formal 1] (list #'kw #`[id (rhombus-body-at tag body ...)])
             #:attr def? #'#t))

  (define-splicing-syntax-class :class-args
    #:attributes (formals arity)
    #:datum-literals (group)
    #:literals (&)
    (pattern (~seq)
             #:attr formals #'#f
             #:attr arity #'#f)
    (pattern (~seq (_::parens arg::class-arg ... (group & id:identifier))) 
             #:attr formals #'(arg.formal ... ... . id)
             #:attr arity (datum->syntax
                           #f
                           (summarize-arity #'(arg.kw ...) #'(arg.def? ...) #t #f)))
    (pattern (~seq (_::parens arg::class-arg ...))
             #:attr formals #'(arg.formal ... ...)
             #:attr arity (datum->syntax
                           #f
                           (summarize-arity #'(arg.kw ...) #'(arg.def? ...) #f #f)))))

(define-for-syntax (make-class-definer define-class-id )
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        ;; Classname and patterns shorthand
        [(form-id class-name args::class-args (_::alts alt ...))
         (generate-syntax-class stx define-class-id #'class-name #'args.formals #'args.arity
                                '#:sequence (syntax->list #'(alt ...)) #f #f)]
        ;; Specify patterns with "pattern"
        [(form-id class-name args::class-args
                  (_::block clause::syntax-class-clause ...))
         (define-values (pattern-alts kind-kw class-desc opaque?)
           (extract-clauses stx (syntax->list #'(clause.parsed ...))))
         (generate-syntax-class stx define-class-id #'class-name #'args.formals #'args.arity
                                kind-kw pattern-alts class-desc opaque?)]))))

(define-for-syntax (extract-clauses stx clauses)
  (define options
    (for/fold ([options #hasheq()]) ([clause (in-list clauses)])
      (define (check what)
        (syntax-parse clause
          [(kw (~and orig-stx (_ id . _)) . _)
           (when (hash-ref options (syntax-e #'kw) #f)
             (raise-syntax-error #f
                                 (string-append "found second " what "clause, but only one is allowed")
                                 stx
                                 #'orig-stx))]))
      (syntax-parse clause
        [(#:pattern _ alts)
         (check "pattern")
         (hash-set options '#:pattern (syntax->list #'alts))]
        [(#:description _ e)
         (check "description")
         (hash-set options '#:description #'e)]
        [(#:kind _ kw)
         (check "kind")
         (hash-set options '#:kind (syntax-e #'kw))]
        [(#:error-mode _ kw)
         (check "error mode")
         (hash-set options '#:error-mode (syntax-e #'kw))])))
  (define alts (hash-ref options '#:pattern #f))
  (unless alts
    (raise-syntax-error #f
                        "missing pattern alternatives"
                        stx))
  (values alts
          (hash-ref options '#:kind '#:sequence)
          (hash-ref options '#:description #f)
          (eq? (hash-ref options '#:error-mode #f) '#:opaque)))

(define-syntax-class-clause-syntax pattern
  (syntax-class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (_::alts b ...))
        #`(#:pattern #,stx (b ...))]))))

(define-syntax-class-clause-syntax description
  (syntax-class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (tag::block e ...+))
        #`(#:description #,stx (rhombus-body-at tag e ...))]))))

(define-for-syntax (make-kw-clause tag-kw valid what)
  (syntax-class-clause-transformer
   (lambda (stx)
     (define (parse-keyword kw)
       (if (memq (syntax-e kw) valid)
           kw
           (raise-syntax-error #f
                               (string-append "not a recognized " what)
                               stx
                               kw)))
     (syntax-parse stx
       #:datum-literals (group)
       [(_ kw:keyword) #`(#,tag-kw #,stx #,(parse-keyword #'kw))]
       [(_ (_::block (group kw:keyword))) #`(#,tag-kw #,stx #,(parse-keyword #'kw))]))))

(define-syntax-class-clause-syntax kind (make-kw-clause '#:kind '(#:term #:sequence #:group #:multi #:block) "kind"))
(define-syntax-class-clause-syntax error_mode (make-kw-clause '#:error-mode '(#:opaque #:transparent) "error mode"))

(define-syntax class (make-class-definer #'define-syntax))
(define-syntax only-class (make-class-definer #'define-syntax-class-syntax))
