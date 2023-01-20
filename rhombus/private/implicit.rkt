#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "provide.rkt"
         "expression.rkt"
         "binding.rkt"
         "repetition.rkt"
         "parse.rkt"
         (submod "function.rkt" for-call)
         (submod "map-ref.rkt" for-ref)
         (submod "list.rkt" for-binding)
         (submod "list.rkt" for-implicit)
         "setmap.rkt"
         "literal.rkt"
         "parens.rkt")

(provide (for-space rhombus/expr
                    #%body
                    #%block
                    #%ref
                    #%literal
                    ;; `#%quotes` provided by "quasiquote.rkt"
                    #%parens
                    #%brackets
                    #%braces
                    #%call)
         (for-space rhombus/bind
                    #%block
                    #%literal
                    #%parens
                    #%brackets
                    #%braces)
         (for-space rhombus/repet
                    #%ref
                    #%literal
                    #%parens
                    #%brackets
                    #%braces
                    #%call))

(module+ for-dynamic-static
  (provide (for-spaces (rhombus/expr
                        rhombus/repet)
                       #%ref
                       static-#%ref
                       #%call
                       static-#%call)))

(define-expression-syntax #%body
  (expression-prefix-operator
   #'#%body
   '((default . stronger))
   'macro
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (~and head ((~and tag (~datum block)) . body)) . tail)
        (values (datum->syntax #f (cons (datum->syntax #'here 'rhombus-body #'tag #'tag) #'body) #'tag)
                #'tail)]))))

(define-expression-syntax #%block
  (expression-transformer
   #'#%block
   (lambda (stxes)
     (syntax-parse stxes
       [(_ b)
        (raise-syntax-error #f
                            "misplaced;\n not allowed as an expression by itself"
                            #'b)]))))

(define-binding-syntax #%block
  (binding-transformer
   #'#%block
   (lambda (stxes)
     (syntax-parse stxes
       [(_ b)
        (raise-syntax-error #f
                            "misplaced;\n not allowed as a binding by itself"
                            #'b)]))))

(define-expression-syntax #%literal
  (expression-transformer
   (in-expression-space #'#%literal)
   (lambda (stxes)
     (syntax-parse stxes
       [(_ datum . tail)
        (when (keyword? (syntax-e #'datum)) (raise-keyword-error #'datum))
        (values (syntax/loc #'datum (quote datum))
                #'tail)]))))

(define-binding-syntax #%literal
  (binding-transformer
   (in-expression-space #'#%literal)
   (lambda (stxes)
     (syntax-parse stxes
       [(_ datum . tail)
        (when (keyword? (syntax-e #'datum)) (raise-keyword-error #'datum))
        (values (binding-form #'literal-infoer
                              #'datum)
                #'tail)]))))

(define-repetition-syntax #%literal
  (repetition-transformer
   (in-repetition-space #'#%literal)
   (lambda (stxes)
     (syntax-parse stxes
       [(_ datum . tail)
        (when (keyword? (syntax-e #'datum)) (raise-keyword-error #'datum))
        (values (make-repetition-info #'datum
                                      #'value
                                      (syntax/loc #'datum (quote datum))
                                      0
                                      0
                                      #'()
                                      #t)
                #'tail)]))))

(define-for-syntax (raise-keyword-error datum)
  (raise-syntax-error #f
                      "misplaced keyword"
                      datum))

(define-expression-syntax #%parens
  (expression-transformer
   #'#%parens
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (~and head ((~datum parens) . args)) . tail)
        (let ([args (syntax->list #'args)])
          (cond
            [(null? args)
             (raise-syntax-error #f "empty expression" #'head)]
            [(pair? (cdr args))
             (raise-syntax-error #f "too many expressions" #'head)]
            [else
             ;; eagerly parse content of parentheses; we could choose to
             ;; delay parsing by using `rhombus-expression`, instead
             (syntax-parse (car args)
               [e::expression (values #'e.parsed #'tail)])]))]))))

(define-binding-syntax #%parens
  (binding-transformer
   #'#%parens
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (~and head ((~datum parens) . args)) . tail)
        (let ([args (syntax->list #'args)])
          (cond
            [(null? args)
             (raise-syntax-error #f "empty pattern" #'head)]
            [(pair? (cdr args))
             (raise-syntax-error #f "too many patterns" #'head)]
            [else
             (syntax-parse (car args)
               [b::binding (values #'b.parsed #'tail)])]))]))))

(define-repetition-syntax #%parens
  (repetition-transformer
   #'#%parens
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (~and head ((~datum parens) . args)) . tail)
        (let ([args (syntax->list #'args)])
          (cond
            [(null? args)
             (raise-syntax-error #f "empty repetition" #'head)]
            [(pair? (cdr args))
             (raise-syntax-error #f "too many repetions" #'head)]
            [else
             (syntax-parse (car args)
               [r::repetition (values #'r.parsed #'tail)])]))]))))

(define-for-syntax (make-#%call-expression static?)
  (expression-infix-operator
   #'#%call
   '((default . stronger))
   'macro
   (lambda (rator stxes)
     (parse-function-call rator '() stxes #:static? static?))
   'left))

(define-expression-syntax #%call (make-#%call-expression #f))
(define-expression-syntax static-#%call (make-#%call-expression #t))

(define-for-syntax (make-#%call-repetition static?)
  (repetition-infix-operator
   (in-repetition-space #'#%call)
   '((default . stronger))
   'macro
   (lambda (rator stxes)
     (parse-function-call rator '() stxes #:static? static? #:repetition? #t))
   'left))

(define-repetition-syntax #%call (make-#%call-repetition #f))
(define-repetition-syntax static-#%call (make-#%call-repetition #t))


(define-expression-syntax #%brackets
  (expression-transformer
   #'#%brackets
   (lambda (stxes)
     (check-brackets stxes)
     (parse-list-expression stxes))))

(define-binding-syntax #%brackets
  (binding-transformer
   #'#%brackets
   (lambda (stxes)
     (check-brackets stxes)
     (parse-list-binding stxes))))

(define-repetition-syntax #%brackets
  (repetition-transformer
   #'#%brackets
   (lambda (stxes)
     (check-brackets stxes)
     (parse-list-repetition stxes))))

(define-for-syntax (check-brackets stxes)
  (syntax-parse stxes
    [(_ (_::brackets . _) . _) (void)]))

(define-for-syntax (make-#%ref more-static?)
  (expression-infix-operator
   #'#%ref
   '((default . stronger))
   'macro
   (lambda (array stxes)
     (parse-map-ref-or-set array stxes more-static?))
   'left))

(define-expression-syntax #%ref
  (make-#%ref #f))
(define-expression-syntax static-#%ref
  (make-#%ref #t))

(define-for-syntax (make-repetition-#%ref more-static?)
  (repetition-infix-operator
   #'#%ref
   '((default . stronger))
   'macro
   (lambda (array stxes)
     (parse-map-ref-or-set array stxes more-static? #:repetition? #t))
   'left))

(define-repetition-syntax #%ref
  (make-repetition-#%ref #f))
(define-repetition-syntax static-#%ref
  (make-repetition-#%ref #t))

(define-expression-syntax #%braces
  (expression-prefix-operator
   #'#%braces
   '((default . stronger))
   'macro
   (lambda (stxes)
     (syntax-parse stxes
       [(_ braces . tail)
        (values (parse-setmap-expression #'braces)
                #'tail)]))))

(define-binding-syntax #%braces
  (binding-prefix-operator
   #'#%braces
   '((default . stronger))
   'macro
   (lambda (stxes)
     (parse-setmap-binding 'braces stxes))))

(define-repetition-syntax #%braces
  (repetition-prefix-operator
   #'#%braces
   '((default . stronger))
   'macro
   (lambda (stxes)
     (syntax-parse stxes
       [(_ braces . tail)
        (values (parse-setmap-expression #'braces #:repetition? #t)
                #'tail)]))))

(begin-for-syntax
  (set-#%call-ids! (quote-syntax #%call)
                   (quote-syntax static-#%call)))
