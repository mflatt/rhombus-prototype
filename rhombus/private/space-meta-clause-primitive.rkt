#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/symbol)
         "space-meta-clause.rkt"
         "parens.rkt"
         "parse.rkt")

(provide (for-space rhombus/space_meta_clause
                    export_syntax_class
                    export_syntax_class_prefix_more
                    export_syntax_class_infix_more
                    description
                    operator_description
                    macro_result
                    identifier_transformer))

(module+ for-space-meta-macro
  (provide rhombus-space-meta-clause
           (for-syntax parse-space-meta-clause-options)))

(define-syntax rhombus-space-meta-clause 'placeholder)

(define-for-syntax (wrap-clause parsed)
  #`[(group (parsed (quote-syntax (rhombus-space-meta-clause #,parsed) #:local)))])

(define-for-syntax (make-identifier-transformer kw)
  (space-meta-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ id:identifier)
        (wrap-clause #`(#,kw id))]))))

(define-space-meta-clause-syntax export_syntax_class
  (make-identifier-transformer '#:syntax_class))
(define-space-meta-clause-syntax export_syntax_class_prefix_more
  (make-identifier-transformer '#:syntax_class_prefix_more))
(define-space-meta-clause-syntax export_syntax_class_infix_more
  (make-identifier-transformer '#:syntax_class_infix_more))

(define-for-syntax (make-expression-transformer kw)
  (space-meta-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id (tag::block g ...+))
        (wrap-clause #`(#,kw #,stx (rhombus-body-at tag g ...)))]
       [(form-id e ...)
        (wrap-clause #`(#,kw #,stx (rhombus-expression (group e ...))))]))))

(define-space-meta-clause-syntax description
  (make-expression-transformer '#:desc))
(define-space-meta-clause-syntax operator_description
  (make-expression-transformer '#:operator_desc))
(define-space-meta-clause-syntax macro_result
  (make-expression-transformer '#:macro_result))
(define-space-meta-clause-syntax identifier_transformer
  (make-expression-transformer '#:identifier_transformer))

(define-for-syntax (parse-space-meta-clause-options orig-stx enforest? options-stx)
  (for/fold ([options #hasheq()]) ([option (syntax->list options-stx)])
    (define (check what #:enforest-only? [enforest-only? #f])
      (syntax-parse option
        [(kw stx . _)
         (unless (or enforest? (not enforest-only?))
           (raise-syntax-error #f (format "~a not allowed in a transformer" what) orig-stx #'stx))
         (when (hash-ref options (syntax-e #'kw) #f)
           (raise-syntax-error #f (format "multiple ~a declared" what) orig-stx #'stx))]))
    (syntax-parse option
      [(_ (#:syntax_class id))
       (check "syntax class exports")
       (hash-set options '#:syntax_class #'id)]
      [(_ (#:syntax_class_prefix_more id))
       (check "prefix-more syntax class exports" #:enforest-only? #t)
       (hash-set options '#:syntax_class_prefix_more #'id)]
      [(_ (#:syntax_class_infix_more id))
       (check "infix-more syntax class exports" #:enforest-only? #t)
       (hash-set options '#:syntax_class_infix_more #'id)]
      [(_ (#:desc stx e))
       (check "description string expressions")
       (hash-set options '#:desc #'e)]
      [(_ (#:operator_desc stx e))
       (check "operator description string expressions" #:enforest-only? #t)
       (hash-set options '#:operator_desc #'e)]
      [(_ (#:macro_result stx e))
       (check "macro result function expressions")
       (hash-set options '#:macro_result #'e)]
      [(_ (#:identifier_transformer stx e))
       (check "identifier transformer expressions" #:enforest-only? #t)
       (hash-set options '#:identifier_transformer #'e)]
      [else
       (error "unhandled" option)])))
