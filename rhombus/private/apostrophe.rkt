#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         "literal.rkt")

(provide (for-spaces (rhombus/expr
                      rhombus/bind)
                     |#'|))

;; see also "unquote-binding-primitive.rkt"

(define-expression-syntax |#'|
  (expression-prefix-operator
   (in-expression-space #'|#'|)
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens quotes group)
       [(form-name q . tail)
        (check-quotable #'form-name #'q)
        (values (syntax/loc stx (quote q))
                #'tail)]))))

(define-binding-syntax |#'|
  (binding-prefix-operator
   (in-binding-space #'|#'|)
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens quotes group)
       [(form-name q . tail)
        (check-quotable #'form-name #'q)
        (values (binding-form #'literal-infoer
                              #'q)
                #'tail)]))))

(define-for-syntax (check-quotable form-name q)
  (define s (syntax-e q))
  (unless (or (keyword? s)
              (symbol? s))
    (raise-syntax-error #f
                        "only an identifier or keyword allowed"
                        (datum->syntax #f (list form-name q))
                        q)))
