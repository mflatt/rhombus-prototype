#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/stx
                     enforest/transformer
                     enforest/sequence
                     enforest/property
                     enforest/proc-name
                     "expression-space.rkt"))

(begin-for-syntax
  (provide (property-out definition-transformer)
           (property-out definition-sequence-transformer)

           check-definition-result)

  (property definition-transformer transformer)
  (property definition-sequence-transformer sequence-transformer)

  (define (check-definition-result forms proc)
    (unless (stx-list? forms) (raise-result-error (proc-name proc) "stx-list?" forms))
    forms))

(provide define-definition-syntax)

(define-syntax (define-definition-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-expression-space #'name) rhs))]))
