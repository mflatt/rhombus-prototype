#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "operator-parse.rkt"
                     enforest/property
                     enforest/syntax-local)
         "expression.rkt"
         "static-info.rkt"
         "ref-result-key.rkt")

(provide (rename-out [rhombus... ...]))

(module+ for-repeat
  (provide
   (for-syntax make-repetition
               repetition-as-list)))

(begin-for-syntax
  (property repetition expression-prefix-operator (seq-id element-static-infos))

  (define (make-repetition name seq-id element-static-infos)
    (repetition
     name '((default . stronger)) 'macro
     (lambda (stx)
       (syntax-parse stx
         [(self . _)
          (raise-syntax-error #f
                              "cannot use repetition binding as an expression"
                              #'self)]))
     seq-id
     element-static-infos)))

(define-syntax rhombus...
  (expression-transformer
   #'rhombus...
   (lambda (stx)
     (syntax-parse stx
       [(op::operator . tail)
        (raise-syntax-error #f
                            "misuse outside of a binding or constructor"
                            #'op.name)]))))

(define-for-syntax (repetition-as-list ellipses stx)
  (syntax-parse stx
    #:datum-literals (group)
    [(group rep-name:id)
     #:do [(define rep (syntax-local-value* #'rep-name repetition-ref))]
     #:when rep
     (wrap-static-info (repetition-seq-id rep)
                       #'#%ref-result
                       (repetition-element-static-infos rep))]
    [_
     (raise-syntax-error (syntax-e ellipses)
                         "not bound as a repetition"
                         stx)]))
