#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "expression.rkt"
         "parse.rkt")

(provide (for-space rhombus/expr
                    (rename-out [rhombus-begin begin])))

(define-expression-syntax rhombus-begin
  (expression-transformer
   #'rhombus-begin
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts block group)
       [(form-id ((~and tag block) form ...)
                 . tail)
        (values
         #'(let ()
             (rhombus-body-at tag form ...))
         #'tail)]))))
