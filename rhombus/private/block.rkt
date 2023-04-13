#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "expression.rkt"
         "parse.rkt")

(provide (rename-out [rhombus-block block]))

(define-syntax rhombus-block
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts block group)
       [(form-id ((~and tag block) form ...)
                 . tail)
        (values
         #'(let ()
             (rhombus-body-at tag form ...))
         #'tail)]))))