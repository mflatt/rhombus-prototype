#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "parse.rkt")

(provide (for-syntax wrap-expression))

(define-for-syntax (wrap-expression form)
  (syntax-parse form
    #:datum-literals (parsed group parens)
    [(parsed e) #'e]
    [(group e ...) #`(rhombus-expression ,form)]
    [(parens g) #`(rhombus-expression g)]
    [(parens) (raise-syntax-error #f "invalid empty expression" form)]
    [_ (raise-syntax-error #f "invalid expression representation" form)]))
