#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "parse.rkt")

(provide (for-syntax wrap-expression))

(define-for-syntax (wrap-expression form)
  (syntax-parse form
    #:datum-literals (parsed group multi)
    [(multi (group (parsed e))) #'e] ; shortcut
    [(group . _) #`(rhombus-expression ,form)]
    [(multi g) #`(rhombus-expression g)]
    [(multi) (raise-syntax-error #f "invalid empty expression" form)]
    [_ (raise-syntax-error #f "invalid expression representation" form)]))
