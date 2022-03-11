#lang racket/base
(require syntax/parse
         "pack.rkt")

(provide unpack-static-infos
         pack-static-infos)

(define (unpack-static-infos v)
  (syntax-parse v
    [((key val) ...)
     #'(parens (parens (group key) (group val)) ...)]))

(define (pack-static-infos v who)
  (syntax-parse v
    #:datum-literals (parens group)
    [(parens (parens (group key) (group val)) ...)
     #'((key val) ...)]))

