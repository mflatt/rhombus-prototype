#lang racket/base

(provide prop:comparable
         comparable-ref)

(define-values (prop:comparable comparable? comparable-ref)
  (make-struct-type-property 'comparable))
