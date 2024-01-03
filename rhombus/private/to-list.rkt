#lang racket/base
(require "treelist.rkt"
         "realm.rkt")

(provide listable?
         to-list)

(define (listable? v)
  (or (treelist? v) (list? v)))

(define (to-list who v)
  (cond
    [(treelist? v) (treelist->list v)]
    [(list? v) v]
    [else (raise-argument-error* who rhombus-realm "Listable" v)]))
