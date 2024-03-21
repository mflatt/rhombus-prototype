#lang racket
(require (for-syntax racket/base
                     syntax/parse/pre
                     "introducer.rkt"))

(provide define-map-by-syntax)

(begin-for-syntax
  (provide (struct-out map-by)
           in-map-by-space
           map-by-ref)
  
  (define in-map-by-space (make-interned-syntax-introducer/add 'rhombus/map_by))

  (struct map-by (name-sym map?-id
                           map-build-id map-pair-build-id list->map-id
                           mutable-map?-id mutable-map-build-id
                           empty-stx
                           set?-id
                           set-build-id set-build*-id list->set-id
                           mutable-set?-id mutable-set-build-id))

  (define (map-by-ref v) (and (map-by? v) v)))

(define-syntax (define-map-by-syntax stx)
  (syntax-parse stx
    [(_ id rhs)
     #`(define-syntax #,(in-map-by-space #'id) rhs)]))
