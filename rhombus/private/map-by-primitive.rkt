#lang racket
(require (for-syntax racket/base)
         "map-by.rkt"
         (submod "map.rkt" for-map-by)
         (submod "set.rkt" for-map-by))

(provide (for-space rhombus/map_by
                    ==
                    ===
                    is_now
                    is_number_or_object))

(define-map-by-syntax ==
  (map-by '== #'equal-always-hash?
          #'Map-build #'Map-pair-build #'list->map
          #'mutable-equal-always-hash? #'MutableMap-build
          #'#hashalw()
          #'immutable-equal-always-set?
          #'Set-build #'Set-build* #'list->set
          #'mutable-equal-always-set? #'MutableSet-build))

(define-map-by-syntax ===
  (map-by '=== #'object-hash?
          #'ObjectMap-build #'ObjectMap-pair-build #'list->object-map
          #'mutable-object-hash? #'MutableObjectMap-build
          #'#hasheq()
          #'immutable-object-set?
          #'ObjectSet-build #'ObjectSet-build* #'list->object-set
          #'mutable-object-set? #'MutableObjectSet-build))

(define-map-by-syntax is_now
  (map-by 'is_now #'now-hash?
          #'NowMap-build #'NowMap-pair-build #'list->now-map
          #'mutable-now-hash? #'MutableNowMap-build 
          #'#hash()
          #'immutable-now-set?
          #'NowSet-build #'NowSet-build* #'list->now-set
          #'mutable-now-set? #'MutableNowSet-build ))

(define-map-by-syntax is_number_or_object
  (map-by 'is_number_or_object #'number-or-object-hash?
          #'NumberOrObjectMap-build #'NumberOrObjectMap-pair-build #'list->number-or-object-map
          #'mutable-number-or-object-hash? #'MutableNumberOrObjectMap-build 
          #'#hasheqv()
          #'immutable-number-or-object-set?
          #'NumberOrObjectSet-build #'NumberOrObjectSet-build* #'list->number-or-object-set
          #'mutable-number-or-object-set? #'MutableNumberOrObjectSet-build ))
