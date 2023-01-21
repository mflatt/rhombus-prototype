#lang racket/base
(require (for-syntax racket/base
                     "introducer.rkt"))

(provide (for-syntax in-name-root-space))

(define-for-syntax in-name-root-space
  (make-interned-syntax-introducer/add 'rhombus/namespace))
