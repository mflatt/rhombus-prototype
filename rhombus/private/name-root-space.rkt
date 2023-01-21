#lang racket/base
(require (for-syntax racket/base
                     "introducer.rkt"))

(provide (for-syntax in-name-root-space
                     out-of-name-root-space))

(define-for-syntax in-name-root-space
  (make-interned-syntax-introducer/add 'rhombus/namespace))

(define-for-syntax out-of-name-root-space
  (let ([intro (make-interned-syntax-introducer 'rhombus/namespace)])
    (lambda (id) (intro id 'remove))))

