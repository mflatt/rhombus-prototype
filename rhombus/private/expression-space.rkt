#lang racket/base
(require "introducer.rkt")

(provide in-expression-space)

(define in-expression-space (make-interned-syntax-introducer/add 'rhombus/expr))
