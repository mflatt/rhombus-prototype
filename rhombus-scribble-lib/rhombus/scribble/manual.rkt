#lang racket/base

(require scribble/rhombus/manual)
(provide (all-from-out scribble/rhombus/manual))

(module reader racket/base
  (require (submod scribble/rhombus/manual reader))
  (provide (all-from-out (submod scribble/rhombus/manual reader))))

(module configure-expand racket/base
  (require (submod scribble/rhombus/manual configure-expand))
  (provide (all-from-out (submod scribble/rhombus/manual configure-expand))))
