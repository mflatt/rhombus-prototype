#lang racket/base
(require "../private/bounce.rkt"
         (for-syntax
          racket/base
          (only-in "../private/parse.rkt" rhombus-definition)
          (only-in "../private/forwarding-sequence.rkt" rhombus-module-forwarding-sequence)
          (only-in "../private/dynamic-static.rkt" use_static)))

(begin-for-syntax
  (rhombus-module-forwarding-sequence
   (rhombus-definition (group use_static)))) ;; defines `#%dynamism`

(bounce #:except (#%dynamism)
        "../meta.rkt")
(provide (for-syntax
          #%dynamism))
