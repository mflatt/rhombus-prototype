#lang racket/base
(require (for-syntax racket/base
                     rhombus/private/interface-parse)
         (only-in rhombus/private/class-desc define-class-desc-syntax)
         file/convertible)

(provide (for-space rhombus/class
                    Convertible))

(define-values (prop:Convertible Convertible? Convertible-ref)
  (make-struct-type-property
   'Convertible
   #false
   (list (cons prop:convertible
               (lambda (v)
                 (lambda (self req default)
                   ((vector-ref (Convertible-ref self) 0) self req default)))))))

(define-class-desc-syntax Convertible
  (interface-desc #'Convertible
                  #'Convertible
                  #'()
                  #'prop:Convertible
                  #'prop:Convertible
                  #'Convertible-ref
                  (vector-immutable (box-immutable 'handle_convert))
                  #'#(#:abstract)
                  (hasheq 'handle_convert 0)
                  #hasheq()
                  #t
                  '()
                  #f
                  #'()
                  #f
                  '()))
