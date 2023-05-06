#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt"
                     (only-in "class-parse.rkt" mindex-index))
         "provide.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax))

(provide (for-spaces (rhombus/class)
                     Callable))

(module+ for-class
  (provide (for-syntax callable-method-as-property)))

(define-values (prop:Callable Callable? Callable-ref)
  (make-struct-type-property 'Callable
                             #f
                             ;; prop:procedure should be in this list, but
                             ;; we have to handle it more directly
                             ;; to get the arity right; direct handling also
                             ;; lets us supply the method-as-function directly
                             ;; as the property value; direct handling is
                             ;; implemented by `callable-method-as-property`
                             ;; below
                             (list)))

(define-for-syntax callable-interface-desc
  (interface-desc #'Callable
                  #'Callable
                  #'()
                  #'prop:Callable
                  #'prop:Callable
                  #'Callable-ref
                  '#(#&call)
                  #'#(#:abstract)
                  (hasheq 'call 1) ; 1 must be replaced by overriding
                  #hasheq()
                  #t
                  '()
                  #f))

(define-class-desc-syntax Callable
  callable-interface-desc)

(define-for-syntax (callable-method-as-property all-interfaces-transitively
                                                method-mindex method-names method-vtable method-private)
  (cond
    [(memq callable-interface-desc all-interfaces-transitively)
     (cond
       [(hash-ref method-private 'call #f)
        => (lambda (call-id)
             (list #`(cons prop:procedure #,call-id)))]
       [else
        (define midx (hash-ref method-mindex 'call))
        (list #`(cons prop:procedure
                      #,(vector-ref method-vtable (mindex-index midx))))])]
    [else null]))
