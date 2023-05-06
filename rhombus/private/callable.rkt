#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt"
                     (only-in "class-parse.rkt"
                              class-desc-flags
                              mindex-index))
         "provide.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax))

(provide (for-spaces (rhombus/class)
                     Callable))

(module+ for-class
  (provide (for-syntax callable-method-status
                       callable-method-as-property)))

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
                  (hasheq 'call 0)
                  #hasheq()
                  #t
                  '()
                  #f
                  '(call)))

(define-class-desc-syntax Callable
  callable-interface-desc)

(define-for-syntax (callable-method-status super interfaces method-mindex method-vtable method-private)
  (define call-is-callable?
    (or (and super (memq 'call (class-desc-flags super)))
        (for/or ([intf (in-list interfaces)])
          (memq 'call (interface-desc-flags intf)))))
  (values (and call-is-callable?
               (or (hash-ref method-private 'call #f)
                   (let ([m (hash-ref method-mindex 'call #f)])
                     (and m
                          (not (eq? '#:abstract (vector-ref method-vtable (mindex-index m))))))))
          (and call-is-callable?
               (not (hash-ref method-private 'call #f)))))

(define-for-syntax (callable-method-as-property callable?
                                                method-mindex method-vtable method-private)
  (cond
    [callable?
     (cond
       [(hash-ref method-private 'call #f)
        => (lambda (call-id)
             (list #`(cons prop:procedure #,call-id)))]
       [else
        (define midx (hash-ref method-mindex 'call))
        (list #`(cons prop:procedure
                      #,(vector-ref method-vtable (mindex-index midx))))])]
    [else null]))
