#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt")
         "provide.rkt"
         "annotation.rkt"
         "printer-property.rkt"
         "name-root.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "realm.rkt"
         "class-dot.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax)
         "define-arity.rkt"
         "print-desc.rkt")

(provide (for-spaces (rhombus/class
                      rhombus/namespace)
                     Printable))

(define-values (prop:Printable Printable? Printable-ref)
  (make-struct-type-property 'Printable
                             #f
                             (list (cons prop:printer
                                         (lambda (v) bounce-to-printer-interface)))))

(define (bounce-to-printer-interface v mode recur)
  (define pd ((vector-ref (Printable-ref v) 0) v mode recur))
  (print-description-unwrap 'Printable.print pd #t))

(define-class-desc-syntax Printable
  (interface-desc #'Printable
                  #'Printable
                  #'()
                  #'prop:Printable
                  #'prop:Printable
                  #'Printable-ref
                  '#(#&print)
                  #'#(#:abstract)
                  (hasheq 'print 0)
                  #hasheq()
                  #t
                  '()
                  #f
                  #'()
                  '()))


(define-name-root Printable
  #:fields   
  (sequence
    [sequence Printable.sequence]
    [newline Printable.newline]
    [nest Printable.nest]
    [align Printable.align]
    [or Printable.or]
    Description))

(define (get-printer who v)
  (define vt (Printable-ref v #f))
  (unless vt
    (raise-argument-error* who rhombus-realm "Printable" v))
  vt)

(define-annotation-syntax Description
  (identifier-annotation #'Printable.Description? #'()))

(define (print-description-unwrap who pd [result? #f])
  (cond
    [(Printable.Description? pd)
     (Printable.Description-doc pd)]
    [(string? pd) pd]
    [(bytes? pd) pd]
    [else
     (if result?
         (raise-result-error* who rhombus-realm "Printable.Description || String || Bytes" pd)
         (raise-argument-error* who rhombus-realm "Printable.Description || String || Bytes" pd))]))

(define/arity (Printable.sequence . pds)
  (Printable.Description
   `(seq ,@(for/list ([pd (in-list pds)])
             (print-description-unwrap 'Printable.sequence pd)))))
       
(define/arity (Printable.newline)
  (Printable.Description 'nl))
       
(define/arity (Printable.nest n pd)
  (unless (exact-integer? n)
    (raise-argument-error* rhombus-realm "Integer" pd))
  (Printable.Description
   `(next ,n ,(print-description-unwrap 'Printable.nest pd))))

(define/arity (Printable.align pd)
  (Printable.Description
   `(align ,(print-description-unwrap 'Printable.align pd))))

(define/arity (Printable.or pd1 pd2)
  (Printable.Description
   `(or ,(print-description-unwrap 'Printable.or pd1)
        ,(print-description-unwrap 'Printable.or pd2))))
