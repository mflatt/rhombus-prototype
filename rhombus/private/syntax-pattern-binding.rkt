#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest
                     enforest/operator
                     enforest/transformer
                     enforest/property
                     enforest/syntax-local
                     "introducer.rkt"
                     "name-path-op.rkt")
         "name-root-ref.rkt")

(begin-for-syntax
  (provide (property-out syntax-pattern-binding-prefix-operator)
           (property-out syntax-pattern-binding-infix-operator)

           syntax-pattern-binding-transformer
                     
           :syntax-pattern-binding

           in-syntax-pattern-binding-space

           current-syntax-pattern-binding-kind

           syntax-pattern-binding-prefix+infix-operator

           syntax-pattern-binding-id?))

(provide define-syntax-pattern-binding-syntax)

(begin-for-syntax
  (property syntax-pattern-binding-prefix-operator prefix-operator)
  (property syntax-pattern-binding-infix-operator infix-operator)

  (define (syntax-pattern-binding-transformer name proc)
    (syntax-pattern-binding-prefix-operator name '((default . stronger)) 'macro proc))

  (define (make-identifier-syntax-pattern-binding id)
    id)

  (define in-syntax-pattern-binding-space (make-interned-syntax-introducer/add 'rhombus/syntax_pattern_binding))

  (define current-syntax-pattern-binding-kind (make-parameter 'term))

  (define-enforest
    #:syntax-class :syntax-pattern-binding
    #:desc "syntax pattern binding"
    #:operator-desc "syntax pattern binding operator"
    #:in-space in-syntax-pattern-binding-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:name-root-ref-root name-root-ref-root
    #:prefix-operator-ref syntax-pattern-binding-prefix-operator-ref
    #:infix-operator-ref syntax-pattern-binding-infix-operator-ref
    #:make-identifier-form make-identifier-syntax-pattern-binding)

  (struct syntax-pattern-binding-prefix+infix-operator (prefix infix)
    #:property prop:syntax-pattern-binding-prefix-operator (lambda (self) (syntax-pattern-binding-prefix+infix-operator-prefix self))
    #:property prop:syntax-pattern-binding-infix-operator (lambda (self) (syntax-pattern-binding-prefix+infix-operator-infix self)))

  (define (syntax-pattern-binding-id? id)
    (syntax-local-value* (in-syntax-pattern-binding-space id)
                         (lambda (v)
                           (or (syntax-pattern-binding-prefix-operator-ref v)
                               (syntax-pattern-binding-infix-operator-ref v))))))

(define-syntax (define-syntax-pattern-binding-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-syntax-pattern-binding-space #'name) rhs))]))
