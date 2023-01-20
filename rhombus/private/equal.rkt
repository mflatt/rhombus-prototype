#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse)
         "provide.rkt"
         "expression.rkt"
         "binding.rkt")

(provide (for-spaces (rhombus/expr
                      rhombus/bind)
                     (rename-out [rhombus= =])))

(module+ for-parse
  (provide (for-syntax :equal
                       :not-equal)))

(define-expression-syntax rhombus=
  (expression-infix-operator
   (in-expression-space #'rhombus=)
   '((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       #:datum-literals (op)
       [((op o) . _)
        (raise-syntax-error #f
                            "not an expression operator"
                            #'o)]))
   'none))

(define-binding-syntax rhombus=
  (binding-infix-operator
   (in-binding-space #'rhombus=)
   '((default . weaker))
   'macro
   (lambda (form tail)
     (syntax-parse tail
       #:datum-literals (op)
       [((op o) . _)
        (raise-syntax-error #f
                            "not a binding operator"
                            #'o)]))
   'none))

(begin-for-syntax
  (define-syntax-class :equal
    #:attributes ()
    (pattern op::name
             #:when (free-identifier=? (in-expression-space #'op.name)
                                       (expr-quote rhombus=))))
  (define-syntax-class :not-equal
    #:attributes ()
    (pattern (~not _::equal))))
