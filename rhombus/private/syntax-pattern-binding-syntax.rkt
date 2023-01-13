#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt"
                     "realm.rkt"
                     (submod "class-meta.rkt" for-static-info))
         "syntax-pattern-binding.rkt"
         "name-root.rkt"
         "syntax.rkt")

(provide syntax_pattern_binding)

(define-simple-name-root syntax_pattern_binding
  macro
  only)

(define-name-root only
  #:fields
  ([macro macro-only]))

(define-operator-definition-transformer+only macro macro-only
  'macro
  rhombus/syntax_pattern_binding
  #'make-syntax-pattern-binding-prefix-operator
  #'make-syntax-pattern-binding-infix-operator
  #'syntax-pattern-binding-prefix+infix-operator)

(define-for-syntax (make-syntax-pattern-binding-prefix-operator name prec protocol proc)
  (syntax-pattern-binding-prefix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 stx)
         (finish (lambda ()
                   (syntax-parse stx
                     [(head . tail) (proc form1 (pack-tail #'tail) #'head)]))
                 proc))
       (lambda (stx)
         (finish (lambda ()
                   (syntax-parse stx
                     [(head . tail) (proc (pack-tail #'tail) #'head)]))
                 proc)))))

(define-for-syntax (make-syntax-pattern-binding-infix-operator name prec protocol proc assc)
  (syntax-pattern-binding-prefix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 form2 stx)
         (finish (lambda ()
                   (syntax-parse stx
                     [(head . tail) (proc form1 form2 (pack-tail #'tail) #'head)]))
                 proc))
       (lambda (form1 stx)
         (finish (lambda ()
                   (syntax-parse stx
                     [(head . tail) (proc form1 (pack-tail #'tail) #'head)]))
                 proc)))
   assc))

(define-for-syntax (finish thunk proc)
  (define-values (binds tail)
    (call-with-values
     thunk
     (case-lambda
       [(binds tail) (values binds (unpack-tail tail proc #f))]
       [(binds) (values binds #'())])))
  (unless (syntax? binds)
    (raise-result-error* (proc-name proc) rhombus-realm "Syntax" binds))
  (values (syntax-parse (unpack-group binds proc binds)
            [esc::syntax-pattern-binding #'esc.parsed])
          tail))
