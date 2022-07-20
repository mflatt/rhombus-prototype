#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "operator-parse.rkt"
                     enforest
                     enforest/property
                     enforest/syntax-local
                     enforest/operator
                     enforest/transformer
                     enforest/property
                     enforest/name-parse
                     enforest/proc-name)
         "expression.rkt"
         "static-info.rkt"
         "ref-result-key.rkt")

(provide (rename-out [rhombus... ...]))

(module+ for-repeat
  (provide
   (for-syntax make-repetition
               repetition-as-list
               repetition-as-syntax)))

(begin-for-syntax
  (property repetition expression-prefix-operator (seq-id depth element-static-infos))

  (property repetition-prefix-operator prefix-operator)
  (property repetition-infix-operator infix-operator)

  (define in-repetition-space (make-interned-syntax-introducer/add 'rhombus/repetition))

  (struct repetition-use (rep depth))

  (define (check-repetition-use-result form proc)
    (unless (repetition-use? form) (raise-result-error* (proc-name proc) rhombus-realm "Repetition_Use" form))
    form)

  (define (identifier-repetition-use id)
    (raise-syntax-error #f
                        "not bound as a repetition"
                        id))

  ;; Form in a repetition context:
  (define-enforest
    #:syntax-class :repetition-use
    #:prefix-more-syntax-class :prefix-op+repetition-use+tail
    #:infix-more-syntax-class :infix-op+repetition-use+tail
    #:desc "repetition"
    #:operator-desc "repetition operator"
    #:in-space in-repetition-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:prefix-operator-ref repetition-prefix-operator-ref
    #:infix-operator-ref repetition-infix-operator-ref
    #:check-result check-binding-result
    #:make-identifier-form identifier-repetition-use)

  (define (make-repetition name seq-id element-static-infos
                           #:depth [depth 1]
                           #:expr-handler [expr-handler (lambda (stx fail) (fail))])
    (repetition
     name '((default . stronger)) 'macro
     (lambda (stx)
       (expr-handler stx (lambda ()
                           (syntax-parse stx
                             [(self . _)
                              (raise-syntax-error #f
                                                  "cannot use repetition binding as an expression"
                                                  #'self)]))))
     seq-id
     depth
     element-static-infos)))

(define-syntax rhombus...
  (expression-transformer
   #'rhombus...
   (lambda (stx)
     (syntax-parse stx
       [(op::operator . tail)
        (raise-syntax-error #f
                            "misuse outside of a binding or constructor"
                            #'op.name)]))))

(define-for-syntax (repetition-as-list ellipses stx depth)
  (syntax-parse stx
    #:datum-literals (group)
    [(group rep-name:id)
     #:do [(define rep (syntax-local-value* #'rep-name repetition-ref))]
     #:when rep
     (unless (= depth (repetition-depth rep))
       (raise-syntax-error #f
                           "used with wrong ellipsis depth"
                           #'rep-name
                           #f
                           null
                           (format "\n  expected: ~a\n  actual: ~a"
                                   (repetition-depth rep)
                                   depth)))
     (wrap-static-info (repetition-seq-id rep)
                       #'#%ref-result
                       (repetition-element-static-infos rep))]
    [_
     (raise-syntax-error (syntax-e ellipses)
                         "not bound as a repetition"
                         stx)]))

(define-for-syntax (repetition-as-syntax ellipses stx depth)
  (repetition-as-list ellipses stx depth))
