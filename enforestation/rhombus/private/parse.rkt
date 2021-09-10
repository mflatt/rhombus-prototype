#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     enforest
                     enforest/transformer
                     "srcloc.rkt"
                     "name-path-op.rkt")
         "forwarding-sequence.rkt"
         "declaration.rkt"
         "definition.rkt"
         "expression.rkt"
         "binding.rkt"
         "srcloc.rkt")

(provide rhombus-top
         rhombus-definition
         rhombus-block
         rhombus-expression

         rhombus-block-at
         rhombus-body

         (for-syntax :declaration
                     :definition
                     :expression
                     :binding

                     ;; for continuing enforestation of expressions or bindings:
                     :prefix-op+expression+tail
                     :infix-op+expression+tail
                     :prefix-op+binding+tail
                     :infix-op+binding+tail))

(begin-for-syntax
  ;; Form at the top of a module:
  (define-transform
    #:syntax-class :declaration
    #:desc "declaration"
    #:name-path-op name-path-op
    #:transformer-ref declaration-transformer-ref
    #:check-result check-declaration-result)

  ;; Form in a definition context:
  (define-transform
    #:syntax-class :definition
    #:desc "definition"
    #:name-path-op name-path-op
    #:transformer-ref definition-transformer-ref
    #:check-result check-definition-result)

  ;; Form in an expression context:
  (define-enforest
    #:syntax-class :expression
    #:prefix-more-syntax-class :prefix-op+expression+tail
    #:infix-more-syntax-class :infix-op+expression+tail
    #:desc "expression"
    #:operator-desc "expression operator"
    #:in-space in-expression-space
    #:name-path-op name-path-op
    #:prefix-operator-ref expression-prefix-operator-ref
    #:infix-operator-ref expression-infix-operator-ref
    #:check-result check-expression-result
    #:make-identifier-form make-identifier-expression)

  ;; Form in a binding context:
  (define-enforest
    #:syntax-class :binding
    #:prefix-more-syntax-class :prefix-op+binding+tail
    #:infix-more-syntax-class :infix-op+binding+tail
    #:desc "binding"
    #:operator-desc "binding operator"
    #:in-space in-binding-space
    #:name-path-op name-path-op
    #:prefix-operator-ref binding-prefix-operator-ref
    #:infix-operator-ref binding-infix-operator-ref
    #:check-result check-binding-result
    #:make-identifier-form make-identifier-binding))

;; For a module top level, interleaves expansion and enforestation:
(define-syntax (rhombus-top stx)
  (syntax-parse stx
    [(_) #'(begin)]
    [(_ form . forms)
     (define parsed
       (with-syntax-error-respan
         (syntax-local-introduce
          ;; note that we may perform hierarchical name resolution
          ;; up to three times, since resolution for `:declaration`
          ;; doesn't carry over
          (syntax-parse (syntax-local-introduce #'form)
            [e::declaration #'(begin . e.parsed)]
            [e::definition #'(begin . e.parsed)]
            [e::expression #'(#%expression e.parsed)]))))
     (syntax-parse #'forms
       [() parsed]
       [_ #`(begin #,parsed (rhombus-top . forms))])]))

;; For a definition context:
(define-syntax (rhombus-definition stx)
  (with-syntax-error-respan
    (syntax-local-introduce
     (syntax-parse (syntax-local-introduce stx)
       [(_) #'(begin)]
       [(_ ((~datum group) ((~datum parsed) defn))) #'defn]
       [(_ e::definition) #'(begin . e.parsed)]
       [(_ e::expression) #'(#%expression e.parsed)]))))

;; For an expression context, interleaves expansion and enforestation:
(define-syntax (rhombus-block stx)
  (syntax-parse stx
    [(_)
     (raise-syntax-error #f "block has no expressions" stx)]
    [(_ . tail)
     #`(let ()
         (rhombus-forwarding-sequence
          #:need-end-expr #,stx
          (rhombus-body . tail)))]))

(define-syntax (rhombus-block-at stx)
  (syntax-parse stx
    [(_ tag . tail)
     (syntax/loc #'tag (rhombus-block . tail))]))

;; For a definition context, interleaves expansion and enforestation:
(define-syntax (rhombus-body stx)
  (with-syntax-error-respan
    (syntax-parse (syntax-local-introduce stx)
      [(_) #'(begin)]
      [(_ e::definition . tail)
       (syntax-local-introduce
        #`(begin
            (begin . e.parsed)
            (rhombus-body . tail)))]
      [(_ e::expression . tail)
       (syntax-local-introduce
        #`(begin
            (#%expression e.parsed)
            (rhombus-body . tail)))])))

;; For an expression context:
(define-syntax (rhombus-expression stx)
  (with-syntax-error-respan
    (syntax-parse (syntax-local-introduce stx)
      [(_ e::expression) (syntax-local-introduce #'e.parsed)])))