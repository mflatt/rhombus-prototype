#lang racket/base
(require (for-syntax racket/base)
         "expression.rkt"
         (only-in (submod "implicit.rkt" for-dynamic-static)
                  static-#%call))

(provide (for-syntax is-static-call-context?))

(define-for-syntax (is-static-call-context? tag)
  (free-identifier=? (in-expression-space (datum->syntax tag '#%call))
                     (expr-quote static-#%call)))
