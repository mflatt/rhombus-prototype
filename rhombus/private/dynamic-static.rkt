#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "definition.rkt"
         "expression.rkt"
         "repetition.rkt"
         (submod "dot.rkt" for-dynamic-static)
         (submod "implicit.rkt" for-dynamic-static))

(provide (for-space rhombus/expr
                    dynamic
                    use_dynamic
                    use_static))
         
(define-expression dynamic (lambda (v) v))

(begin-for-syntax
  (define-values (use_dynamic use_static)
    (let ([mk (lambda (more-static?)
                (definition-transformer
                  (lambda (stx)
                    (syntax-parse stx
                      [(form-id)
                       #`(#,@(build-definitions #'form-id '|.| (if more-static? #'static-|.| #'|.|))
                          #,@(build-definitions #'form-id '#%ref (if more-static? #'static-#%ref #'#%ref))
                          #,@(build-definitions #'form-id '#%call (if more-static? #'static-#%call #'#%call)))]))))])
      (values (mk #f)
              (mk #t)))))

(define-definition-syntax use_dynamic use_dynamic)
(define-definition-syntax use_static use_static)

(define-for-syntax (build-definitions ctx sym id)
  (define sym-id (datum->syntax ctx sym))
  #`((define-syntax #,(in-expression-space sym-id)
       (make-rename-transformer (quote-syntax #,(in-expression-space id))))
     (define-syntax #,(in-repetition-space sym-id)
       (make-rename-transformer (quote-syntax #,(in-repetition-space id))))))
