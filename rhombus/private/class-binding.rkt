#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     (only-in enforest/operator operator-proc)
                     "srcloc.rkt"
                     "class-parse.rkt")
         "binding.rkt"
         "binding-syntax.rkt"
         (submod "binding-syntax.rkt" for-class)
         "composite.rkt"
         "parens.rkt"
         (submod "boolean-pattern.rkt" for-class)
         (for-syntax "class-transformer.rkt")
         (submod "dot.rkt" for-dot-provider))

(provide (for-syntax build-class-binding-form))

(define-for-syntax (build-class-binding-form super binding-rhs
                                             exposed-internal-id intro
                                             names)
  (with-syntax ([(name name-instance name?
                       constructor-name-fields constructor-public-name-fields super-name-fields
                       constructor-field-static-infos-exprs constructor-public-field-static-infos-exprs super-field-static-infoss
                       field-keywords public-field-keywords super-field-keywords)
                 names])
    (define (make-binding-transformer no-super? name-fields static-infos-exprs keywords)
      (with-syntax ([(constructor-name-field ...) name-fields]
                    [(constructor-field-static-info-exprs ...) static-infos-exprs]
                    [(field-keyword ...) keywords]
                    [(super-name-field ...) (if no-super? '() #'super-name-fields)]
                    [(super-field-static-infos ...) (if no-super? '() #'super-field-static-infoss)]
                    [(super-field-keyword ...) (if no-super? '() #'super-field-keywords)])
        #`(binding-transformer
           (quote-syntax name)
           (make-composite-binding-transformer #,(symbol->string (syntax-e #'name))
                                               (quote-syntax name?)
                                               #:static-infos (quote-syntax ((#%dot-provider name-instance)))
                                               (list (quote-syntax super-name-field) ...
                                                     (quote-syntax constructor-name-field) ...)
                                               #:keywords '(super-field-keyword ... field-keyword ...)
                                               (list (quote-syntax super-field-static-infos) ...
                                                     constructor-field-static-info-exprs ...)
                                               #:accessor->info? #t))))
    (append
     (if exposed-internal-id
         (list
          #`(define-binding-syntax #,exposed-internal-id
              #,(make-binding-transformer #t
                                          #'constructor-name-fields
                                          #'constructor-field-static-infos-exprs
                                          #'field-keywords)))
         null)
     (cond
       [binding-rhs
        (list
         #`(define-binding-syntax name
             (wrap-class-transformer name #,(intro binding-rhs) make-binding-prefix-operator)))]
       [else
        (list
         #`(define-binding-syntax name
             #,(make-binding-transformer #f
                                         #'constructor-public-name-fields
                                         #'constructor-public-field-static-infos-exprs
                                         #'public-field-keywords)))]))))
