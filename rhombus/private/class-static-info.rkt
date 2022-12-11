#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     enforest/hier-name-parse
                     "class-parse.rkt")
         (submod "dot.rkt" for-dot-provider)
         "call-result-key.rkt"
         "static-info.rkt")

(provide (for-syntax build-class-static-infos))

(module+ for-top-level
  (provide define-static-info-syntax
           define-static-info-syntax/maybe*))

(define-for-syntax (build-class-static-infos exposed-internal-id
                                             super
                                             names)
  (with-syntax ([(name constructor-name name-instance
                       internal-name-instance make-internal-name
                       [name-field ...]
                       [field-static-infos-expr ...])
                 names])
    (append
     (list
      #'(define-static-info-syntax constructor-name (#%call-result ((#%dot-provider name-instance)))))
     (if exposed-internal-id
         (list
          #`(define-static-info-syntax make-internal-name
              #,(let ([info #'(#%call-result ((#%dot-provider internal-name-instance)))])
                  (if super
                      ;; internal constructor is curried
                      #`(#%call-result (#,info))
                      info))))
         '())
     (list
      #'(begin
          (define-static-info-syntax/maybe* name-field (#%call-result #,field-static-infos-expr))
          ...)))))

(define-syntax (define-static-info-syntax/maybe* stx)
  (syntax-parse stx
    #:literals (unsyntax quote-syntax)
    [(_ id (_ #,(quote-syntax ()))) #'(begin)]
    [(_ id rhs) #'(define-static-info-syntax id rhs)]))
