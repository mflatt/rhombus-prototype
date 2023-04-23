#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (submod "dot-macro.rkt" for-compose))

(provide (for-syntax compose-dot-providers
                     wrap-class-dot-provider-transformer))

(define-for-syntax (compose-dot-providers . dps)
  (let loop ([dps dps])
    (cond
      [(null? dps) (lambda (form1 dot field-id tail more-static? success failure)
                     (failure))]
      [else
       (define (convert v)
         (if (procedure? v)
             v
             (syntax-local-value v)))
       (let ([main (convert (car dps))]
             [next (loop (cdr dps))])
         (lambda (form1 dot field-id tail more-static? success failure)
           (main form1 dot field-id tail more-static?
                 success
                 (lambda ()
                   (next form1 dot field-id tail more-static? success failure)))))])))

(define-for-syntax (wrap-class-dot-provider-transformer procs)
  (apply
   compose-dot-providers
   (map
    (lambda (proc)
      (wrap-dot-provider-transformer
       (lambda (packed-form dot static? packed-tail)
         (syntax-parse packed-form
           [(_ (_ lhs _ name . _))
            (proc packed-form dot #'name static? packed-tail)]))))
    procs)))
