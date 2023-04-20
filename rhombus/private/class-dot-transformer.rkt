#lang racket/base
(require (for-syntax racket/base)
         (submod "dot-macro.rkt" for-compose))

(provide (for-syntax compose-dot-providers))

(define-for-syntax (compose-dot-providers main default)
  (let ([main (wrap-dot-provider-transformer main)])
    (lambda (form1 dot field-id tail more-static? success failure)
      (main form1 dot field-id tail more-static?
            success
            (lambda ()
              (default form1 dot field-id tail more-static? success failure))))))
