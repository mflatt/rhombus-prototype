#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     "statically-str.rkt")
         "expression.rkt"
         "static-info.rkt"
         "parse.rkt"
         "dot-provider-key.rkt")

(provide with)

(module+ for-update
  (provide define-update-syntax
           (for-syntax update-transformer)))


(begin-for-syntax
  (define in-update-space (make-interned-syntax-introducer 'rhombus/update))

  (struct update-transformer (proc))
  (define (update-transformer-ref v)
    (and (update-transformer? v) v))
  
  (define-syntax-class :update-provider
    (pattern (~var ref-id (:static-info #'#%dot-provider))
             #:attr id #'ref-id.val)))

(define-syntax with
  (expression-infix-operator
   (quote-syntax with)
   '((default . weaker))
   'macro
   (lambda (orig-form1 tail)
     (syntax-parse tail
       [(with-id . tail)
        (let ([form1 (rhombus-local-expand orig-form1)])
          (define update-id
            (syntax-parse form1
              [dp::update-provider #'dp.id]
              [_ #f]))
          (define updater
            (and update-id
                 (syntax-local-value* (in-update-space update-id) update-transformer-ref)))
          (unless updater
            (raise-syntax-error #f
                                (string-append "no update implementation available" statically-str)
                                #'with-id
                                (unwrap-static-infos orig-form1)))
          ((update-transformer-proc updater) form1 #'with-id #'tail))]))
   'left))
  
(define-syntax (define-update-syntax stx)
  (syntax-parse stx
    [(_ id rhs)
     #`(define-syntax #,(in-update-space #'id) rhs)]))
