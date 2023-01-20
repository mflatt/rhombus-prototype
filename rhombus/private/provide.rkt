#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/provide-transform))

;; Use `at-spaces` to provide bindings that are defined in the default
;; space so that they're provided at specific spaces (and not at the
;; default space)

(provide for-spaces
         at-spaces)

(define-syntax for-spaces
  (make-provide-pre-transformer
   (lambda (stx space+phases)
     (syntax-parse stx
       [(_ (space ...) out ...)
        #`(combine-out (for-space space out ...)
                       ...)]))))

(define-syntax at-spaces
  (make-provide-pre-transformer
   (lambda (stx space+phases)
     (syntax-parse stx
       [(_ (space ...) id ...)
        (define ids (syntax->list #'(id ...)))
        (for ([space (in-list (syntax->list #'(space ...)))])
          (define intro (make-interned-syntax-introducer (syntax-e space)))
          (for ([id (in-list ids)])
            (syntax-parse id
              [_:identifier
               (syntax-local-lift-module-end-declaration
                #`(define-syntax #,(intro id) (make-rename-transformer (quote-syntax #,id))))]
              [(rename [int-id ext-id] ...)
               (for ([int-id (in-list (syntax->list #'(int-id ...)))])
                 (syntax-local-lift-module-end-declaration
                  #`(define-syntax #,(intro int-id) (make-rename-transformer (quote-syntax #,int-id)))))])))
        #`(combine-out
           (for-space space id ...)
           ...)]))))
