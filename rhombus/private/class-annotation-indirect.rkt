#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         (only-in "annotation.rkt" -:)
         (submod "annotation.rkt" for-class))

(provide (for-syntax annotation-sequence->indirect-forms
                     build-class-annotation-indirections))

(module+ for-top-level
  (provide define-class-annotation-indirection))

(define-for-syntax (annotation-sequence->indirect-forms name ctc-seq combine)
  (if (and ctc-seq (syntax-e ctc-seq))
      (let* ([si-expr #`(lookup (quote-syntax
                                 #,(datum->syntax #'here (string->symbol (format "~a-static-infos" (syntax-e name))))))])
        (syntax-parse ctc-seq
          #:datum-literals (op)
          #:literals (-:)
          [((op -:) . _)
           (combine #'#f #'#f si-expr)]
          [else
           (combine (datum->syntax #'here (string->symbol (format "ok-~a?" (syntax-e name))))
                    (datum->syntax #'here (string->symbol (format "~a-ann-str" (syntax-e name))))
                   si-expr)]))
      (combine #f #f (quote-syntax (quote-syntax ())))))

(define-for-syntax (build-class-annotation-indirections ctc-seqs-stx
                                                        predicates-stx
                                                        annotation-strs-stx
                                                        static-infos-expr-stx)
  (for/list ([ctc-seq (in-list (syntax->list ctc-seqs-stx))]
             [predicate (in-list (syntax->list predicates-stx))]
             [annotation-str (in-list (syntax->list annotation-strs-stx))]
             [static-infos-expr (in-list (syntax->list static-infos-expr-stx))]
             #:when (syntax-e ctc-seq))
    #`(define-class-annotation-indirection #,ctc-seq #,predicate #,annotation-str #,static-infos-expr)))

(define-syntax (define-class-annotation-indirection stx)
  (syntax-parse stx
    [(_ (c::inline-annotation) predicate annotation-str (_ (_ static-infos-id)))
     (define si #`(define-syntax static-infos-id (quote-syntax c.static-infos)))
     (if (syntax-e #'c.predicate)
         #`(begin
             (define (predicate v) (c.predicate v))
             (define annotation-str 'c.annotation-str)
             #,si)
         si)]))

(define-for-syntax (lookup id)
  (syntax-local-value id))
