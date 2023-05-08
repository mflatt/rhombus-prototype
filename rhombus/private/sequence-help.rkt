#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "parse.rkt"
                     "pack.rkt")
         "definition.rkt"
         "parsed.rkt"
         "parse.rkt"
         "parens.rkt")

(provide sequence_macro
         (for-syntax make_sequence_constructor))

(define-syntax sequence_macro
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        [(_ id (tag::block rhs-g))
         (list
          #'(define-sequence-syntax id
              (lambda () (error "shouldn't get here"))
              (rhombus-expression rhs-g)))]))))

(define-for-syntax (make_sequence_constructor proc)
  (lambda (stx)
    (syntax-parse stx
      [[(lhs:identifier ...) (_ expr)]
       (define s (proc #`(group lhs ... (block (group #,(parsed #'expr))))))
       (syntax-parse s
         #:datum-literals (group)
         [(_::parens
           (group (_::parens
                   (group outer-id:identifier ... (outer-tag::block outer-rhs ...))
                   ...))
           outer-check-g
           (group (_::parens
                   (group loop-id:identifier (loop-tag::block loop-rhs ...))
                   ...))
           pos-guard-g
           (group (_::parens
                   (group inner-id:identifier ... (inner-tag::block inner-rhs ...))
                   ...))
           pre-guard-g
           post-guard-g
           (group (_::parens
                   recur-g
                   ...)))
          #'[(lhs ...)
             (:do-in
              ([(outer-id ...) (rhombus-body-at outer-tag outer-rhs ...)] ...)
              (rhombus-expression outer-check-g)
              ([loop-id (rhombus-body-at loop-tag loop-rhs ...)] ...)
              (rhombus-expression pos-guard-g)
              ([(inner-id ...) (rhombus-body-at inner-tag inner-rhs ...)] ...)
              (rhombus-expression pre-guard-g)
              (rhombus-expression post-guard-g)
              ((rhombus-expression recur-g) ...))]])])))
