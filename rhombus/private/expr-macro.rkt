#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer-result
                     enforest/syntax-local
                     "srcloc.rkt"
                     "pack.rkt"
                     "pack-s-exp.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     (for-syntax racket/base)
                     "tail-returner.rkt"
                     "realm.rkt")
         (only-in "space.rkt" space-syntax)
         "space-provide.rkt"
         "name-root.rkt"
         "macro-macro.rkt"
         "expression.rkt"
         "parse.rkt"
         "wrap-expression.rkt"
         (for-syntax "name-root.rkt")
         "definition.rkt"
         "declaration.rkt"
         "nestable-declaration.rkt"
         "parens.rkt"
         "dotted-sequence-parse.rkt")

(provide (for-syntax (for-space rhombus/namespace
                                expr_meta)))

(module+ for-define
  (provide (for-syntax make-expression-infix-operator
                       make-expression-prefix-operator)))

(define+provide-space expr #f
  #:fields
  (macro
   merge))

(begin-for-syntax
  (define-name-root expr_meta
    #:fields
    (space
     Parsed
     AfterPrefixParsed
     AfterInfixParsed
     parse_more
     pack_s_exp
     pack_expr)))

(define-for-syntax space
  (space-syntax #f))

(define-operator-definition-transformer macro
  'macro
  #f
  #'make-expression-prefix-operator
  #'make-expression-infix-operator
  #'expression-prefix+infix-operator)

(begin-for-syntax
  (define-operator-syntax-classes
    Parsed :expression #:rhombus/expr
    AfterPrefixParsed :prefix-op+expression+tail
    AfterInfixParsed :infix-op+expression+tail))

(define-for-syntax (parsed-argument form)
  ;; use `rhombus-local-expand` to expose static information
  (define loc (maybe-respan form))
  (relocate loc #`(parsed #:rhombus/expr #,(relocate loc (rhombus-local-expand form)))))

(define-for-syntax (make-expression-infix-operator name prec protocol proc assc)
  (expression-infix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 form2 stx)
         (wrap-expression (check-expression-result
                           (proc (parsed-argument form1) (parsed-argument form2) stx)
                           proc)
                          #:srcloc (span-srcloc form1 form2)))
       (lambda (form1 tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (parsed-argument form1) (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (wrap-expression (check-expression-result form proc))
                                   (unpack-tail new-tail proc #f)
                                   proc)))
   assc))

(define-for-syntax (make-expression-prefix-operator name prec protocol proc)
  (expression-prefix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form stx)
         (wrap-expression (check-expression-result
                           (proc (parsed-argument form) stx)
                           proc)
                          #:srcloc (span-srcloc stx form)))
       (lambda (tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (wrap-expression (check-expression-result form proc))
                                   (unpack-tail new-tail proc #f)
                                   proc)))))

(define-for-syntax (pack_s_exp orig-s)
  #`(parsed
     #:rhombus/expr
     #,(pack-s-exp 'expr.pack_s_exp orig-s)))

(define-for-syntax (pack_expr s)
  (unless (syntax? s)
    (raise-argument-error* 'expr.pack_expr rhombus-realm "Syntax" s))
  #`(parsed #:rhombus/expr (rhombus-expression #,(unpack-group s 'expr.pack_expr #f))))

(define-for-syntax (parse_more s)
  (syntax-parse (unpack-group s 'expr_meta.parse_more #f)
    [e::expression #`(parsed #:rhombus/expr #,(rhombus-local-expand #'e.parsed))]))

;; ----------------------------------------

(define-syntax merge
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (group)
        [(form-id seq::dotted-identifier-sequence
                  (_::block (group id:identifier ...)
                            ...))
         #:with name::dotted-identifier #'seq
         (build-syntax-definitions/maybe-extension
          (list #f) #'name.name #'name.extends
          #`(merge-expr-space-values 'form-id (list (quote-syntax id) ... ...)))]))))

(define-for-syntax (merge-expr-space-values who ids)
  (define ht
    (for/fold ([ht #hasheq()]) ([id (in-list ids)])
      (define-values (new-ht did?)
        (for/fold ([ht ht] [did? #f]) ([ref (list expression-prefix-operator-ref
                                                  expression-infix-operator-ref
                                                  definition-transformer-ref
                                                  definition-sequence-transformer-ref
                                                  declaration-transformer-ref
                                                  nestable-declaration-transformer-ref)]
                                       [prop (list prop:expression-prefix-operator
                                                   prop:expression-infix-operator
                                                   prop:definition-transformer
                                                   prop:definition-sequence-transformer
                                                   prop:declaration-transformer
                                                   prop:nestable-declaration-transformer)]
                                       [key (list "prefix expression operator"
                                                  "infix expression operator"
                                                  "definition form"
                                                  "definition-sequence form"
                                                  "declaration form"
                                                  "nestedable declaration form")])
          (define v (syntax-local-value* id ref))
          (cond
            [(not v) (values ht did?)]
            [(hash-ref ht key #f)
             (raise-syntax-error who
                                 (format "duplicate ~a" key)
                                 id)]
            [else
             (values (hash-set ht key (cons prop v))
                     #t)])))
      (unless did?
        (raise-syntax-error who "not defined as a form that can be merged" id))
      new-ht))
  (define-values (struct: mk ? ref set)
    (make-struct-type 'merged #f 0 0 #f
                      (for/list ([p (in-hash-values ht)])
                        (cons (car p) (lambda (self) (cdr p))))))
  (mk))
