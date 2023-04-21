#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "macro-macro.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "macro-rhs.rkt"
                     (submod "dot.rkt" for-dot-provider)
                     "entry-point.rkt"
                     "srcloc.rkt"
                     (for-syntax racket/base
                                 syntax/parse/pre
                                 "with-syntax.rkt"
                                 "srcloc.rkt"))
         "provide.rkt"
         "class-clause.rkt"
         "class-clause-tag.rkt"
         "interface-clause.rkt"
         "parens.rkt")

(provide (for-spaces (rhombus/class_clause
                      rhombus/interface_clause)
                     dot_provider))

;; see also "class-clause-primitive-macro.rkt"; this one has only
;; forms that need meta-time bindings, so we don't want a mate-time
;; including of the works in `rhombus/meta` (which would then need a
;; meta-meta rhombus)

(define-for-syntax (make-macro-clause-transformer
                    key
                    #:clause-transformer [clause-transformer class-clause-transformer])
  (clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-name (q-tag::quotes ((~and g-tag group) . pat))
                   (~and (_::block . _)
                         template-block))
        (wrap-class-clause #`(#,key (block
                                     #,(no-srcloc
                                        #`(wrap-class-dot-transformer
                                           (form-name (q-tag (g-tag dot-provider . pat))
                                                      template-block))))))]
       [(form-name (a-tag::alts
                    (b-tag::block ((~and g0-tag group)
                                   (q-tag::quotes ((~and g-tag group) . pat))
                                   (~and (_::block . _)
                                         template-block)))
                    ...))
        (wrap-class-clause #`(#,key (block
                                     #,(no-srcloc
                                        #`(wrap-class-dot-transformer
                                           (form-name (a-tag
                                                       (b-tag (g0-tag (q-tag (g-tag dot-provider . pat))
                                                                      template-block))
                                                       ...)))))))]
       [(form-name (_::block g))
        (wrap-class-clause #`(#,key (block (wrap-class-dot-transformer #:entry-point g))))]))))

(define-class-clause-syntax dot_provider
  ;; note: the generated `dot_provider` summary will have `macro-expression`, but tat
  ;; will be ignored, because a dot provider has a different shape, and we make no
  ;; attempt to support a dot provider without meta imports; this shape is handled
  ;; more directly in "class-dot.rkt"
  (make-macro-clause-transformer #'#:dot_provider))

(define-interface-clause-syntax dot_provider
  ;; same note as for class variant
  (make-macro-clause-transformer #'#:dot_provider
                                 #:clause-transformer interface-clause-transformer))

(begin-for-syntax
  (define-syntax (wrap-class-dot-transformer stx)
    (syntax-parse stx
      #:literals ()
      #:datum-literals (group named-macro)
      [(_ #:entry-point g)
       (with-syntax-parse ([(~var lam (:entry-point no-adjustments)) (respan #'g)])
         #`(let ([name (lambda (seq dot-op static? tail)
                         (lam.parsed seq (hash '#:op_stx dot-op
                                               '#:is_static static?
                                               '#:tail tail)))])
             name))]
      [(_ orig-stx)
       (parse-identifier-syntax-transformer #'orig-stx
                                            #'dot-transformer-compiletime
                                            '(#:is_static #:tail)
                                            (lambda (p ct)
                                              ct)
                                            (lambda (ps ct)
                                              ct))]))

  (define-syntax (dot-transformer-compiletime stx)
    (syntax-parse stx
      [(_ pre-parseds self-ids extra-argument-ids)
       (parse-transformer-definition-rhs (syntax->list #'pre-parseds)
                                         (syntax->list #'self-ids)
                                         (syntax->list #'extra-argument-ids)
                                         #'values
                                         #`(#'() syntax-static-infos))])))
