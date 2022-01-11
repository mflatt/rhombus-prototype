#lang racket/base
(require (for-syntax racket/base
                     racket/syntax-srcloc
                     syntax/srcloc
                     syntax/parse
                     "tag.rkt"
                     "srcloc.rkt")
         "expression.rkt"
         "binding.rkt"
         "parse.rkt"
         "folder.rkt"
         "static-info.rkt"
         "ref-result-key.rkt")

(provide (rename-out [rhombus-for for]))

(define-syntax rhombus-for
  (expression-transformer
   #'for
   (lambda (stx)
     (syntax-parse (respan stx)
       #:datum-literals (block)
       [(form-id (block body ...+))
        (values #`(for (#:splice (for-clause-step #,stx [finish] body ...))
                    (finish))
                #'())]
       [(form-id fld ... (block body ...+))
        #:with g-tag group-tag
        #:with fldr::folder #'(g-tag fld ...)
        #:with f::folder-form #'fldr.parsed
        (values #`(f.wrapper
                   (for/fold f.binds (#:splice (for-clause-step #,stx [finish] body ...))
                     (f.body-wrapper (finish))))
                #'())]))))

(require (for-syntax racket/pretty))

(define-splicing-for-clause-syntax for-clause-step
  (lambda (stx)
    (syntax-parse stx
      #:datum-literals (group block)
      [(_ orig [finish] . bodys)
       ;; initialize state
       #`(#:splice (for-clause-step orig [finish () () (void) (void)]
                                    . bodys))]
      [(_ orig [finish rev-clauses rev-bodys matcher binder])
       (when (null? (syntax-e #'rev-bodys))
         (raise-syntax-error #f
                             "empty body after `~each` and `~and` clauses"
                             #'orig))
       #`(#,@(reverse (syntax->list #'rev-clauses))
          #:do [matcher
                binder
                (define (finish)
                  (rhombus-body
                   . #,(reverse (syntax->list #'rev-bodys))))])]
      [(_ orig [finish rev-clauses rev-bodys matcher binder]
          (~and body0 (group (~or #:each #:and). _))
          . bodys)
       #:when (pair? (syntax-e #'rev-bodys))
       ;; emit accumulated body and clauses before starting more clauses
       #`(#,@(reverse (syntax->list #'rev-clauses))
          #:do (matcher
                binder
                (rhombus-body-sequence
                 . #,(reverse (syntax->list #'rev-bodys))))
          #:splice (for-clause-step orig
                                    [finish () () (void) (void)]
                                    body0 . bodys))]
      [(_ orig [finish rev-clauses rev-bodys matcher binder]
          (~and body0 (group #:each . _))
          . bodys)
       #:when (pair? (syntax-e #'rev-clauses)) ; assert: empty rev-bodys
       ;; emit clauses before starting a new group
       #`(#,@(reverse (syntax->list #'rev-clauses))
          #:do [matcher binder]
          #:splice (for-clause-step orig
                                    [finish () () (void) (void)]
                                    body0 . bodys))]
      [(_ orig [finish rev-clauses rev-bodys matcher binder]
          (group (~and tag #:and) . _)
          . bodys)
       #:when (null? (syntax-e #'rev-clauses))
       (raise-syntax-error #f
                           "found `~and` without preceding `~each`"
                           #'orig
                           #'tag)]
      [(_ orig [finish rev-clauses rev-bodys matcher binder]
          (group (~or #:each #:and) any ... (~and rhs-block (block body ...)))
          . bodys)
       ;; parse binding to start or extend binding group
       #:with lhs::binding #'(group any ...)
       #:with lhs-e::binding-form #'lhs.parsed
       #:with rhs (rhombus-local-expand (enforest-expression-block #'rhs-block))
       #:with static-infos (or (syntax-local-static-info #'rhs #'#%ref-result)
                               #'())
       #:with lhs-impl::binding-impl #'(lhs-e.infoer-id static-infos lhs-e.data)
       #:with lhs-i::binding-info #'lhs-impl.info
       #:with (form-id . _) #'orig
       #:with (tmp-id) (generate-temporaries #'(lhs-i.name-id))
       (define seq-ctr (syntax-local-static-info #'rhs #'#%sequence-constructor))
       #`(#:splice (for-clause-step
                    orig
                    [finish
                     ([tmp-id #,(cond
                                  [seq-ctr #`(#,seq-ctr rhs)]
                                  [else #'rhs])]
                      . rev-clauses)
                     ()
                     (begin
                       matcher
                       (lhs-i.matcher-id tmp-id
                                         lhs-i.data
                                         flattened-if
                                         (void)
                                         (rhs-binding-failure 'form-id tmp-id 'lhs-i.annotation-str)))
                     (begin
                       binder
                       (lhs-i.binder-id tmp-id lhs-i.data)
                       (define-static-info-syntax/maybe lhs-i.bind-id lhs-i.bind-static-info ...)
                       ...)]
                    . bodys))]
      [(_ orig state
          (~and body0 (group (~and tag (~or #:each #:and)) . _))
          . bodys)
       (raise-syntax-error #f
                           (format "`~~~a` clause needs binding followed by value block"
                                   (keyword->string (syntax-e #'tag)))
                           #'orig
                           #'body0)]
      [(_ orig [finish rev-clauses rev-bodys matcher binder]
          body0
          . bodys)
       #`(#:splice (for-clause-step
                    orig
                    [finish
                     rev-clauses
                     (body0 . rev-bodys)
                     matcher
                     binder]
                    . bodys))])))

(define-syntax-rule (void-result e)
  (begin
    e
    (void)))

(define-syntax (flattened-if stx)
  (syntax-parse stx
    [(_ check-expr success-expr fail-expr)
     #'(begin
         (unless check-expr fail-expr)
         success-expr)]))

(define (rhs-binding-failure who val binding-str)
  (raise-binding-failure who "element" val binding-str))
