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
         "static-info.rkt")

(provide for)

(define-syntax for
  (expression-transformer
   #'for
   (lambda (stx)
     (syntax-parse (respan stx)
       #:datum-literals (block)
       [(form-id (block body ...+))
        (values (build-for-form #'form-id '([result (void)]) #'values #'void-result #'(body ...))
                #'())]
       [(form-id fld ... (block body ...+))
        #:with g-tag group-tag
        #:with fldr::folder #'(g-tag fld ...)
        #:with f::folder-form #'fldr.parsed
        (values (build-for-form #'form-id #'f.binds #'f.wrapper #'f.body-wrapper #'(body ...))
                #'())]))))

(define-for-syntax (build-for-form form-id binds wrapper inner-wrapper bodys)
  (define-values (clauses body)
    (let loop ([bodys (syntax->list bodys)]
               [rev-clauses null]
               [rev-bodys null]
               [matcher-form #'(void)]
               [binder-form #'(void)]
               [started-group? #f])
      (cond
        [(null? bodys)
         (when (null? rev-bodys)
           (raise-syntax-error #f
                               "empty body after `~each` and `~and` clauses"
                               form-id))
         (values
          (reverse rev-clauses)
          #`(begin
              #,matcher-form
              #,binder-form
              (#,inner-wrapper
               (rhombus-body
                . #,(reverse rev-bodys)))))]
        [else
         (syntax-parse (car bodys)
           #:datum-literals (group block)
           [(group (~or #:each #:and) . _)
            #:when (pair? rev-bodys)
            (loop bodys
                  (list* (list binder-form
                               matcher-form
                               #`(rhombus-body-sequence
                                  . #,(reverse rev-bodys)))
                         #'#:do
                         rev-clauses)
                  null
                  #'(void)
                  #'(void)
                  #f)]
           [(group #:each . _)
            ;; assert: (null? rev-bodys)
            #:when started-group?
            (loop bodys
                  rev-clauses
                  (cons #'(group (parsed (void))) rev-bodys)
                  matcher-form
                  binder-form
                  started-group?)]
           [(group (~and tag #:and) . _)
            #:when (not started-group?)
            (raise-syntax-error #f
                                "found `~and` without preceding `~each`"
                                form-id
                                #'tag)]
           [(group (~or #:each #:and) any ... (~and rhs-block (block body ...)))
            ;; assert: (null? rev-bodys)
            #:with lhs::binding #'(group any ...)
            #:with lhs-e::binding-form #'lhs.parsed
            #:with rhs (rhombus-local-expand (enforest-expression-block #'rhs-block))
            #:with static-infos (or (syntax-local-static-info #'rhs #'#%ref-result)
                                    #'())
            #:with lhs-impl::binding-impl #'(lhs-e.infoer-id static-infos lhs-e.data)
            #:with lhs-i::binding-info #'lhs-impl.info
            #:with (tmp-id) (generate-temporaries #'(lhs-i.name-id))
            (define seq-ctr (syntax-local-static-info #'rhs #'#%sequence-constructor))
            (loop (cdr bodys)
                  (cons
                   #`[tmp-id #,(cond
                                 [seq-ctr #`(#,seq-ctr rhs)]
                                 [else #'rhs])]
                   rev-clauses)
                  null
                  #`(begin
                      #,matcher-form
                      (lhs-i.matcher-id tmp-id
                                        lhs-i.data
                                        flattened-if
                                        (void)
                                        (rhs-binding-failure '#,form-id tmp-id 'lhs-i.annotation-str)))
                  #`(begin
                      #,binder-form
                      (lhs-i.binder-id tmp-id lhs-i.data)
                      (define-static-info-syntax/maybe lhs-i.bind-id lhs-i.bind-static-info ...)
                      ...)
                  #t)]
           [(group #:each . _)
            (raise-syntax-error #f
                                "`~each` clause needs binding followed by value block"
                                form-id
                                (car bodys))]
           [(group #:and . _)
            (raise-syntax-error #f
                                "`~and` clause needs binding followed by value block"
                                form-id
                                (car bodys))]
           [_
            (loop (cdr bodys)
                  rev-clauses
                  (cons (car bodys) rev-bodys)
                  matcher-form
                  binder-form
                  started-group?)])])))
  #`(#,wrapper
     (for/fold #,binds
               #,clauses
       #,body)))

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
