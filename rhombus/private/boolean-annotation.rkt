#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "with-syntax.rkt"
                     "annotation-string.rkt")
         "annotation.rkt"
         (submod "annotation.rkt" for-class)
         "binding.rkt"
         "static-info.rkt"
         "parse.rkt")

(provide (for-space rhombus/annot
                    &&
                    \|\|))

;; ----------------------------------------
;; &&

(define-annotation-syntax &&
  (annotation-infix-operator
   (annot-quote &&)
   (list (cons (annot-quote \|\|) 'stronger))
   'automatic
   (lambda (lhs rhs stx)
     (syntax-parse (list lhs rhs)
       [(l::annotation-predicate-form r::annotation-predicate-form)
        #:with (r-static-info ...) #'r.static-infos
        (annotation-predicate-form
         #`(let ([l-pred l.predicate]
                 [r-pred r.predicate])
             (lambda (v)
               (and (l-pred v) (r-pred v))))
         #`(r-static-info ... . l.static-infos))]
       [(l::annotation-binding-form r::annotation-binding-form)
        (annotation-binding-form
         (binding-form
          #'and-infoer
          #`[l.binding r.binding]))]))
   'left))

(define-syntax (and-infoer stx)
  (syntax-parse stx
    [(_ static-infos (lhs-i::binding-form rhs-i::binding-form))
     #:with lhs-impl::binding-impl #'(lhs-i.infoer-id static-infos lhs-i.data)
     #:with lhs::binding-info #'lhs-impl.info
     #:with (lhs-static-info ...) #'lhs.static-infos
     #:with rhs-impl::binding-impl #'(rhs-i.infoer-id (lhs-static-info ... . static-infos) rhs-i.data)
     #:with rhs::binding-info #'rhs-impl.info
     #:with (lhs-bind-info ...) #'lhs.bind-infos
     (binding-info (annotation-string-and (syntax-e #'lhs.annotation-str) (syntax-e #'rhs.annotation-str))
                   #'lhs.name-id
                   #'rhs.static-infos
                   #'rhs.bind-infos
                   #'and-matcher
                   #'and-committer
                   #'and-binder
                   #'(lhs rhs))]))

(define-syntax (and-matcher stx)
  (syntax-parse stx
    [(_ arg-id (lhs::binding-info rhs::binding-info) IF success fail)
     #'(lhs.matcher-id arg-id lhs.data IF
                       (begin
                         (lhs.committer-id arg-id lhs.data)
                         (lhs.binder-id arg-id lhs.data)
                         (rhs.matcher-id arg-id rhs.data IF success fail))
                       fail)]))

(define-syntax (and-committer stx)
  (syntax-parse stx
    [(_ arg-id (lhs::binding-info rhs::binding-info))
     #'(rhs.committer-id arg-id rhs.data)]))

(define-syntax (and-binder stx)
  (syntax-parse stx
    [(_ arg-id (lhs::binding-info rhs::binding-info))
     #`(rhs.binder-id arg-id rhs.data)]))

;; ----------------------------------------
;; ||

(define-annotation-syntax \|\|
  (annotation-infix-operator
   (annot-quote \|\|)
   null
   'automatic
   (lambda (lhs rhs stx)
     (syntax-parse (list lhs rhs)
       [(l::annotation-predicate-form r::annotation-predicate-form)
        (annotation-predicate-form
         #'(let ([l-pred l.predicate]
                 [r-pred r.predicate])
             (lambda (v)
               (or (l-pred v) (r-pred v))))
         (static-infos-intersect #'l.static-infos #'r.static-infos))]
       [(l::annotation-binding-form r::annotation-binding-form)
        (check-single-variable-binding #'l.binding lhs stx)
        (check-single-variable-binding #'r.binding rhs stx)
        (annotation-binding-form
         (binding-form
          #'or-infoer
          #'[l.binding r.binding]))]))
   'left))


(define-syntax (or-infoer stx)
  (syntax-parse stx
    [(_ static-infos (lhs-i::binding-form rhs-i::binding-form))
     #:with lhs-impl::binding-impl #'(lhs-i.infoer-id static-infos lhs-i.data)
     #:with lhs::binding-info #'lhs-impl.info
     #:with rhs-impl::binding-impl #'(rhs-i.infoer-id static-infos rhs-i.data)
     #:with rhs::binding-info #'rhs-impl.info
     #:with ((left-bind-id left-bind-use . left-static-infos)) #'lhs.bind-infos
     #:with ((right-bind-id right-bind-use . right-static-infos)) #'rhs.bind-infos
     #:with result-static-infos (static-infos-intersect #'left-static-infos #'right-static-infos)
     (binding-info (annotation-string-or (syntax-e #'lhs.annotation-str) (syntax-e #'rhs.annotation-str))
                   #'lhs.name-id
                   (static-infos-intersect #'lhs.static-infos #'rhs.static-infos)
                   #`((result (0) . result-static-infos))
                   #'or-matcher
                   #'or-committer
                   #'or-binder
                   #'(lhs rhs finish result left-bind-id right-bind-id))]))

(define-syntax (or-matcher stx)
  (syntax-parse stx
    [(_ arg-id (lhs::binding-info rhs::binding-info finish-id result-id left-bind-id right-bind-id)
        IF success fail)
     #`(begin
         (define finish-id
           (let ()
             (lhs.matcher-id arg-id lhs.data block-if
                             (lambda ()
                               (lhs.committer-id arg-id lhs.data)
                               (lhs.binder-id arg-id lhs.data)
                               left-bind-id)
                             (rhs.matcher-id arg-id rhs.data block-if
                                             (lambda ()
                                               (rhs.committer-id arg-id rhs.data)
                                               (rhs.binder-id arg-id rhs.data)
                                               right-bind-id)
                                             #f))))
         (IF finish-id success fail))]))

(define-syntax-rule (block-if a b c)
  (if (let () a)
      (let () b)
      (let () c)))

(define-syntax (or-committer stx)
  (syntax-parse stx
    [(_ arg-id (lhs rhs finish-id . _))
     #'(begin)]))

(define-syntax (or-binder stx)
  (syntax-parse stx
    [(_ arg-id (lhs rhs finish-id result-id . _))
     #'(define result-id (finish-id))]))
