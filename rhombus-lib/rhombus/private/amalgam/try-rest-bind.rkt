#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "annotation-string.rkt")
         "provide.rkt"
         "expression.rkt"
         "binding.rkt"
         "parse.rkt"
         "static-info.rkt"
         "if-blocked.rkt"
         "treelist.rkt"
         (submod "composite.rkt" for-rest))

(provide (for-spaces (#f
                      rhombus/bind)
                     try-rest-bind))

(define-syntax try-rest-bind
  (expression-prefix-operator
   `()
   'macro
   (lambda (tail) (error "should not get here"))))

(define-binding-syntax try-rest-bind
  (binding-prefix-operator
   `()
   'macro
   (lambda (tail)
     (syntax-parse tail
       [(_ static-infos constructor
           head-mode as-treelist? rest-to-repetition
           first-rest-arg::binding
           rest-arg ...)
        #:with first-rest::binding-form #'first-rest-arg.parsed
        #:with rest::binding #'(group constructor (brackets rest-arg ...))
        #:with rest-rest::binding-form #'rest.parsed
        (values
         (binding-form
          #'try-rest-bind-infoer
          #'[static-infos head-mode as-treelist? rest-to-repetition
                          first-rest.infoer-id first-rest.data
                          rest-rest.infoer-id rest-rest.data])
         #'())]))))

(define-syntax (try-rest-bind-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos [static-infos head-mode as-treelist? rest-to-repetition
                                      first-rest-infoer-id first-rest-data
                                      rest-rest-infoer-id rest-rest-data])
     #:with all-static-infos (static-infos-union #'static-infos #'up-static-infos)
     #:with first-rest-impl::binding-impl #'(first-rest-infoer-id all-static-infos first-rest-data)
     #:with rest-rest-impl::binding-impl #'(rest-rest-infoer-id all-static-infos rest-rest-data)
     #:with first-i::binding-info #'first-rest-impl.info
     #:with rest-i::binding-info #'rest-rest-impl.info
     (define (strip-constructor str)
       ;; relies on constructor being "List" or "PairList"
       (substring str
                  (if (syntax-e #'as-treelist?) 5 9)
                  (sub1 (string-length str))))
     (binding-info (annotation-string-from-pattern
                    (string-append (if (eq? (syntax-e #'head-mode) '#:splice) "& " "")
                                   (annotation-string-to-pattern
                                    (syntax-e #'first-i.annotation-str))
                                   (if (eq? (syntax-e #'head-mode) '#:splice) ", " ", ..., ")
                                   (strip-constructor
                                    (annotation-string-to-pattern
                                     (syntax-e #'rest-i.annotation-str)))))
                   #'rest
                   #'()
                   (append (syntax->list #'first-i.bind-infos)
                           (syntax->list #'rest-i.bind-infos))
                   #'try-rest-matcher
                   #'evidence
                   #'try-rest-committer
                   #'try-rest-binder
                   #`(head-mode
                      as-treelist? rest-to-repetition
                      evidence
                      prefix suffix #,(generate-temporaries-tree #`(#,(if (eq? (syntax-e #'head-mode) '#:splice)
                                                                          #'first-i.evidence-ids
                                                                          #'())
                                                                    rest-i.evidence-ids))
                      first-i.matcher-id first-i.evidence-ids first-i.committer-id first-i.binder-id first-i.data
                      first-i.bind-infos #,(and (eq? (syntax-e #'head-mode) '#:repetition)
                                                (generate-temporaries #'(first-i.bind-id ...)))
                      rest-i.matcher-id rest-i.evidence-ids rest-i.committer-id rest-i.binder-id rest-i.data))]))

(define-syntax (try-rest-matcher stx)
  (syntax-parse stx
    [(_ val-id (head-mode
                as-treelist? rest-to-repetition
                evidence _ _ _
                first-matcher-id first-evidence-ids first-committer-id first-binder-id first-data
                first-bind-infos first-seq-tmp-ids
                rest-matcher-id rest-evidence-ids rest-committer-id rest-binder-id rest-data)
        IF success fail)
     #:with ((first-bind-id first-bind-uses . _) ...) #'first-bind-infos
     (case (syntax-e #'head-mode)
       [(#:splice)
        (with-syntax ([(list-length list-sublist) (if (syntax-e #'as-treelist?)
                                                      #'(treelist-length treelist-sublist)
                                                      #'(length pairlist-sublist))])
          #`(begin
              (define evidence
                (let ([len (list-length val-id)])
                  (let loop ([i len])
                    (cond
                      [(= i -1) #f]
                      [else
                       (define prefix (list-sublist val-id 0 i))
                       (first-matcher-id prefix first-data
                                         if/blocked
                                         (let ([suffix (list-sublist val-id i len)])
                                           (rest-matcher-id suffix rest-data
                                                            if/blocked
                                                            (vector prefix suffix #,@(flatten-tree #'(first-evidence-ids rest-evidence-ids)))
                                                            (loop (sub1 i))))
                                         (loop (sub1 i)))]))))
              (IF evidence success fail)))]
       [else
        (with-syntax ([(state init state-done? state-next state-first state-rest
                              accum-next accum-result)
                       (if (syntax-e #'as-treelist?)
                           (list #'i
                                 #'0
                                 #'(lambda (i) (= i (treelist-length val-id)))
                                 #'add1
                                 #'(lambda (i) (treelist-ref val-id i))
                                 #'(lambda (i) (treelist-sublist val-id (add1 i)  (treelist-length val-id)))
                                 #'(lambda (accum i) null)
                                 #'(lambda (accum i) (treelist-sublist val-id 0 (add1 i))))
                           (list #'lst
                                 #'val-id
                                 #'null?
                                 #'cdr
                                 #'car
                                 #'cdr
                                 #'(lambda (accum lst) (cons (car lst) accum))
                                 #'(lambda (accum lst) (reverse (cons (car lst) accum)))))])
          #`(begin
              (define evidence
                (let loop ([state init] [accum null])
                  (cond
                    [(state-done? state)
                     #f]
                    #,(if (free-identifier=? #'always-succeed #'first-matcher-id)
                          #`[else
                             (or (loop (state-next state) (accum-next accum state))
                                 (let ([suffix (state-rest state)])
                                   (rest-matcher-id suffix rest-data
                                                    if/blocked
                                                    (let ([result (accum-result accum state)])
                                                      (vector (lambda () result) suffix #,@(flatten-tree #'rest-evidence-ids)))
                                                    #f)))]
                          #`[else
                             (define elem (state-first state))
                             (first-matcher-id elem first-data
                                               if/blocked
                                               (let* ([elem-evidence (lambda ()
                                                                       (first-committer-id elem first-evidence-ids first-data)
                                                                       (first-binder-id elem first-evidence-ids first-data)
                                                                       (values (maybe-repetition-as-list first-bind-id first-bind-uses)
                                                                               ...))]
                                                      [accum-evidence (cons elem-evidence accum)])
                                                 (or (loop (state-next state) accum-evidence)
                                                     (let ([suffix (state-rest state)])
                                                       (rest-matcher-id suffix rest-data
                                                                        if/blocked
                                                                        (let ([getter (build-overall-rest-getter '(first-bind-id ...)
                                                                                                                 accum-evidence)])
                                                                          (vector getter suffix #,@(flatten-tree #'rest-evidence-ids)))
                                                                        #f))))
                                               #f)]))))
              (IF evidence success fail)))])]))

(define-syntax (try-rest-committer stx)
  (syntax-parse stx
    [(_ val-id evidence (head-mode
                         as-treelist? rest-to-repetition
                         _ prefix suffix (~and evidence-ids (first-evidence-ids rest-evidence-ids))
                         first-matcher-id _ first-committer-id first-binder-id first-data
                         first-bind-infos first-seq-tmp-ids
                         rest-matcher-id _ rest-committer-id rest-binder-id rest-data))
     #`(begin
         (define-values (prefix suffix #,@(flatten-tree #'evidence-ids)) (vector->values evidence))
         #,(if (eq? (syntax-e #'head-mode) '#:splice)
               #`(first-committer-id prefix first-evidence-ids first-data)
               #'(define-values first-seq-tmp-ids (prefix)))
         (rest-committer-id suffix rest-evidence-ids rest-data))]))

(define-syntax (try-rest-binder stx)
  (syntax-parse stx
    [(_ val-id evidence (head-mode
                         as-treelist? rest-to-repetition
                         _ prefix suffix (first-evidence-ids rest-evidence-ids)
                         first-matcher-id _ first-committer-id first-binder-id first-data
                         first-bind-infos first-seq-tmp-ids
                         rest-matcher-id _ rest-committer-id rest-binder-id rest-data))
     #:with ((first-bind-id first-bind-uses first-bind-static-info ...) ...) #'first-bind-infos
     #`(begin
         #,@(if (eq? (syntax-e #'head-mode) '#:splice)
                #'((first-binder-id prefix first-evidence-ids first-data))
                (make-repetition-bind #'(first-bind-uses ...)
                                      #'(first-bind-id ...)
                                      #'((first-bind-static-info ...) ...)
                                      #'first-seq-tmp-ids
                                      (free-identifier=? #'always-succeed #'first-matcher-id)
                                      #'rest-to-repetition))
         (rest-binder-id suffix rest-evidence-ids rest-data))]))

(define-for-syntax (flatten-tree t)
  (cond
    [(identifier? t) (list t)]
    [else (apply append (map flatten-tree (syntax->list t)))]))

(define-for-syntax (generate-temporaries-tree t)
  (cond
    [(identifier? t) (car (generate-temporaries (list t)))]
    [else (map generate-temporaries-tree (syntax->list t))]))

(define (pairlist-sublist l s e)
  (for/list ([i (in-range (- e s))]
             [elem (in-list (list-tail l s))])
    elem))
