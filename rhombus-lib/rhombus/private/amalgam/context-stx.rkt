#lang racket/base
(require syntax/parse/pre
         "pack.rkt"
         "realm.rkt")

(provide extract-ctx
         extract-group-ctx)

(define (extract-ctx who ctx-stx
                     #:false-ok? [false-ok? #t]
                     #:update [update #f]
                     #:update-outer [update-outer (lambda (outer-stx innner-stx) outer-stx)]
                     #:annot [annot #f])
  (and (or (not false-ok?) ctx-stx)
       (let ([t (and (syntax? ctx-stx)
                     (unpack-term ctx-stx #f #f))])
         (unless t
           (raise-argument-error* who rhombus-realm (or annot (if false-ok? "maybe(Term)" "Term")) ctx-stx))
         (syntax-parse t
           #:datum-literals (op)
           [((~and tag op) id)
            (if update
                (let ([inner-stx (update #'id)])
                  (update-outer (datum->syntax t (list #'tag inner-stx) t t)
                                inner-stx))
                #'id)]
           [((~and tag parsed) space o)
            (if update
                (let ([inner-stx (update #'o)])
                  (update-outer (datum->syntax t (list #'tag #'space inner-stx) t t)
                                inner-stx))
                #'o)]
           [(head . tail)
            (if update
                (let ([inner-stx (update #'head)])
                  (update-outer (datum->syntax t (cons inner-stx #'tail) t t)
                                inner-stx))
                #'head)]
           [_ (if update
                  (update t)
                  t)]))))

(define (extract-group-ctx who ctx-stx
                           #:false-ok? [false-ok? #t]
                           #:update [update #f]
                           #:annot [annot #f])
  (and (or (not false-ok?) ctx-stx)
       (let ([t (and (syntax? ctx-stx)
                     (unpack-group ctx-stx #f #f))])
         (unless t
           (raise-argument-error* who rhombus-realm (or annot (if false-ok? "maybe(Group)" "Group")) ctx-stx))
         (syntax-parse t
           #:datum-literals (group)
           [((~and tag group) . tail)
            (if update
                (datum->syntax t (cons (update #'tag) #'tail) t t)
                #'tag)]
           [_ (if update
                  (update t)
                  t)]))))
