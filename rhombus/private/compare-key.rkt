#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "static-info.rkt")

(define-static-info-key-syntax/provide #%compare
  (static-info-key (lambda (a b)
                     a)
                   (lambda (a b)
                     (if (or (identifier? a) (identifier? b))
                         (static-info-identifier-intersect a b)
                         (let ([as (syntax->list a)]
                               [bs (syntax->list b)])
                           (and as
                                bs
                                ;; we could try to intersect at a finer granularity, but
                                ;; just check whether they're the same
                                (= (length as) (length bs))
                                (for/and ([a (in-list as)]
                                          [b (in-list bs)])
                                  (syntax-parse (list a b)
                                    [((a-op a-id:identifier) (b-op b-id:identifier))
                                     (and (eq? (syntax-e #'a-op) (syntax-e #'b-op))
                                          (free-identifier=? #'a-id #'b-id))]
                                    [_ #false]))
                                a))))))
