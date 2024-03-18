#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt")

(define-static-info-key-syntax/provide #%call-result
  (static-info-key static-infos-union
                   static-infos-intersect))

(define-static-info-key-syntax/provide #%call-results-at-arities
  (let ([merge-tables (lambda (a b keep-one? combine)
                        ;; FIXME: improve merging
                        (and keep-one?
                             a))])
    (static-info-key (lambda (a b)
                       (merge-tables a b #t static-infos-union))
                     (lambda (a b)
                       (merge-tables a b #f static-infos-intersect)))))
