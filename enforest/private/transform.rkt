#lang racket/base
(require syntax/stx
         "../proc-name.rkt")

(provide transform-in
         transform-out
         call-as-transformer
         check-transformer-result
         track-sequence-origin
         transform-binder
         transform-use-scope-accumulate-key
         (struct-out transform-use-sites))

(define no-props (datum->syntax #f #f))

(define current-transformer-introduce (make-parameter (lambda (stx) stx)))
(define (transform-in stx)
  ((current-transformer-introduce) stx))
(define (transform-out stx)
  ((current-transformer-introduce) stx))

;; use internal-definition contexts to allow scope pruning
(define (make-syntax-introducer/intdef)
  (define intdef-ctx (syntax-local-make-definition-context))
  (define x0 (datum->syntax #f 'x))
  (define x (internal-definition-context-add-scopes intdef-ctx x0))
  (define intro (make-syntax-delta-introducer x x0))
  (lambda (stx [op 'flip]) (intro stx op)))

(struct transform-use-sites ([ctx #:mutable] base-ctx))
;; mapped to a boxed syntax object:
(define transform-use-scope-accumulate-key (gensym))

(define (transform-binder stx)
  (define accum (continuation-mark-set-first #f transform-use-scope-accumulate-key))
  (if accum
      ((make-syntax-delta-introducer (transform-use-sites-ctx accum)
                                     (transform-use-sites-base-ctx accum))
       stx
       'remove)
      stx))

(define (call-as-transformer id track-origin thunk
                             #:use-sites? [use-sites? #f])
  (define intro (make-syntax-introducer))
  (define use (and use-sites?
                   (let ([accum (continuation-mark-set-first #f transform-use-scope-accumulate-key)])
                     (and accum
                          (let ([use (make-syntax-introducer/intdef)])
                            (set-transform-use-sites-ctx! accum (use (transform-use-sites-ctx accum)))
                            use)))))
  (define flip (if use
                   (lambda (stx) (intro (use stx)))
                   intro))
  (parameterize ([current-transformer-introduce flip])
    (thunk flip
           (lambda (stx)
             (let loop ([stx stx])
               (cond
                 [(syntax? stx)
                  (track-origin (intro stx)
                                (let ([du (syntax-property id 'disappeared-use)])
                                  (if du
                                      (syntax-property no-props 'disappeared-use du)
                                      no-props))
                                id)]
                 [(pair? stx) (cons (loop (car stx))
                                    (loop (cdr stx)))]
                 [else stx]))))))

(define (check-transformer-result form tail proc)
  (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
  ;; we'd like to check for a syntax list in `tail`, but that's not constant-time
  (unless (or (pair? tail)
              (null? tail)
              (and (syntax? tail)
                   (let ([e (syntax-e tail)])
                     (or (pair? e) (null? e)))))
    (raise-result-error (proc-name proc) "stx-list?" tail))
  (values form tail))

(define (track-sequence-origin stx from-stx id)
  (datum->syntax stx
                 (for/list ([e (in-list (syntax->list stx))])
                   (syntax-track-origin e from-stx id))
                 stx
                 stx))

