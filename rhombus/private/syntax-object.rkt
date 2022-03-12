#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "pack.rkt")
         syntax/parse
         racket/syntax-srcloc
         shrubbery/property
         "expression.rkt"
         "pack.rkt"
         "realm.rkt")

(provide literal_syntax
         to_syntax
         unwrap_syntax
         relocate_syntax
         relocate_span_syntax)

(define-syntax literal_syntax
  (expression-transformer
   #'literal_syntax
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens quotes group)
       [(_ ((~or parens quotes) (group term)) . tail)
        ;; Note: discarding group properties in this case
        (values #'(quote-syntax term) #'tail)]
       [(_ (~and ((~or parens quotes) . _) gs) . tail)
        (values #`(quote-syntax #,(pack-multi #'gs)) #'tail)]))))

;; ----------------------------------------

(define (relevant-source-syntax ctx-stx-in)
  (syntax-parse ctx-stx-in
    #:datum-literals (multi group block alts parens brackets quotes braces op)
    [(multi head (group t)) #'head]
    [(group t) (relevant-source-syntax #'t)]
    [((~and head (~or block alts parens brackets quotes braces)) . _) #'head]
    [(op o) #'o]
    [_ ctx-stx-in]))

(define (to_syntax v)
  (datum->syntax #f v))

(define (unwrap_syntax v)
  (cond
    [(not (syntax? v))
     (raise-argument-error* 'unwrap_syntax rhombus-realm "Syntax" v)]
    [else
     (syntax-e (unpack-term v 'unwrap_syntax))]))

(define (relocate_syntax stx ctx-stx-in)
  (unless (syntax? stx) (raise-argument-error* 'relocate_syntax rhombus-realm "Syntax" stx))
  (syntax-parse stx
    #:datum-literals (group)
    [((~and head group) . rest)
     ;; transfer group context to group context?
     (syntax-parse ctx-stx-in
       #:datum-literals (group)
       [((~and group g) . _)
        (datum->syntax (cons (datum->syntax #'head (syntax-e #'head) #'g #'g) #'rest))]
       [else
        (error 'relocate_syntax "cannot transfer non-group context to group syntax object")])]
    [else
     (define ctx-stx (relevant-source-syntax ctx-stx-in))
     (syntax-parse stx
       #:datum-literals (multi block alts parens brackets braces quotes op)
       [((~and m multi) head . rest)
        (datum->syntax #f (list* #'m (datum->syntax #'head (syntax-e #'head) ctx-stx ctx-stx) #'rest))]
       [((~and head (~or block alts parens brackets braces quotes)) . rest)
        (datum->syntax (cons (datum->syntax #'head (syntax-e #'head) ctx-stx ctx-stx) #'rest))]
       [((~and head op) o)
        (datum->syntax #f (list #'head (datum->syntax #'o (syntax-e #'o) ctx-stx ctx-stx)))]
       [_
        (datum->syntax stx (syntax-e stx) ctx-stx ctx-stx)])]))

(define (relocate_span_syntax stx ctx-stxes-in
                              #:keep_raw_interior [keep-raw-interior? #f])
  (unless (syntax? stx) (raise-argument-error* 'relocate_span_syntax rhombus-realm "Syntax" stx))
  (define ctx-stxes (map relevant-source-syntax ctx-stxes-in))
  (define (combine-raw a b) (if (null? a) b (if (null? b) a (cons a b))))
  (let loop ([ctx-stxes (cdr ctx-stxes)]
             [loc (syntax-srcloc (car ctx-stxes))]
             [pre (or (syntax-raw-prefix-property (car ctx-stxes)) null)]
             [raw (if keep-raw-interior?
                      (or (syntax-raw-property (car ctx-stxes)) null)
                      null)]
             [suffix (combine-raw
                      (if keep-raw-interior?
                          (or (syntax-raw-tail-property (car ctx-stxes)) null)
                          null)
                      (if (or keep-raw-interior?
                              (null? (cdr ctx-stxes)))
                          (or (syntax-raw-suffix-property (car ctx-stxes)) null)
                          null))])
    (cond
      [(null? ctx-stxes)
       (let* ([ctx (datum->syntax #f #f loc)]
              [ctx (if (null? pre)
                       ctx
                       (syntax-raw-prefix-property ctx pre))]
              [ctx (syntax-raw-property ctx raw)]
              [ctx (if (null? suffix)
                       ctx
                       (syntax-raw-suffix-property ctx suffix))])
         (relocate_syntax stx ctx))]
      [(and (pair? (cdr ctx-stxes))
            (not keep-raw-interior?))
       (loop (cdr ctx-stxes) pre raw suffix)]
      [else
       (define empty-raw? (and (null? raw) (null? suffix)))
       (define ctx (car ctx-stxes))
       (define new-raw (or (syntax-raw-property ctx) null))
       (define new-loc (syntax-srcloc ctx))
       (loop (cdr ctx-stxes)
             (if (and loc
                      new-loc
                      (equal? (srcloc-source loc)
                              (srcloc-source new-loc)))
                 (srcloc (srcloc-source loc)
                         (srcloc-line loc)
                         (srcloc-column loc)
                         (srcloc-position loc)
                         (if (and (srcloc-position new-loc)
                                  (srcloc-span new-loc)
                                  (srcloc-position loc))
                             (- (+ (srcloc-position new-loc)
                                   (srcloc-span new-loc))
                                (srcloc-position loc))
                             (srcloc-span loc)))
                 loc)
             (if empty-raw?
                 (combine-raw pre (or (syntax-raw-prefix-property ctx) null))
                 pre)
             (if empty-raw?
                 (or (syntax-raw-property ctx) null)
                 (combine-raw (combine-raw (combine-raw raw suffix)
                                           (or (syntax-raw-prefix-property ctx) null))
                              (or (syntax-raw-property ctx) null)))
             (combine-raw (if keep-raw-interior?
                              (or (syntax-raw-tail-property ctx) null)
                              null)
                          (combine-raw
                           (or (syntax-raw-tail-suffix-property ctx) null)
                           (or (syntax-raw-suffix-property ctx) null))))])))

