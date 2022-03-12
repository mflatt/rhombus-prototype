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
       [(_ (~and ((~or parens quotes) . ts) gs) . tail)
        (values #`(quote-syntax #,(pack-multi (cons any-blank #'ts))) #'tail)]))))

;; ----------------------------------------

(define (relevant-source-syntax ctx-stx-in)
  ;; In the case of `multi` or `group`, find the outermost with source information
  (syntax-parse ctx-stx-in
    #:datum-literals (multi group block alts parens brackets quotes braces op)
    [(multi head (g-head t)) (or (source-term #'head)
                                 (source-term #'g-head)
                                 #'t)]
    [(multi head (g-head . _)) (or (source-term #'head)
                                   #'g-head)]
    [((~and g-head group) t) (or (source-term #'g-head)
                                 #'t)]
    [((~and g-head group) . _) #'g-head]
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
  (unless (syntax? ctx-stx-in) (raise-argument-error* 'relocate_syntax rhombus-realm "Syntax" ctx-stx-in))
  (define ctx-stx (relevant-source-syntax ctx-stx-in))
  (log-error "?? ~s" (syntax->datum stx))
  (log-error "    @ ~s" (syntax->datum ctx-stx-in))
  (log-error "    = ~s" (syntax->datum ctx-stx))
  (syntax-parse stx
    #:datum-literals (multi parens)
    [(multi (~and p parens) . _) (log-error "    ~s" #'p)]
    [_ (void)])
  (define (relocate stx)
    (datum->syntax stx (syntax-e stx) ctx-stx ctx-stx))
  (define (relocate-term stx)
    (syntax-parse stx
      #:datum-literals (block alts parens brackets braces quotes op)
      [((~and head (~or block alts parens brackets braces quotes)) . rest)
       (datum->syntax #f (cons (relocate #'head) #'rest))]
      [((~and head op) o)
       (datum->syntax #f (list #'head (relocate #'o)))]
      [_
       (datum->syntax stx (syntax-e stx) ctx-stx ctx-stx)]))
  ;; Push information down to the innermost that doesn't have information
  (syntax-parse stx
    #:datum-literals (multi group)
    [((~and m multi) head (~and g (g-head t)))
     (cond
       [(source-term #'head)
        (datum->syntax #f (list #'m (relocate #'head) #'g))]
       [(source-term #'g-head)
        (datum->syntax #f (list #'m #'head (list (relocate #'g-head) #'t)))]
       [else
        (datum->syntax #f (list #'m #'head (list #'g-head (relocate-term #'t))))])]
    [((~and m multi) head (~and g (g-head . rest)))
     (cond
       [(source-term #'head)
        (datum->syntax #f (list #'m (relocate #'head) #'g))]
       [else
        (datum->syntax #f (list #'m #'head (cons (relocate #'g-head) #'rest)))])]
    [_
     (relocate-term stx)]))

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


;; ----------------------------------------

(define (source-term e)
  (and (or (syntax-srcloc e)
           (syntax-raw-prefix-property e)
           (syntax-raw-suffix-property e)
           (syntax-raw-tail-property e)
           (syntax-raw-tail-suffix-property e))
       e))
