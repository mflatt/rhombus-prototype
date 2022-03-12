#lang racket/base
(require syntax/stx
         enforest/proc-name
         "realm.rkt")

;; We represent Rhombus syntax objects as a syntax object with one of
;; the following forms:
;;   (multi head (group term ...) ...) - as a general multi-group syntax object,
;;                                       where `head` preserves properies
;;   (group term ...) - for a group syntax object
;;   term - for a single-term syntax object
;; Coercions among these happen automatically. So, the Rhombus expression
;;   'x'
;; could be represented for most purposes as #'(multi any (group x)) or
;; just #'x.

;; "Pack" means going form the internal Racket-side representation to
;; the Rhombus repesentation, such as when sending syntax to a
;; Rhombus-implemented macro, and "unpack" means going back.

;; "Tail" below refers to the argument to a macro for the rest of the
;; enclosing sequence. We represent it using `multi` on the Rhombus
;; side, but it's just a list on the Racket side, which means adding
;; both `group` and them `multi` to get from here to there. On the way
;; back, we coerce from any of the three possible shapes, containing
;; `multi` to a single group.

(provide pack-tail
         unpack-tail
         pack-term
         unpack-term
         unpack-group
         pack-multi
         unpack-multi
         
         pack-term*
         unpack-term*
         pack-group*
         unpack-group*
         pack-multi*
         unpack-multi*
         pack-tail*
         unpack-tail*
         
         repack-group-or-term)

(define multi-blank (syntax-property (datum->syntax #f 'multi) 'raw ""))
(define group-blank (syntax-property (datum->syntax #f 'group) 'raw ""))

;; Used as a synthesized head for `multi`:
(define any-blank (syntax-property (datum->syntax #f 'any) 'raw ""))

(define (group-syntax? r)
  (and (syntax? r)
       (pair? (syntax-e r))
       (eq? 'group (syntax-e (car (syntax-e r))))))

(define (multi-syntax? r)
  (and (syntax? r)
       (pair? (syntax-e r))
       (eq? 'multi (syntax-e (car (syntax-e r))))))

;; ----------------------------------------

;; assumes that `tail` is a syntax list, and wraps it with `multi`;
;; an empty list turns into `multi` with no groups
(define (pack-tail tail #:after [after #f])
  (if (stx-null? tail)
      (cond
        [(and after
              (syntax-position after)
              (syntax-span after))
         (define loc (srcloc (syntax-source after)
                             (syntax-line after)
                             (let ([col (syntax-column after)])
                               (and col (+ col (syntax-span after))))
                             (+ (syntax-position after) (syntax-span after))
                             0))
         (datum->syntax #f (list multi-blank
                                 (syntax-property (syntax/loc loc any) 'raw "")))]
        [else (datum->syntax #f (list multi-blank any-blank))])
      (datum->syntax #f (list multi-blank any-blank (cons group-blank tail)))))

;; assumes that `packed-tail` represents a shrubbery,
;; and unpacks by removing `multi` ... `group` wrapper
(define (unpack-tail r who)
  (datum->syntax
   #f
   (cond
     [(multi-syntax? r)
      (define l (syntax->list r))
      (cond
        [(null? (cddr l)) '()]
        [(= 3 (length l)) (cdr (syntax-e (caddr l)))]
        [else (raise-error who "multi-group syntax not allowed in group context" r)])]
     [(group-syntax? r) (cdr (syntax-e r))]
     [(or (null? r) (pair? r)) (cannot-coerce-list who r)]
     [else (list r)])))

;; `stx` is a single term
(define (pack-term stx) stx)

(define (unpack-term form who)
  (define (fail)
    (raise-error who "multi-term syntax not allowed in term context" form))
  (let loop ([r form])
    (cond
      [(multi-syntax? r)
       (define l (syntax->list r))
       (if (= 3 (length l))
           (loop (caddr l))
           (fail))]
      [(group-syntax? r)
       (define l (syntax->list r))
       (if (= 2 (length l))
           (cadr l)
           (fail))]
      [(list? r) (cannot-coerce-list who r)]
      [else r])))

;; "Unpacks" to a `(group term ...)` form, as opposed to just `(term
;; ...)`, which makes is symmetric with `pack-group` and preserves
;; properties on the `group` tag. So, unpacking here is really about
;; coercing from different representations, as opposed to changing a
;; `group` representation.
(define (unpack-group r who)
  (cond
    [(multi-syntax? r)
     (define l (syntax->list r))
     (cond
       [(= 3 (length l)) (syntax-e (caddr l))]
       [else (raise-error who "multi-group syntax not allowed in group context" r)])]
    [(group-syntax? r) r]
    [(or (null? r) (pair? r)) (cannot-coerce-list who r)]
    [else (datum->syntax #f (list group-blank r))]))

;; `r` is a terms like `(parens ...)` or `(block ...)`
(define (pack-multi r)
  (datum->syntax #f (cons multi-blank r)))

;; `r` can be any of the allowed representations (multi-group, single-group, or single-term),
;; and the result is a list of group syntax objects (asymmetric to `pack-multi`)
(define (unpack-multi r who)
  (cond
    [(multi-syntax? r) (cddr (syntax->list r))]
    [(group-syntax? r) (list r)]
    [(or (null? r) (pair? r)) (cannot-coerce-list who r)]
    [else (list (list group-blank r))]))

;; ----------------------------------------

;; The `pack*` and `unpack*` variants deal with repetition
;; `depth` layers deep as needed for patterns and templates

(define (pack* stx depth wrap)
  (cond
    [(eqv? depth 0) (wrap stx)]
    [else (for/list ([t (in-list (syntax->list stx))])
            (pack* t (sub1 depth) wrap))]))

(define (unpack* qs r depth unwrap)
  (datum->syntax
   qs
   (let unpack* ([r r] [depth depth])
     (cond
       [(eqv? depth 0)
        (unwrap r)]
       [else
        (if (list? r)
            (datum->syntax
             qs
             (for/list ([r (in-list r)])
               (unpack* r (sub1 depth))))
            (raise-argument-error* '... rhombus-realm "List" r))]))))

(define (pack-term* stx depth)
  (pack* stx depth pack-term))
                     
(define (unpack-term* qs r depth)
  (unpack* qs r depth
           (lambda (r) (unpack-term r (syntax-e qs)))))

;; Packs to a `group` form, where the `stx`es start with `group` already
(define (pack-group* stx depth)
  (pack* stx depth (lambda (stx) stx)))

;; "Unpacks" to a `group` form, which is really more about coercsions
(define (unpack-group* qs r depth)
  (unpack* qs r depth
           (lambda (r)
             (unpack-group r (syntax-e qs)))))

;; Packs to a `multi` form, the the inner `stxes`s have a head ter like
;; `parens` or `block`
(define (pack-multi* stxes depth)
  (pack* stxes depth (lambda (stxes)
                       (datum->syntax #f (cons multi-blank stxes)))))

;; Unpacks to a list of groups, which is asymmetric to `pack-multi*`
(define (unpack-multi* qs r depth)
  (unpack* qs r depth
           (lambda (r)
             (unpack-multi r qs))))

(define (pack-tail* stxes depth)
  (pack* stxes depth pack-tail))

(define (unpack-tail* qs r depth)
  (pack* r depth (lambda (r)
                   (unpack-tail r (if (symbol? qs) qs (syntax-e qs))))))

;; normalize as input for pattern matching:
(define (repack-group-or-term r)
  (cond
    [(group-syntax? r) (list multi-blank any-blank r)]
    [(multi-syntax? r) r]
    [else (list multi-blank any-blank (list group-blank r))]))

(define (cannot-coerce-list who r)
  (raise-arguments-error* who rhombus-realm
                          "cannot coerce list to syntax"
                          "list" r))

(define (raise-error who msg r)
  (if (procedure? who)
      (raise-result-error (proc-name who) msg r)
      (raise-arguments-error* who rhombus-realm
                              msg
                              "syntax" r)))
