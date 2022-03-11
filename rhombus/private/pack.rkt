#lang racket/base
(require syntax/stx
         enforest/proc-name
         "realm.rkt")

(provide pack-tail
         unpack-tail
         pack-term
         unpack-term
         pack-group
         unpack-group
         
         pack-term*
         unpack-term*
         pack-group*
         unpack-group*
         pack-block*
         unpack-block*
         pack-tail*
         unpack-tail*
         
         repack-group-or-term)

(define multi-blank (syntax-property (datum->syntax #f 'multi) 'raw ""))
(define group-blank (syntax-property (datum->syntax #f 'group) 'raw ""))
(define block-blank (syntax-property (datum->syntax #f 'block) 'raw ""))

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
         #`(#,(syntax-property (syntax/loc loc multi) 'raw ""))]
        [else #`(#,multi-blank)])
      #`(#,multi-blank (#,group-blank . #,tail))))

;; assumes that `packed-tail` represents a shrubbery,
;; and unpacks by removing `multi` ... `group` wrapper
(define (unpack-tail r who)
  (datum->syntax
   #f
   (cond
     [(multi-syntax? r)
      (define l (syntax->list r))
      (cond
        [(null? (cdr l)) '()]
        [(= 2 (length l)) (cdr (syntax-e (cadr l)))]
        [else (raise-error who "multi-group syntax not allowed in group context" r)])]
     [(group-syntax? r) (cdr (syntax-e r))]
     [(or (null? r) (pair? r)) (cannot-coerce-list who r)]
     [else (list r)])))

(define (pack-term stx)
  stx
  ;; This should work, too, and it can help make sure packing
  ;; and unpacking is done consistently everywhere:
  (datum->syntax #f `(,multi-blank (,group-blank ,stx))))

(define (unpack-term form who)
  (define (fail)
    (raise-error who "multi-term syntax not allowed in term context" form))
  (let loop ([r form])
    (cond
      [(multi-syntax? r)
       (define l (syntax->list r))
       (if (= 2 (length l))
           (loop (cadr l))
           (fail))]
      [(group-syntax? r)
       (define l (syntax->list r))
       (if (= 2 (length l))
           (cadr l)
           (fail))]
      [(list? r) (cannot-coerce-list who r)]
      [else r])))

(define (pack-group r)
  (datum->syntax #f (group-blank r)))

(define (unpack-group r who)
  (cond
    [(multi-syntax? r)
     (define l (syntax->list r))
     (if (= 2 (length l))
         (syntax-e (cadr l))
         (raise-error who "multi-group syntax not allowed in group context" r))]
    [(group-syntax? r) r]
    [(or (null? r) (pair? r)) (cannot-coerce-list who r)]
    [else #`(#,group-blank #,r)]))

;; ----------------------------------------

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

;; For syntax patterns, grou pwrapper applied externally:
(define (pack-group* stx depth)
  (pack* stx depth (lambda (stx) stx)))

(define (unpack-group* qs r depth)
  (unpack* qs r depth
           (lambda (r)
             (unpack-group r (syntax-e qs)))))

(define (pack-block* stxes depth)
  (pack* stxes depth (lambda (stxes)
                       (datum->syntax #f (cons multi-blank stxes)))))

(define (unpack-block* qs r depth)
  (unpack* qs r depth
           (lambda (r)
             (cons
              block-blank
              (cond
                [(multi-syntax? r) (cdr (syntax->list r))]
                [(group-syntax? r) (list r)]
                [(or (null? r) (pair? r)) (cannot-coerce-list qs r)]
                [else (list (list group-blank r))])))))

(define (pack-tail* stxes depth)
  (pack* stxes depth pack-tail))

(define (unpack-tail* qs r depth)
  (pack* r depth (lambda (r)
                   (unpack-tail r (syntax-e qs)))))

;; normalize for pattern matching:
(define (repack-group-or-term r)
  (cond
    [(group-syntax? r) (list multi-blank r)]
    [(multi-syntax? r) r]
    [else (list multi-blank (list group-blank r))]))

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
