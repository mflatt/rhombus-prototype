#lang racket/base
(require "realm.rkt")

(provide pack-list*
         unpack-list*
         pack-term*
         unpack-term*
         pack-group*
         unpack-group*
         unpack-single-term-group
         convert-single-term-group
         pack-block*
         unpack-block*)

(define (pack* stx depth wrap)
  (cond
    [(eqv? depth 0) (wrap stx)]
    [else (for/list ([t (in-list (syntax->list stx))])
            (pack* t (sub1 depth) wrap))]))

(define (unpack* qs r depth unwrap)
  (datum->syntax
   qs
   (let unpack-list* ([r r] [depth depth])
     (cond
       [(eqv? depth 0)
        (unwrap r)]
       [else
        (if (list? r)
            (datum->syntax
             qs
             (for/list ([r (in-list r)])
               (unpack-list* r (sub1 depth))))
            (raise-argument-error* '... rhombus-realm "List" r))]))))

(define (pack-list* stx depth)
  (pack* stx depth values))

(define (unpack-list* qs r depth)
  (unpack* qs r depth values))
  
(define (pack-term* stx depth)
  (pack* stx depth (lambda (stx)
                     #`(parens (group #,stx)))))

(define (unpack-term* qs r depth)
  (unpack* qs r depth
           (lambda (r)
             (when (or (null? r) (pair? r))
               (raise-arguments-error* '|$| rhombus-realm
                                       "cannot coerce list to syntax"
                                       "list" r))
             (cond
               [(and qs (group-syntax? r))
                (define l (syntax->list r))
                (if (= 2 (length l))
                    (cadr l)
                    (raise-arguments-error* '|$| rhombus-realm
                                            "multi-term group syntax not allowed in term context"
                                            "group syntax" r))]
               [else
                

                r]))))

(define (unpack-single-term-group r)
  (or (convert-single-term-group r)
      r))

(define (convert-single-term-group r)
  (cond
    [(group-syntax? r)
     (define l (syntax->list r))
     (and (= 2 (length l))
          #`(parens #,r))]
    [else r]))
  
(define (pack-group* stx depth)
  (pack-list* stx depth))

(define (unpack-group* qs r depth)
  (datum->syntax
   qs
   (let unpack-group* ([r r] [depth depth])
     (cond
       [(eqv? depth 0)
        (if (group-syntax? r)
            r
            (list 'group r))]
       [else
        (if (list? r)
            (datum->syntax
             qs
             (for/list ([r (in-list r)])
               (unpack-group* r (sub1 depth))))
            (raise-argument-error* '... rhombus-realm "List" r))]))))

(define (group-syntax? r)
  (and (syntax? r)
       (pair? (syntax-e r))
       (eq? 'group (syntax-e (car (syntax-e r))))))

(define (pack-block* stx depth)
  (pack-term* stx depth))

(define (unpack-block* qs r depth)
  (datum->syntax
   qs
   (let unpack-block* ([r r] [depth depth])
     (cond
       [(eqv? depth 0)
        (cond
          [(block-syntax? r) r]
          [(group-syntax? r) (list 'block r)]
          [else (list 'block (list 'group r))])]
       [else
        (if (list? r)
            (datum->syntax
             qs
             (for/list ([r (in-list r)])
               (unpack-block* r (sub1 depth))))
            (raise-argument-error* '... rhombus-realm "List" r))]))))

(define (block-syntax? r)
  (and (syntax? r)
       (pair? (syntax-e r))
       (eq? 'block (syntax-e (car (syntax-e r))))))
