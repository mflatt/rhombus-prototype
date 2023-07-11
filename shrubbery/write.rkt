#lang racket/base
(require racket/keyword
         racket/symbol)

;; Writing a shubbery represented as an S-expression.

(provide write-shrubbery
         pretty-shrubbery)

(define rx:identifier #px"^(?:\\p{L}|_)(?:\\p{L}|\\p{N}|_)*$")

(define (write-shrubbery v [op (current-output-port)])
  (do-write-shrubbery v op))

(define (pretty-shrubbery v)
  (do-write-shrubbery v 'doc))

(define (do-write-shrubbery v op)
  (cond
    [(and (pair? v) (eq? 'group (car v)))
     ;; printing a raw group
     (cond
       [(eq? op 'doc)
        (define-values (inside inside-multi) (do-write-shrubbery-term v op)) 
        `(or (seq "«" (nest 2 (seq ,inside)) "»")
             (seq "«"
                  (nest 2 (seq nl ,inside-multi))
                  nl "»"))]
       [else
        (display "«" op)
        (do-write-shrubbery-term v op)
        (display "»" op)])]
    [else
     (cond
       [(eq? op 'doc)
        (define-values (inside inside-multi) (do-write-shrubbery-term v op))
        `(or ,inside ,inside-multi)]
       [else
        (do-write-shrubbery-term v op)])]))

(define (do-write-shrubbery-term v op)
  (let loop ([v v])
    (cond
      [(list? v)
       (cond
         [(null? v)
          #;(error 'write-shubbery "unexpected ~s" v)
          (display* "#{()}" op)]
         [(eq? 'op (car v))
          (display* (cadr v) op)]
         [(eq? 'alts (car v))
          (cond
            [(eq? op 'doc)
             (define-values (insides insides-multi)
               (for/lists (insides insides-multi) ([v (in-list (cdr v))])
                 (unless (and (pair? v) (eq? (car v) 'block))
                   (error 'write-shubbery "unexpected ~s" v))
                 (define-values (sub-insides sub-insides-multi)
                   (for/lists (sub-insides sub-insides-multi) ([v (in-list (cdr v))])
                     (loop v)))
                 (values `(seq ,@(add-between sub-insides "; "))
                         `(nest 2 (seq nl ,@(add-between sub-insides-multi '(seq ";" nl)))))))
             (values `(seq "|« " ,@(add-between insides " » |« ") " »")
                     `(seq nl (align (seq "|«" ,@(add-between insides-multi `(seq nl "»" nl "|«")) nl "»"))))]
            [else
             (for/fold ([first? #t]) ([v (in-list (cdr v))])
               (unless first? (display " " op))
               (display "|« " op)
               (unless (and (pair? v) (eq? (car v) 'block))
                 (error 'write-shubbery "unexpected ~s" v))
               (for/fold ([first? #t]) ([v (in-list (cdr v))])
                 (unless first? (display "; " op))
                 (loop v)
                 #f)
               (display " »" op)
               #f)
             (void)])]
         [(eq? 'top (car v))
          (cond
            [(eq? op 'doc)
             (define-values (insides insides-multi)
               (for/lists  (insides insides-multi) ([v (in-list (cdr v))])
                 (loop v)))
             (values `(seq ,@(add-between insides "; "))
                     `(seq ";«" (nest 2 (seq nl ,@(add-between insides-multi '(seq ";" nl)))) nl "»"))]
            [else
             (for/fold ([first? #t]) ([v (in-list (cdr v))])
               (unless first? (display "; " op))
               (loop v)
               #f)
             (void)])]
         [else
          (define-values (align? open line-open sep sep+space block-sep+space line-close close one-line?)
            (case (car v)
              [(group) (values #t "" "" "" " " "" "" "" #t)]
              [(block) (values #f ":«" ":« " ";" "; " "; " " »" "»" #f)]
              [(parens) (values #f "(" "(" "," ", " ", " ")" ")" #f)]
              [(brackets) (values #f "[" "[" "," ", " ", " "]" "]" #f)]
              [(braces) (values #f "{" "{" "," ", " ", " "}" "}" #f)]
              [(quotes) (values #f "'«" "'«" ";" "; " "; " "»'" "»'" #f)]
              [else (values #f #f #f #f #f #f #f #f #f)]))
          (cond
            [open
             (cond
               [(eq? op 'doc)
                (define-values (insides insides-semi insides-multi)
                  ;; add separators here, since we need to treat block items
                  ;; specially for 'group mode
                  (let inside-loop ([l (cdr v)])
                    (cond
                      [(null? l) (values null null null)]
                      [(null? (cdr l))
                       (define-values (inside inside-multi) (loop (car l)))
                       (values (list inside)
                               (list inside-multi)
                               (list `(nest 2 (seq nl ,inside-multi))))]
                      [else
                       (define v (car l))
                       (define next-v (cadr l))
                       (define-values (inside inside-multi) (loop v))
                       (define-values (insides insides-semi insides-multi) (inside-loop (cdr l)))
                       (define this-sep+space (if (and (pair? next-v) (eq? (car next-v) 'block))
                                                  block-sep+space
                                                  sep+space))
                       (values (list* inside this-sep+space insides)
                               (let ([semi (list* inside this-sep+space insides-semi)])
                                 (cond
                                   [(and (null? (cddr l))
                                         (pair? (car l))
                                         (eq? (car (car l)) 'block))
                                    ;; must be a block with alts after
                                    (list `(or (seq ,semi)
                                               (seq ,inside-multi
                                                    ,@insides-semi)))]
                                   [else semi]))
                               (cons `(nest 2 (seq nl ,inside-multi ,sep))
                                     insides-multi))])))
                (define line
                  `(seq ,line-open ,@insides ,line-close))
                (cond
                  [(null? (cdr v)) (values line line)]
                  [else
                   (values line
                           (let ([multi
                                  (cond
                                    [one-line? `(seq ,line-open ,@insides-semi ,line-close)]
                                    [else `(seq ,open ,@insides-multi nl ,close)])])
                             `(or ,line
                                  ,(if align? `(align ,multi) multi))))])]
               [else
                (display line-open op)
                (for/fold ([first? #t]) ([v (in-list (cdr v))])
                  (unless (or first?
                              (and (pair? v) (eq? (car v) 'block)))
                    (display sep+space op))
                  (loop v)
                  #f)
                (display line-close op)])]
            [else
             (write-escaped v op)])])]
      [(symbol? v)
       (define s (symbol->immutable-string v))
       (cond
         [(regexp-match? rx:identifier s)
          (display* s op)]
         [else
          (write-escaped v op)])]
      [(keyword? v)
       (define s (keyword->immutable-string v))
       (cond
         [(regexp-match? rx:identifier s)
          (cond
            [(eq? op 'doc)
             (twice (string-append-immutable "~" s))]
            [else
             (display "~" op)
             (display s op)])]
         [else
          (write-escaped v op)])]
      [(or (string? v)
           (bytes? v)
           (exact-integer? v))
       (write* v op)]
      [(flonum? v)
       (cond
         [(eqv? v +inf.0) (display* "#inf" op)]
         [(eqv? v -inf.0) (display* "#neginf" op)]
         [(eqv? v +nan.0) (display* "#nan" op)]
         [else (write* v op)])]
      [(boolean? v)
       (display* (if v "#true" "#false") op)]
      [(void? v)
       (display* "#void" op)]
      [else
       (write-escaped v op)])))

(define (display* v op)
  (if (eq? op 'doc)
      (twice (if (or (string? v)
                     (bytes? v))
                 v
                 (format "~a" v)))
      (display v op)))

(define (write* v op)
  (if (eq? op 'doc)
      (twice (format "~s" v))
      (write v op)))

(define (write-escaped v op)
  (cond
    [(eq? op 'doc)
     (twice (format "#{~s}" v))]
    [else
     (display "#{" op)
     (write v op)
     (display "}" op)]))

(define (twice v)
  (values v v))

(define (add-between l v)
  (cond
    [(null? l) l]
    [(null? (cdr l)) l]
    [else (list* (car l) v (add-between (cdr l) v))]))
