#lang racket/base
(require "private/property.rkt")

;; Printing syntax object using raw-text properties

(provide shrubbery-syntax->string)

(module+ for-parse
  (provide syntax-to-raw))

;; Expects `s` to be a shrubbery, but accomodates other shapes
(define (shrubbery-syntax->string s
                                  #:use-raw? [use-raw? #f]
                                  #:max-length [max-length #f]
                                  #:keep-prefix? [keep-prefix? #f]
                                  #:keep-suffix? [keep-suffix? #f]
                                  #:infer-starting-indentation? [infer-starting-indentation? #t]
                                  #:register-stx-range [register-stx-range void]
                                  #:render-stx-hook [render-stx-hook (lambda (stx output) #f)])
  (cond
    [(or use-raw?
         (and (syntax? s) (all-raw-available? s)))
     (define o (open-output-string))
     (port-count-lines! o)
     (syntax-to-raw (datum->syntax #f s)
                    #:output o
                    #:max-length max-length
                    #:keep-prefix? keep-prefix?
                    #:keep-suffix? keep-suffix?
                    #:register-stx-range register-stx-range
                    #:render-stx-hook render-stx-hook)
     (define orig-str (get-output-string o))
     (define starting-col (and infer-starting-indentation?
                               (extract-starting-column s)))
     ;; strip `string-col` spaces from the start of lines after the first one:
     (define str (if infer-starting-indentation?
                     (regexp-replace* (string-append "\n" (make-string starting-col #\space))
                                      orig-str
                                      "\n")
                     orig-str))
     (if (and max-length
              ((string-length str) . > . max-length))
         (string-append (substring str 0 (max 0 (- max-length 3)))
                        "...")
         str)]
    [else
     (define v (if (syntax? s) (syntax->datum s) s))
     (if max-length
         (parameterize ([error-print-width max-length])
           (format "~.s" v))
         (format "~s" v))]))

(define (to-output raw output max-length)
  (define (full?)
    (and max-length
         ((file-position output) . > . max-length)))
  (let loop ([l raw])
    (cond
      [(pair? l)
       (unless (full?)
         (loop (car l))
         (unless (full?)
           (loop (cdr l))))]
      [(null? l) (void)]
      [(string? l) (display l output)]
      [else (void)])))

(define (syntax-to-raw g
                       #:output [output #f]
                       #:max-length [max-length #f]
                       #:keep-prefix? [keep-prefix? #f]
                       #:keep-suffix? [keep-suffix? #t]
                       #:register-stx-range [register-stx-range void]
                       #:render-stx-hook [render-stx-hook (lambda (stx output) #f)])
  (define (raw-cons a b) (if (and a (not (null? a)))
                             (if (and b (not (null? b)))
                                 (cons a b)
                                 a)
                             (or b null)))
  (let loop ([g g] [tail null] [use-prefix? keep-prefix?] [keep-suffix? keep-suffix?])
    (define (finish raw tail)
      (cond
        [output
         (to-output raw output max-length)
         #f]
        [else
         (raw-cons raw tail)]))
    (define (other g tail)
      (define raw (list "#{" (format "~s" (syntax->datum g)) "}"))
      (finish raw tail))
    (define (sequence l tail use-prefix? keep-suffix?)
      (let e-loop ([pre null] [l l] [use-prefix? use-prefix?] [keep-suffix? keep-suffix?])
        (cond
          [(null? l) tail]
          [(null? (cdr l))
           (cons pre
                 (loop (car l) tail use-prefix? keep-suffix?))]
          [else
           (e-loop (raw-cons pre
                             (loop (car l) tail use-prefix? #t))
                   (cdr l)
                   #t
                   keep-suffix?)])))
    (define (container a l tail bracketed? use-prefix? keep-suffix?)
      (define prefix (finish (raw-cons
                              (and use-prefix? (syntax-raw-prefix-property a))
                              (syntax-raw-property a))
                             #f))
      (define suffix (raw-cons (syntax-raw-tail-property a)
                               (and keep-suffix?
                                    (syntax-raw-suffix-property a))))
      (define mid
        (cond
          [(syntax-raw-opaque-content-property a)
           => (lambda (raw)
                (finish raw (raw-cons tail suffix)))]
          [else
           (sequence l (raw-cons tail suffix) (or bracketed? keep-prefix?) (or bracketed? keep-suffix?))]))
      (if output
          (finish suffix #f)
          (raw-cons prefix mid)))

    (define (file-location-position p)
      (define-values (line col pos) (port-next-location p))
      (- pos 1))
    (define start-pos (and register-stx-range
                           output
                           (file-location-position output)))

    (begin0
      (cond
        [(syntax-opaque-raw-property g)
         => (lambda (raw)
              (finish raw tail))]
        [(pair? (syntax-e g))
         (define l (syntax->list g))
         (cond
           [(not l) (other g tail)]
           [else
            (define a (car l))
            (case (syntax-e a)
              [(top multi)
               (container a (cdr l) tail #t use-prefix? keep-suffix?)]
              [(group)
               (container a (cdr l) tail #f use-prefix? keep-suffix?)]
              [(op)
               (if (and (pair? (cdr l)) (null? (cddr l)))
                   (loop (cadr l) tail use-prefix? keep-suffix?)
                   (other g tail))]
              [(parens brackets braces quotes block alts)
               (container a (cdr l) tail #t use-prefix? keep-suffix?)]
              [(parsed)
               (other g tail)]
              [else #f])])]
        [(syntax-raw-property g)
         => (lambda (raw)
              (container g null tail #f use-prefix? keep-suffix?))]
        [else
         (other g tail)])

      (when start-pos
        (register-stx-range g start-pos (file-location-position output))))))

(define (all-raw-available? s)
  (let loop ([s s])
    (or
     (cond
       ;; allow anything that has been specifically designated as raw and opaque:
       [(syntax-opaque-raw-property s)
        #t]
       ;; otherwise, traverse the shrubbery encoding
       [(pair? (syntax-e s))
        (define l (syntax->list s))
        (define (rest-available? l)
          (for/and ([e (in-list l)])
            (all-raw-available? e)))
        (cond
          [(not l) #f]
          [else
           (case (syntax-e (car l))
             [(top multi)
              (rest-available? (cdr l))]
             [(group)
              (or (syntax-opaque-raw-property (car l))
                  (rest-available? (cdr l)))]
             [(op)
              (and (pair? (cdr l))
                   (null? (cddr l))
                   (loop (cadr l)))]
             [(parens brackets braces quotes block alts)
              (or (syntax-opaque-raw-property (car l))
                  (and (syntax-raw-property (car l))
                       (rest-available? (cdr l))))]
             [(parsed)
              (= 3 (length l))]
             [else #f])])]
       [else (syntax-raw-property s)])
     #;
     (and (log-error "?? ~s" s)
          #f))))

(define (extract-starting-column s)
  (cond
    [(syntax? s)
     (or (syntax-column s)
         (let ([e (syntax-e s)])
           (and (pair? e)
                (extract-starting-column (car e))))
         0)]
    [else 0]))
