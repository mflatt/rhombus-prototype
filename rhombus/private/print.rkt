#lang racket/base
(require racket/symbol
         racket/keyword
         shrubbery/write
         shrubbery/private/simple-pretty
         "provide.rkt"
         (submod "set.rkt" for-ref)
         "adjust-name.rkt"
         "printer-property.rkt"
         "define-arity.rkt"
         "function-arity-key.rkt"
         "static-info.rkt"
         "expression.rkt"
         "mutability.rkt"
         "realm.rkt"
         "print-desc.rkt")

(provide (for-spaces (#f
                      rhombus/statinfo)
                     (rename-out
                      [rhombus-print print])
                     println
                     (rename-out
                      [current-input-port current_input_port]
                      [current-output-port current_output_port]
                      [current-error-port current_error_port])))

(module+ redirect
  (provide (struct-out racket-print-redirect)))

(module+ for-class
  (provide prop:print-field-shapes))

(module+ for-string
  (provide (rename-out [do-print print]
                       [do-display display])))

(module+ for-runtime
  (provide (rename-out [do-print print])))

(define-values (prop:print-field-shapes print-field-shapes? print-field-shapes-ref)
  (make-struct-type-property 'print-field-shapes))

(define/arity #:name print (rhombus-print v [op (current-output-port)]
                                          #:as_expr [as-expr? #t])
  (do-print v op (if as-expr? 'expr 'text)))

(define/arity (println v [op (current-output-port)]
                       #:as_expr [as-expr? #t])
  (do-print v op (if as-expr? 'expr 'text))
  (newline op))

(define (do-display v op)
  (do-print v op 'text))

;; Fast path for simple printing: either call `display`, `write`,
;; of `other` once, or return results of multiple calls through `combine`
(define (maybe-print-immediate v display write combine other mode op)
  (define (display?) (eq? mode 'text))
  (cond
    [(flonum? v)
     (cond
       [(eqv? v +inf.0) (display "#inf" op)]
       [(eqv? v -inf.0) (display "#neginf" op)]
       [(eqv? v +nan.0) (display "#nan" op)]
       [else (write v op)])]
    [(or (and (string? v) (immutable? v))
         (and (bytes? v) (immutable? v))
         (exact-integer? v))
     (cond
       [(display?) (display v op)]
       [else (write v op)])]
    [(exact-integer? v)
     (write v op)]
    [(boolean? v)
     (display (if v "#true" "#false") op)]
    [(void? v)
     (display "#void" op)]
    [(path? v)
     (cond
       [(display?)
        (display v op)]
       [else
        (write v op)])]
    [(procedure? v)
     (define name (adjust-procedure-name (object-name v) (procedure-realm v)))
     (cond
       [name
        (combine
         (display "#<function:" op)
         (display name op)
         (display ">" op))]
       [else
        (display "#<function>" op)])]
    [(symbol? v)
     (cond
       [(display?)
        (display (symbol->immutable-string v) op)]
       [else
        (combine
         (display "#'" op)
         (write-shrubbery* v display write op))])]
    [(keyword? v)
     (cond
       [(display?)
        (display (keyword->immutable-string v) op)]
       [else
        (combine
         (display "#'" op)
         (write-shrubbery* v display write op))])]
    [(and (identifier? v)
          (display?))
     (display (syntax->datum v) op)]
    [else (other v mode op)]))

(define (write-shrubbery* v use-display use-write op)
  (cond
    [(and (eq? use-display display)
          (eq? use-write write))
     (write-shrubbery v op)]
    [else
     (define s-op (open-output-string))
     (write-shrubbery v s-op)
     (use-display (get-output-string s-op) op)]))

(define (do-print v op [mode 'expr])
  (maybe-print-immediate v display write void print-other mode op))

(define (print-other v mode op)
  (define desc (pretty v mode))
  (render-pretty desc op))

(define (pretty v mode)
  (maybe-print-immediate v pretty-as-display pretty-as-write pretty-combine pretty-other mode #f))

(define (pretty-as-display v op)
  (cond
    [(or (string? v) (bytes? v)) v]
    [else (format "~a" v)]))

(define (pretty-as-write v op)
  (format "~s" v))

(define (pretty-combine . docs)
  (pretty-combine-list docs))

(define (pretty-combine-list docs)
  (cons 'seq docs))

(define (pretty-listlike pre elems post)
  (pretty-combine pre
                  (pretty-combine-list
                   (let loop ([elems elems])
                     (cond
                       [(null? elems) null]
                       [(null? (cdr elems)) elems]
                       [else (list* (car elems) ", " (loop (cdr elems)))])))
                  post))

(define (pretty-other v mode op)
  (define (display?) (eq? mode 'text))
  (define (print v) (pretty v 'expr))
  (cond
    [(printer-ref v #f)
     => (lambda (printer)
          (printer v mode (lambda (v [mode 'expr])
                            (unless (or (eq? mode 'expr)
                                        (eq? mode 'text))
                              (raise-argument-error* 'print_recur rhombus-realm "#'expr || #'text" mode))
                            (Printable.Description (pretty v mode)))))]
    [(struct? v)
     (define vec (struct->vector v))
     (pretty-listlike
      (pretty-combine
       (pretty-as-write (if (srcloc? v)
                            'Srcloc
                            (object-name v))
                        op)
       (pretty-as-display "(" op))
      (cond
        [(print-field-shapes-ref v #f)
         => (lambda (shapes)
              (cond
                [(eq? shapes 'opaque)
                 (pretty-as-display "..." op)]
                [else
                 (for/list ([i (in-range 1 (vector-length vec))]
                            [s (in-list shapes)]
                            #:when s)
                   (if (keyword? s)
                       (pretty-combine
                        (pretty-as-display (string-append "~" (keyword->immutable-string s) ": ") op)
                        (print (vector-ref vec i)))
                       (print (vector-ref vec i))))]))]
        [else
         (for/list ([i (in-range 1 (vector-length vec))])
           (print (vector-ref vec i)))])
      (pretty-as-display ")" op))]
    [(list? v)
     (pretty-listlike
      (pretty-as-display "[" op)
      (for/list ([e (in-list v)])
        (print e))
      (pretty-as-display "]" op))]
    [(pair? v)
     (pretty-listlike
      (pretty-as-display "cons(" op)
      (list
       (print (car v))
       (print (cdr v)))
      (pretty-as-display ")" op))]
    [(vector? v)
     (pretty-listlike
      (pretty-as-display "Array(" op)
      (for/list ([e (in-vector v)])
        (print e))
      (pretty-as-display ")" op))]
    [(hash? v)
     (pretty-listlike
      (pretty-as-display (if (mutable-hash? v)
                             "MutableMap{"
                             "{")
                         op)
      (for/list ([k+v (hash-map v cons #t)])
        (define k (car k+v))
        (define v (cdr k+v))
        (pretty-combine
         (print k)
         (pretty-as-display ": " op)
         (print v)))
      (pretty-as-display "}" op))]
    [(set? v)
     (cond
       [(eqv? 0 (hash-count (set-ht v)))
        (if (mutable-hash? (set-ht v))
            (pretty-as-display "MutableSet{}" op)
            (pretty-as-display "Set{}" op))]
       [else
        (pretty-listlike
         (pretty-as-display (if (mutable-hash? (set-ht v))
                                "MutableSet{"
                                "{")
                            op)
         (for/list ([v (in-list (hash-map (set-ht v) (lambda (k v) k) #t))])
           (print v))
         (pretty-as-display "}" op))])]
    [(syntax? v)
     (define s (syntax->datum v))
     (define qs
       (cond
         [(and (pair? s) (eq? 'multi (car s)))
          (cons (if (display?) 'top 'quotes)
                (cdr s))]
         [(and (pair? s) (eq? 'group (car s)))
          (if (display?)
              s
              (list 'quotes s))]
         [else (if (display?)
                   s
                   (list 'quotes (list 'group s)))]))
     (pretty-shrubbery qs #:armor? #t)]
    [else
     (cond
       [(display?)
        (pretty-as-display v op)]
       [else
        (define rop (open-output-bytes))
        (display "#{'" rop)
        (racket-print (racket-print-redirect v) rop 1)
        (display "}" rop)
        (pretty-as-display (get-output-bytes rop) op)])]))

(define (racket-print v op mode)
  (print v op mode))

(struct racket-print-redirect (val)
  #:property prop:custom-write
  (lambda (r op mode)
    (racket-print (racket-print-redirect-val r) op mode)))

(define-static-info-syntaxes (current-input-port current-output-port current-error-port)
  (#%function-arity 3))
