#lang racket/base
(require (for-syntax racket/base))

(provide (for-syntax summarize-arity
                     check-arity))

(define-for-syntax (summarize-arity kws defaults rest? kw-rest?)
  (let loop ([kws (syntax->list kws)]
             [defaults (syntax->list defaults)]
             [bit 1]
             [mask 0]
             [allowed-kws #hasheq()]
             [required-kws #hasheq()])
    (cond
      [(null? kws)
       (define a (bitwise-ior mask
                              (if rest?
                                  (bitwise-xor -1 (sub1 bit))
                                  bit)))
       (if kw-rest?
           `(,a ,(sort (hash-keys required-kws) keyword<?) #f)
           (if (zero? (hash-count allowed-kws))
               a
               `(,a ,(sort (hash-keys required-kws) keyword<?)
                    ,(sort (hash-keys allowed-kws) keyword<?))))]
      [(syntax-e (car kws))
       (loop (cdr kws)
             (cdr defaults)
             bit
             mask
             (hash-set allowed-kws (syntax-e (car kws)) #t)
             (if (syntax-e (car defaults))
                 required-kws
                 (hash-set required-kws (syntax-e (car kws)) #t)))]
      [else
       (loop (cdr kws)
             (cdr defaults)
             (arithmetic-shift bit 1)
             (if (syntax-e (car defaults))
                 (bitwise-ior mask bit)
                 mask)
             allowed-kws
             required-kws)])))

(define-for-syntax (check-arity head a kws args rsts kwrsts)
  (let loop ([kws kws] [args args] [n 0] [needed-kws #f] [allowed-kws #f])
    (cond
      [(null? kws)
       (unless rsts
         (when (zero? (bitwise-and (arithmetic-shift 1 n) (if (pair? a) (car a) a)))
           (raise-syntax-error #f
                               (string-append "wrong number of "
                                              (if needed-kws "by-position " "")
                                              "arguments in function call")
                               head)))
       (unless kwrsts
         (when (and needed-kws ((hash-count needed-kws) . > . 0))
           (raise-syntax-error #f
                               (string-append "missing keyword argument in function call\n"
                                              "  keyword: ~"
                                              (keyword->string (hash-iterate-key needed-kws (hash-iterate-first needed-kws))))
                               head)))]
      [(syntax-e (car kws))
       (define kw (syntax-e (car kws)))
       (define needed (hash-remove (or needed-kws
                                       (if (pair? a)
                                           (for/hasheq ([kw (in-list (cadr a))])
                                             (values kw #t))
                                           #hasheq()))
                                   kw))
       (define allowed (or allowed-kws
                           (if (pair? a)
                               (and (caddr a)
                                    (for/hasheq ([kw (in-list (caddr a))])
                                      (values kw #t)))
                               #hasheq())))
       (when (and allowed
                  (not (hash-ref allowed kw #f)))
         (raise-syntax-error #f
                             (string-append "keyword argument not recognized by called function\n"
                                            "  keyword: ~"
                                            (keyword->string kw))
                             head))
       (loop (cdr kws) (cdr args) n needed allowed)]
      [else
       (loop (cdr kws) (cdr args) (add1 n) needed-kws allowed-kws)])))
