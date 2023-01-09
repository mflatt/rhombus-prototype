#lang racket/base
(require (for-syntax racket/base))

(provide (for-syntax summarize-arity
                     union-arity-summaries
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

(define-for-syntax (union-arity-summaries as)
  (cond
    [(null? as) #f]
    [(= 1 (length as)) (car as)]
    [else
     (define (list->set l) (for/hasheq ([v (in-list l)]) (values v #t)))
     (define (set->list s) (sort (for/list ([v (in-hash-keys s)]) v) keyword<?))
     (define (set-intersect a b) (for/hasheq ([k (in-hash-keys b)]
                                              #:when (hash-ref a k #f))
                                   (values b #t)))
     (define (set-union a b) (for/fold ([a a]) ([k (in-hash-keys b)])
                               (hash-set a k #t)))
     (define (normalize a)
       (if (pair? a)
           (list (car a) (list->set (cadr a)) (and (caddr a) (list->set (caddr a))))
           (list a #hasheq() #hasheq())))
     (define norm-a
       (for/fold ([new-a (normalize (car as))]) ([a (in-list (cdr as))])
         (let ([a (normalize a)])
           (list (bitwise-ior (car new-a) (car a))
                 (set-intersect (cadr new-a) (cadr a))
                 (and (caddr new-a) (caddr a) (set-union (caddr new-a) (caddr a)))))))
     (define required-kws (set->list (cadr norm-a)))
     (define allowed-kws (and (caddr norm-a) (set->list (caddr norm-a))))
     (if (and (null? required-kws)
              (null? allowed-kws))
         (car norm-a)
         (list (car norm-a) required-kws allowed-kws))]))

(define-for-syntax (check-arity head a n kws rsts kwrsts)
  (let loop ([kws kws] [n n] [needed-kws #f] [allowed-kws #f])
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
       (loop (cdr kws) n needed allowed)]
      [else
       (loop (cdr kws) (add1 n) needed-kws allowed-kws)])))
