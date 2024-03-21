#lang racket
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse)
         "space-provide.rkt"
         "definition.rkt"
         "mutability.rkt"
         "map-by.rkt"
         "parens.rkt"
         (submod "map.rkt" for-map-by-macro)
         (submod "set.rkt" for-map-by-macro)
         "realm.rkt"
         "parse.rkt")

;; This implementation does not work, because immutable hash tables
;; cannot be impersonated. But it's an idea about how we might support
;; custom hashing through an impersonator-like mechanism.

(define+provide-space map_by rhombus/map_by
  #:fields
  (def))

(define-defn-syntax def
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (group)
        [(_ (_::quotes (group name::name))
            (body-tag::block
             (~and
              (~seq (group kw clause-block) ...)
              (~seq
               (~alt (~optional (group #:equals
                                       (equals-tag::block
                                        equals-body ...)))
                     (~optional (group #:hash_code
                                       (hash-code-tag::block
                                        hash-code-body ...))))
               ...))))
         (unless (attribute equals-tag)
           (raise-syntax-error #f "missing a `~equals` clause" stx))
         (unless (attribute hash-code-tag)
           (raise-syntax-error #f "missing an `~hash_code` clause" stx))
         #`((define-values (x-equals? x-hash-code)
              (hash-procedures (~@ kw (rhombus-body-expression clause-block)) ...))
            ;; keys are wrapped in this struct, which lets use own own
            ;; hash function for the keys
            (struct x (v)
              #:property prop:equal+hash (list (lambda (a b recur mode)
                                                 (x-equals? (x-v a) (x-v b) recur))
                                               (lambda (a recur mode)
                                                 (x-hash-code (x-v a) recur))))
            ;; to recognize our impersonated tables:
            (define-values (prop:x x-map? x-map-ref)
              (make-impersonator-property 'name.name))
            (define (wrap ht)
              (impersonate-hash ht
                                (lambda (ht key)
                                  (values (x key)
                                          (lambda (ht key val) val)))
                                (lambda (ht key val)
                                  (values (x key) val))
                                (lambda (ht key)
                                  (x key))
                                (lambda (ht key)
                                  (x-v key))
                                (lambda (ht) (void))
                                prop:x #t))
            (define empty-x-map (wrap #hasheqv()))
            (define (x-map-build . args)
              (build-map 'x-map-build (args->pairs 'x-map-build args) empty-x-map))
            (define (x-map-pair-build pairs)
              (build-map 'x-map-build pairs empty-x-map))
            (define (list->x-map pairs)
              (build-map 'list->x-map pairs empty-x-map))
            (define (mutable-x-map? v)
              (and (mutable-hash? v) (x-map? v)))
            (define (x-mutable-map-build . args)
              (define ht (wrap (make-hash)))
              (for ([p (in-list (args->pairs 'x-mutable-map-build args))])
                (hash-set! ht (car p) (cdr p)))
              ht)
            (define (immutable-x-set? v)
              (and (set? v) (immutable-hash? (set-ht v)) (x-map? (set-ht v))))
            (define (x-set-build . args)
              (list->x-set args))
            (define (list->x-set args)
              (x-map-set-build args empty-x-map))
            (define (mutable-x-set? v)
              (and (set? v) (mutable-hash? (set-ht v)) (x-map? (set-ht v))))
            (define-map-by-syntax name.name
              (map-by 'name.name #'x-map?
                      #'x-map-build #'x-map-pair-build #'list->x-map
                      #'mutable-x-map? #'x-mutable-map-build
                      #'empty-x-map
                      #'immutable-x-set?
                      #'x-set-build #'x-set-build #'list->x-set
                      #'mutable-x-set?)))]))))

(define (hash-procedures #:equals equals #:hash_code hash_code)
  (unless (and (procedure? equals) (procedure-arity-includes? equals 3))
    (raise-argument-error* 'map_by.def rhombus-realm "Function.of_arity(3)" equals))
  (unless (and (procedure? hash_code) (procedure-arity-includes? hash_code 2))
    (raise-argument-error* 'map_by.def rhombus-realm "Function.of_arity(2)" hash_code))
  (values equals hash_code))

(define (x-map-set-build elems ht)
  (set
   (for/fold ([ht ht]) ([e (in-list elems)])
     (hash-set ht e #t))))

(define (args->pairs who orig-args)
  (let loop ([args orig-args])
    (cond
      [(null? args) null]
      [(null? (cdr args))
       (raise-arguments-error who "expected an even number of arguments")]
      [else (cons (list (car args) (cadr args)) (loop (cddr args)))])))
