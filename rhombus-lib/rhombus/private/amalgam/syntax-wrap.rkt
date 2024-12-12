#lang racket/base

(provide (struct-out syntax-wrap)
         syntax-unwrap
         syntax*?)

(struct syntax-wrap (stx key attribs))

(define (syntax-unwrap s)
  (if (syntax-wrap? s)
      (syntax-wrap-stx s)
      s))

(define (syntax*? v)
  (or (syntax? v)
      (syntax-wrap? v)))
