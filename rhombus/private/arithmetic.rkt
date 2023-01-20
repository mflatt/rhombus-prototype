#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "provide.rkt"
         "expression.rkt"
         "repetition.rkt"
         "define-operator.rkt"
         "function-arity-key.rkt"
         "static-info.rkt"
         (only-in "dot.rkt"
                  |.|)
         (only-in "repetition.rkt"
                  expression+repetition-prefix+infix-operator))

(provide (for-spaces (rhombus/expr
                      rhombus/repet)

                     (rename-out [rhombus+ +]
                                 [rhombus- -]
                                 [rhombus* *]
                                 [rhombus/ /]
                                 [rhombus< <]
                                 [rhombus<= <=]
                                 [rhombus>= >=]
                                 [rhombus> >])
                     .=

                     !
                     &&
                     \|\|

                     ==
                     !=

                     ===)

         (for-spaces (rhombus/expr
                      rhombus/statinfo)
                     sqrt cos sin tan log exp expt acos asin atan
                     floor ceiling round))

(define-infix rhombus+ +
  #:weaker-than (rhombus* rhombus/)
  #:same-as (rhombus-))

(define-for-syntax minus-operator
  (expression+repetition-prefix+infix-operator
   (prefix rhombus- - #:weaker-than (rhombus* rhombus/))
   (infix rhombus- - #:weaker-than (rhombus* rhombus/))))

(define-expression-syntax rhombus- minus-operator)
(define-repetition-syntax rhombus- minus-operator)

(define-infix rhombus* *
  #:same-on-left-as (rhombus/))

(define-infix rhombus/ /)

(define-prefix ! not
  #:stronger-than (&& \|\|))

(define-infix && and
  #:weaker-than (rhombus+ rhombus- rhombus* rhombus/)
  #:stronger-than (\|\|))

(define-infix \|\| or
  #:weaker-than (rhombus+ rhombus- rhombus* rhombus/))

(define-syntax-rule (define-comp-infix name racket-name)
  (define-infix name racket-name
    #:weaker-than (rhombus+ rhombus- rhombus* rhombus/)
    #:same-as (rhombus> rhombus>= .= rhombus<=)
    #:stronger-than (\|\| &&)
    #:associate 'none))

(define-comp-infix rhombus< <)
(define-comp-infix rhombus<= <=)
(define-comp-infix .= =)
(define-comp-infix rhombus>= >=)
(define-comp-infix rhombus> >)

(define-syntax-rule (define-eql-infix name racket-name)
  (define-infix name racket-name
    #:weaker-than (rhombus+ rhombus- rhombus* rhombus/ |.|)
    #:stronger-than (\|\| &&)
    #:associate 'none))

(define-eql-infix == equal-always?)
(define-eql-infix != not-equal-always?)
(define-eql-infix === eq?)

(define (not-equal-always? a b) (not (equal-always? a b)))

(define-syntax (bounce-functions stx)
  (syntax-parse stx
    [(_ id ...)
     #`(begin
         #,@(for/list ([id (in-list (syntax->list #'(id ...)))])
              #`(define #,(in-expression-space id) #,id)))]))
      
(bounce-functions sqrt cos sin tan log exp expt acos asin atan
                 floor ceiling round)

(define-static-info-syntaxes (sqrt cos sin tan exp acos asin
                                   floor ceiling round)
  (#%function-arity 2))

(define-static-info-syntaxes (expt)
  (#%function-arity 4))

(define-static-info-syntaxes (log atan)
  (#%function-arity 6))

