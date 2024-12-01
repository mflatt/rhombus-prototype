#lang racket/base
(require "private/bounce.rkt"
         "private/version-case.rkt")

(require (only-space-in rhombus/impo
                        (only-in (submod "private/amalgam.rkt" core)
                                 !))
         (only-space-in rhombus/modpath
                        (only-in (submod "private/amalgam.rkt" core)
                                 !))
         (only-space-in rhombus/annot
                        (only-in (submod "private/amalgam.rkt" core)
                                 !))
         (only-space-in rhombus/unquote_bind
                        (only-in (submod "private/amalgam.rkt" core)
                                 !))
         (rename-in (submod "private/amalgam.rkt" core-derived)
                    [?! !]))

(bounce #:except (!) (submod "private/amalgam.rkt" core))
(bounce (submod "private/amalgam.rkt" core-macro))
(bounce #:except (?!) (submod "private/amalgam.rkt" core-derived))

(meta-if-version-at-least
 "8.13.0.4"
 (#%declare #:flatten-requires)
 (void))

(module reader syntax/module-reader
  #:language 'rhombus
  #:read (lambda (in) (list (syntax->datum (parse-all in))))
  #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
  #:info rhombus:get-info-proc
  #:whole-body-readers? #t
  (require shrubbery/parse
           (prefix-in rhombus: (submod "private/core.rkt" reader))))

(module configure-runtime racket/base
  (require rhombus/runtime-config))

(module configure-expand racket/base
  (require rhombus/expand-config)
  (provide enter-parameterization
           exit-parameterization))
