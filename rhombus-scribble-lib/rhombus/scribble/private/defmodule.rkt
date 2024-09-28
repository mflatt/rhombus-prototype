#lang racket/base
(require rhombus/private/version-case)

(provide defmodule)

(meta-if-version-at-least
 "8.14.0.5" ; assuming implies "scribble-lib" version 1.54
 (require (only-in scribble/manual defmodule))
 (begin
   (require (rename-in scribble/manual
                       [defmodule scribble:defmodule])
            (for-syntax racket/base
                        syntax/parse/pre))
   (define-syntax (defmodule stx)
     (syntax-parse stx
       [(_ pre ... #:language-family fam post ...)
        #'(scribble:defmodule pre ... post ...)]))))
