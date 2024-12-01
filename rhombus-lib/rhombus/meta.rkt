#lang racket/base
(require "private/bounce.rkt"
         "private/version-case.rkt")

(require (only-space-in rhombus/impo
                        (only-in (submod "private/amalgam.rkt" core-meta)
                                 !))
         (only-space-in rhombus/modpath
                        (only-in (submod "private/amalgam.rkt" core-meta)
                                 !))
         (only-space-in rhombus/annot
                        (only-in (submod "private/amalgam.rkt" core-meta)
                                 !))
         (only-space-in rhombus/unquote_bind
                        (only-in (submod "private/amalgam.rkt" core-meta)
                                 !)))

(bounce #:except (!) (submod "private/amalgam.rkt" core-meta))
(bounce (submod "private/amalgam.rkt" core-meta sequence_meta))

(require (for-syntax (rename-in (submod "private/amalgam.rkt" core-derived)
                                [?! !])))
(provide (for-syntax (all-from-out (submod "private/amalgam.rkt" core-derived))))

(bounce (submod "private/amalgam.rkt" core-meta class-meta)
        (submod "private/amalgam.rkt" core-meta interface-meta)
        (submod "private/amalgam.rkt" core-meta veneer-meta))

;; re-export `meta` for non-expression spaces,
;; otherwise these can get shadowed (in a sense)
;; by the `meta` expression export
(bounce #:only (meta) #:spaces (rhombus/impo rhombus/expo)
        (submod "private/amalgam.rkt" core))

(meta-if-version-at-least
 "8.13.0.4"
 (#%declare #:flatten-requires)
 (void))
