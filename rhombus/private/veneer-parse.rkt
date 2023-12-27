#lang racket/base
(require enforest/syntax-local)

(provide (struct-out veneer-desc)
         veneer-desc-ref)

(struct veneer-desc (id
                     super-id
                     interface-ids
                     predicate-id    ; #f if not checked
                     convert-id      ; #f if predicate-based
                     method-shapes   ; same as `class-desc`
                     method-vtable   ; same as `class-desc`
                     method-map      ; same as `class-desc`
                     method-result   ; same as `class-desc`
                     dots            ; list of symbols for dot syntax
                     dot-provider    ; #f or compile-time identifier
                     static-infos    ; same as `class-desc`
                     flags))         ; list of 'call (public `call` is Callable), 'get, 'set, and/or 'append

(define (veneer-desc-ref v) (and (veneer-desc? v) v))
