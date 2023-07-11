#lang racket/base
(require racket/pretty
         "../lex.rkt"
         "../parse.rkt"
         "../print.rkt"
         "../write.rkt"
         "input.rkt")

(define (check which input expected)
  (printf "checking ~s\n" which)
  (let ([in (open-input-string input)])
    (define (out name parsed write)
      (define path (build-path (find-system-path 'temp-dir) name))
      (printf "~a\n" path)
      (call-with-output-file*
       path
       #:exists 'truncate
       (lambda (o) (write parsed o))))
    (port-count-lines! in)
    (define parsed-stx (parse-all in))
    (define parsed (syntax->datum parsed-stx))
    (unless (equal? expected parsed)
      (out "expected" expected pretty-write)
      (out "parsed" parsed pretty-write)
      (error "parse failed"))
    (define printed (shrubbery-syntax->string parsed-stx))
    (unless (equal? input printed)
      (out "expected" input display)
      (out "printed" printed display)
      (error "print failed"))
    (define (check-reparse mode)
      (define (add-newlines bstr)
        (define in (open-input-bytes bstr))
        (define out (open-output-bytes))
        (display ";«" out)
        (for ([tok (in-list (lex-all in error))])
          (define loc (token-srcloc tok))
          (define pos (sub1 (srcloc-position loc)))
          (display (subbytes bstr pos (+ pos (srcloc-span loc))) out)
          (newline out))
        (display "»" out)
        (get-output-bytes out))
      (define (render-doc doc o)
        ;; a dumb "pretty" printer that always uses either the 1-line mode
        ;; or maximal-line mode, on the assumption that the first branch
        ;; of each `or` leads to the former and the latter branch leads
        ;; to the latter
        (let loop ([doc doc] [col 0] [indent 0])
          (cond
            [(string? doc) (display doc o) (+ col (string-length doc))]
            [(bytes? doc) (display doc o) (+ col (bytes-utf-8-length doc))]
            [(eq? doc 'nl)
             (newline o)
             (display (make-string indent #\space) o)
             indent]
            [(not (and (pair? doc) (list? doc)))
             (error 'render-doc "bad format ~v" doc)]
            [(eq? (car doc) 'seq)
             (for/fold ([col col]) ([doc (in-list (cdr doc))])
               (loop doc col indent))]
            [(and (eq? (car doc) 'nest)
                  (= (length doc) 3))
             (loop (caddr doc) col (+ indent (cadr doc)))]
            [(and (eq? (car doc) 'align)
                  (= (length doc) 2))
             (loop (cadr doc) col col)]
            [(and (eq? (car doc) 'or)
                  (= (length doc) 3))
             (if (eq? mode 'pretty-multi)
                 (loop (caddr doc) col indent)
                 (loop (cadr doc) col indent))]
            [else
             (error 'render-doc "bad format ~v" doc)])))
      (define reparsed (let ([o (open-output-bytes)])
                         (cond
                           [(or (eq? mode 'pretty)
                                (eq? mode 'pretty-multi))
                            (define doc (pretty-shrubbery parsed))
                            (render-doc doc o)
                            (unless (eq? mode 'pretty-multi)
                              (define o2 (open-output-bytes))
                              (write-shrubbery parsed o2)
                              (unless (equal? (get-output-bytes o) (get-output-bytes o2))
                                (out "direct" (get-output-bytes o2) display)
                                (out "doc" (get-output-bytes o) display)
                                (error "doc does not match direct")))]
                           [else
                            (write-shrubbery parsed o)])
                         (define new-in (let ([bstr (get-output-bytes o)])
                                          (cond
                                            [(eq? mode 'add-newlines) (add-newlines bstr)]
                                            [else bstr])))
                         (or (with-handlers ([exn:fail? (lambda (exn) (eprintf "~a\n" (exn-message exn)) #f)])
                               (define in (open-input-bytes new-in))
                               (when (eq? mode 'count)
                                 (port-count-lines! in))
                               (syntax->datum (parse-all in)))
                             (begin
                               (out "wrote" new-in displayln)
                               (error "parse of wrote failed")))))
      (unless (equal? parsed reparsed)
        (out "expected" parsed pretty-print)
        (out "reparsed" reparsed pretty-print)
        (out "printed" printed display)
        (error "print failed")))
    (check-reparse 'count)
    (check-reparse 'no-count)
    (check-reparse 'add-newlines)
    (check-reparse 'pretty)
    (check-reparse 'pretty-multi)))

(define (check-fail input rx)
  (let ([in (open-input-string input)])
    (port-count-lines! in)
    (unless (with-handlers ([exn:fail? (lambda (exn) (regexp-match? rx (exn-message exn)))])
              (parse-all in)
              #f)
      (error 'check-fail "failed to fail: ~s" input))))

(define (lines s . ss)
  (apply string-append s (for/list ([s (in-list ss)]) (string-append "\n" s))))

(check 1 input1 expected1)
(check '1a input1a expected1a)
(check '1b input1b expected1b)
(check 2 input2 expected2)
(check 3 input3 expected3)
(check 4 input4 expected4)
(check 5 input5 expected5)
(check 6 input6 expected6)
(check 7 input7 expected7)

(check-fail "x:" #rx"empty block")
(check-fail "x:\ny" #rx"empty block")
(check-fail "x |" #rx"empty block")
(check-fail "x |\ny" #rx"empty block")
(check-fail "(x:)" #rx"empty block")
(check-fail "(1, x:)" #rx"empty block")

(check-fail "if t | «tag: if f | a | b» more | y" #rx"no terms allowed after `»`")
(check-fail "x: y:« a; b » more; c" #rx"no terms allowed after `»`")

(check-fail (lines "x"
                   " y")
            #rx"wrong indentation")
(check-fail (lines "1"
                   " + 2"
                   "   + 3")
            #rx"wrong indentation")
(check-fail (lines "1: 2"
                   " + 3")
            #rx"wrong indentation")
(check-fail (lines "x | y | c"
                   "  + 3")
            #rx"wrong indentation")

(check-fail "a:\n«c»" #rx"not on the same line")
(check-fail "a |\n«c»" #rx"not on the same line")
(check-fail ";\n«c»" #rx"not on the same line")

(check-fail "(«| a | c»)" #rx"misplaced `«`")
(check-fail "z «|« « | y » » »" #rx"misplaced `«`")
(check-fail "«|« w « | y » » »" #rx"misplaced `«`")

(check-fail (lines "(#// x,"
                   "     y)") #rx"wrong indentation")
(check-fail (lines "(#//"
                   "   x,"
                   "   y)") #rx"wrong indentation")
(check-fail (lines "cond"
                   "  #// | x") #rx"wrong indentation")
(check-fail (lines "cond"
                   "#// | x") #rx"misplaced `[|]`")
(check-fail "x #// y" #rx"misplaced group comment")

(check-fail "a(;«1» 2)" #rx"comma")
(check-fail "a(;«1; 2», 2)" #rx"multi-group splice")

(check-fail "@«1, 2»" #rx"second group not allowed")
(check-fail "@(«1, 2»)" #rx"second group not allowed")
(check-fail "@(«1 2» )" #rx"expected a closing `[)]` immediately after closing `»`")
(check-fail "@x[1]{2}" #rx"cannot start `[[]`")
(check-fail "@«1: 2»()" #rx"block not allowed")
(check-fail "@«1: 2» x" #rx"block not allowed")
(check-fail "@(«1: 2»)x" #rx"block not allowed")
(check-fail "@«| 1» more" #rx"block not allowed")

(check-fail "1x" #rx"read error")
(check-fail "1x_y" #rx"read error")
(check-fail "1.x" #rx"read error")
(check-fail "1.2x" #rx"read error")
(check-fail "1.2ex" #rx"read error")
(check-fail "1.2e5x" #rx"read error")
(check-fail "1.2e5.x" #rx"read error")
(check-fail "1.2e5.0x" #rx"read error")
(check-fail "1.2.3" #rx"read error")
(check-fail "1.2.3." #rx"read error")

(check-fail "~#%call" #rx"read error")
