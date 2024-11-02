#lang racket/base
(require (for-syntax racket/base)
	 racket/path
         "provide.rkt"
         "class-primitive.rkt"
         "annotation-failure.rkt"
         "call-result-key.rkt"
         "define-arity.rkt"
         "compare-key.rkt"
         "index-result-key.rkt"
         "static-info.rkt"
         "annotation-failure.rkt"
         "enum.rkt"
         "name-root.rkt"
         "realm.rkt"
         "rhombus-primitive.rkt"
         (submod "annotation.rkt" for-class)
         (submod "bytes.rkt" static-infos)
         (submod "function.rkt" for-info)
         (submod "list.rkt" for-listable)
         (submod "string.rkt" static-infos)
         (submod "symbol.rkt" for-static-info))

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot)
                     Path
                     CrossPath)
         (for-space rhombus/annot
                    PathString))

(module+ for-builtin
  (provide path-method-table))

(module+ for-static-info
  (provide (for-syntax get-path-static-infos)))

(define (path<=? p q)
  (or (equal? p q) (path<? p q)))

(define (path!=? p q)
  (not (equal? p q)))

(define (path>=? p q)
  (or (equal? p q) (path<? q p)))

(define (path>? p q)
  (path<? q p))

(define-primitive-class Path path
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info ((#%compare ((< path<?)
                                      (<= path<=?)
                                      (> path>?)
                                      (>= path>=?)
                                      (= equal?)
                                      (!= path!=?))))
  #:existing
  #:translucent
  #:fields
  ([bytes Path.bytes #,(get-bytes-static-infos)])
  #:namespace-fields
  ([Absolute Path.Absolute]
   [Relative Path.Relative]
   [DriveRelative Path.DriveRelative]
   [Element Path.Element]
   [Directory Path.Directory]
   [Dot Path.Dot]
   [current_directory current-directory])
  #:properties
  ()
  #:methods
  (name
   parent
   bytes
   add
   split
   string
   to_absolute_path
   to_directory_path
   directory_only
   suffix
   add_suffix
   replace_suffix
   cleanse
   normal_case
   simplify
   as_relative_to))

(define/arity #:name Path (path c)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (cond
    [(path? c) c]
    [(bytes? c) (bytes->path c)]
    [(string? c) (string->path c)]
    [(or (eq? c 'up) (eq? c 'same)) (build-path c)]
    [else (raise-annotation-failure who c "ReadableString || Bytes || Path || Path.Dot")]))

(define-primitive-class CrossPath path-for-some-system
  #:lift-declaration
  #:no-constructor-static-info
  #:existing
  #:translucent
  #:fields
  ([bytes CrossPath.bytes #,(get-bytes-static-infos)]
   [convention CrossPath.convention #,(get-symbol-static-infos)])
  #:namespace-fields
  ([Absolute CrossPath.Absolute]
   [Relative CrossPath.Relative]
   [DriveRelative CrossPath.DriveRelative]
   [Element CrossPath.Element]
   [Directory CrossPath.Directory]
   [Unix CrossPath.Unix]
   [Windows CrossPath.Windows]
   [Convention CrossPath.Convention])
  #:properties
  ()
  #:methods
  (name
   parent
   bytes
   add
   split
   string
   convention
   suffix
   add_suffix
   replace_suffix
   to_absolute_path
   to_directory_path
   directory_only
   cleanse
   normal_case
   simplify
   as_relative_to))

(define/arity #:name CrossPath (path-for-some-system bstr [c (system-path-convention-type)])
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (or (bytes? bstr) (eq? bstr 'up) (eq? bstr 'same))
    (raise-annotation-failure who bstr "Bytes || Path.Dot"))
  (unless (or (eq? c 'unix) (eq? c 'windows)) (raise-annotation-failure who c "CrossPath.Convention"))
  (if (bytes? bstr)
      (bytes->path bstr c)
      (build-path/convention-type bstr c)))

(define (path-is-absolute? v)
  (and (path? v)
       (complete-path? v)))

(define (path-is-relative? v)
  (and (path? v)
       (relative-path? v)))

(define (path-is-drive-relative? v)
  (and (path? v)
       (absolute-path? v)
       (not (complete-path? v))))

(define (path-dot? v)
  (or (eq? v 'same) (eq? v 'up)))

(define (path-directory? s)
  (and (path? s)
       (let-values ([(base name dir?) (split-path s)])
         dir?)))

(define (this-system-path-element? s)
  (and (path? s) (path-element? s)))

(define-annotation-syntax Path.Absolute
  (identifier-annotation path-is-absolute? #,(get-path-static-infos)))

(define-annotation-syntax Path.Relative
  (identifier-annotation path-is-relative? #,(get-path-static-infos)))

(define-annotation-syntax Path.DriveRelative
  (identifier-annotation path-is-absolute? #,(get-path-static-infos)))

(define-name-root Path.Element
  #:fields
  ([maybe Path.Element.maybe]
   [string Path.Element.string]
   [bytes Path.Element.bytes]))

(define-annotation-syntax Path.Element
  (identifier-annotation this-system-path-element? #,(get-path-static-infos)))

(define-annotation-syntax Path.Directory
  (identifier-annotation path-directory? #,(get-path-static-infos)))

(define-simple-symbol-enum Path.Dot
  up
  same)

(define/arity (Path.Element bstr)
  #:primitive (bytes->path-element)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (unless (bytes? bstr) (raise-annotation-failure who bstr "Bytes"))
  (bytes->path-element bstr))

(define/arity (Path.Element.maybe bstr)
  (unless (bytes? bstr) (raise-annotation-failure who bstr "Bytes"))
  (bytes->path-element bstr (system-path-convention-type) #t))

(define/arity (Path.Element.string p)
  (unless (this-system-path-element? p) (raise-annotation-failure who p "Path.Element"))
  (string->immutable-string (path-element->string p)))

(define/arity (Path.Element.bytes p)
  (unless (this-system-path-element? p) (raise-annotation-failure who p "Path.Element"))
  (bytes->immutable-bytes (path-element->bytes p)))

(define-static-info-syntax current-directory
  (#%function-arity 3)
  (#%call-result #,(get-path-static-infos))
  . #,(get-function-static-infos))

(define/method (Path.name p)
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (define-values (parent name dir?) (split-path p))
  name)

(define/method (Path.parent p)
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (define-values (parent name dir?) (split-path p))
  parent)

(define/method (Path.bytes p)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (unless (path? p) (raise-annotation-failure who p "Path"))
  (bytes->immutable-bytes (path->bytes p)))

(define/method (Path.string s)
  #:primitive (path->string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string (path->string s)))

(define/method (Path.add p . ss)
  #:local-primitive (build-path)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (unless (or (path-string? p) (eq? p 'same) (eq? p 'up))
    (raise-annotation-failure who p "PathString || Path.Dot"))
  (for ([s (in-list ss)])
    (unless (or (path-string? s) (eq? s 'same) (eq? s 'up))
      (raise-annotation-failure who s "PathString || Path.Dot")))
  (apply build-path p ss))

(define/method (Path.split p)
  #:static-infos ((#%call-result (#,@(get-treelist-static-infos)
                                  (#%ref-result #,(get-path-static-infos)))))
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (to-treelist #f (explode-path p)))

(define/method (Path.directory_only p)
  #:primitive (path-only)
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (path-only p))

(define/method (Path.suffix p)
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (define maybe-bstr (path-get-extension p))
  (and maybe-bstr (bytes->immutable-bytes maybe-bstr)))

(define/method (Path.add_suffix p sfx #:sep [sep "_"])
  #:local-primitive (path-add-extension)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (unless (or (string? sfx) (bytes? sfx)) (raise-annotation-failure who sfx "Bytes || ReadableString"))
  (unless (or (string? sep) (bytes? sep)) (raise-annotation-failure who sep "Bytes || ReadableString"))
  (path-add-extension p sfx sep))

(define/method (Path.replace_suffix p sfx)
  #:local-primitive (path-replace-extension)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (unless (or (string? sfx) (bytes? sfx)) (raise-annotation-failure who sfx "Bytes || ReadableString"))
  (path-replace-extension p sfx))

(define/method (Path.to_absolute_path p #:relative_to [base-path (current-directory)])
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (unless (and (path-string? base-path) (complete-path? base-path))
    (raise-annotation-failure who base-path "PathString.to_path && Path.Absolute"))
  (path->complete-path p base-path))

(define/method (Path.to_directory_path p)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (path->directory-path p))

(define/method (Path.cleanse p)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (cleanse-path p))

(define/method (Path.normal_case p)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (normal-case-path p))

(define/method (Path.simplify p)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (simplify-path p #f))

(define/method (Path.as_relative_to p base-p
                                    #:more_than_root [more-than-root? #f]
                                    #:more_than_same [more-than-same? #t]
                                    #:normal_case [normalize-case? #t])
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (unless (path-string? base-p) (raise-annotation-failure who base-p "PathString"))
  (find-relative-path base-p
                      (if (string? p) (string->path p) p)
                      #:more-than-root? more-than-root?	 
                      #:more-than-same? more-than-same?	 
                      #:normalize-case? normalize-case?))	 

(define-annotation-syntax PathString (identifier-annotation path-string? ()))

(define (path-for-some-system-is-absolute? v)
  (and (path-for-some-system? v)
       (complete-path? v)))

(define (path-for-some-system-is-relative? v)
  (and (path-for-some-system? v)
       (relative-path? v)))

(define (path-for-some-system-is-drive-relative? v)
  (and (path-for-some-system? v)
       (absolute-path? v)
       (not (complete-path? v))))

(define (some-system-path-element? s)
  (path-element? s))

(define (some-system-path-directory? s)
  (and (path-element? s)
       (let-values ([(base name dir?) (split-path s)])
         dir?)))

(define-annotation-syntax CrossPath.Absolute
  (identifier-annotation path-for-some-system-is-absolute? #,(get-path-for-some-system-static-infos)))

(define-annotation-syntax CrossPath.Relative
  (identifier-annotation path-for-some-system-is-relative? #,(get-path-for-some-system-static-infos)))

(define-annotation-syntax CrossPath.DriveRelative
  (identifier-annotation path-for-some-system-is-drive-relative? #,(get-path-for-some-system-static-infos)))

(define-annotation-syntax CrossPath.Element
  (identifier-annotation some-system-path-element? #,(get-path-for-some-system-static-infos)))

(define-annotation-syntax CrossPath.Directory
  (identifier-annotation some-system-path-directory? #,(get-path-for-some-system-static-infos)))

(define-simple-symbol-enum CrossPath.Convention
  #:extra
  ([current CrossPath.Convention.current])
  unix
  windows)

(define/arity (CrossPath.Convention.current)
  #:static-infos ((#%call-result #,(get-symbol-static-infos)))
  (system-path-convention-type))

(define-name-root CrossPath.Element
  #:fields
  ([maybe CrossPath.Element.maybe]
   [string CrossPath.Element.string]
   [bytes CrossPath.Element.bytes]
   [bytes CrossPath.Element.convention]))

(define/arity (CrossPath.Element bstr c)
  #:local-primitive (bytes->path-element)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (bytes? bstr) (raise-annotation-failure who bstr "Bytes"))
  (unless (or (eq? c 'unix) (eq? c 'windows)) (raise-annotation-failure who c "CrossPath.Convention"))
  (bytes->path-element bstr c #t))

(define/arity (CrossPath.Element.maybe bstr c)
  (unless (bytes? bstr) (raise-annotation-failure who bstr "Bytes"))
  (unless (or (eq? c 'unix) (eq? c 'windows)) (raise-annotation-failure who c "CrossPath.Convention"))
  (bytes->path-element bstr c #t))

(define/arity (CrossPath.Element.string p)
  (unless (some-system-path-element? p) (raise-annotation-failure who p "CrossPath.Element"))
  (string->immutable-string (path-element->string p)))

(define/arity (CrossPath.Element.bytes p)
  (unless (some-system-path-element? p) (raise-annotation-failure who p "CrossPath.Element"))
  (bytes->immutable-bytes (path-element->bytes p)))

(define/method (CrossPath.name p)
  (unless (path-for-some-system? p) (raise-annotation-failure who p "CrossPath"))
  (define-values (parent name dir?) (split-path p))
  name)

(define/method (CrossPath.parent p)
  (unless (path-for-some-system? p) (raise-annotation-failure who p "CrossPath"))
  (define-values (parent name dir?) (split-path p))
  parent)

(define/method (CrossPath.bytes s)
  #:primitive (path->bytes)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (bytes->immutable-bytes (path->bytes s)))

(define/method (CrossPath.string s)
  #:primitive (some-system-path->string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string (some-system-path->string s)))

(define/method (CrossPath.convention s)
  #:primitive (path-convention-type)
  #:static-infos ((#%call-result #,(get-symbol-static-infos)))
  (path-convention-type s))

(define/method (CrossPath.add p . ss)
  #:local-primitive (build-path)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (path-for-some-system? p) (raise-annotation-failure who p "CrossPath"))
  (for ([s (in-list ss)])
    (unless (or (some-system-path-element? s) (path-dot? s))
      (raise-annotation-failure who s "CrossPath.Element || Path.Dot")))
  (apply build-path p ss))

(define/method (CrossPath.split p)
  #:static-infos ((#%call-result (#,@(get-treelist-static-infos)
                                  (#%ref-result #,(get-path-for-some-system-static-infos)))))
  (unless (path-for-some-system? p) (raise-annotation-failure who p "CrossPath"))
  (to-treelist #f (explode-path p)))

(define/method (CrossPath.suffix p)
  (unless (path-for-some-system? p) (raise-annotation-failure who p "CrossPath"))
  (define maybe-bstr (path-get-extension p))
  (and maybe-bstr (bytes->immutable-bytes maybe-bstr)))

(define/method (CrossPath.add_suffix p sfx #:sep [sep "_"])
  #:local-primitive (path-add-extension)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (path-for-some-system? p) (raise-annotation-failure who p "CrossPath"))
  (unless (or (string? sfx) (bytes? sfx)) (raise-annotation-failure who sfx "Bytes || ReadableString"))
  (unless (or (string? sep) (bytes? sep)) (raise-annotation-failure who sep "Bytes || ReadableString"))
  (path-add-extension p sfx sep))

(define/method (CrossPath.replace_suffix p sfx)
  #:local-primitive (path-replace-extension)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (path-for-some-system? p) (raise-annotation-failure who p "CrossPath"))
  (unless (or (string? sfx) (bytes? sfx)) (raise-annotation-failure who sfx "Bytes || ReadableString"))
  (path-replace-extension p sfx))

(define/method (CrossPath.to_absolute_path p #:relative_to base-path)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (path-for-some-system? p) (raise-annotation-failure who p "CrossPath"))
  (unless (path-for-some-system-is-absolute? p) (raise-annotation-failure who p "CrossPath.Absolute"))
  (path->complete-path p base-path))

(define/method (CrossPath.directory_only p)
  (unless (path-for-some-system? p) (raise-annotation-failure who p "CrossPath"))
  (path-only p))

(define/method (CrossPath.to_directory_path p)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (path-for-some-system? p) (raise-annotation-failure who p "CrossPath"))
  (path->directory-path p))

(define/method (CrossPath.cleanse p)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (path-for-some-system? p) (raise-annotation-failure who p "CrossPath"))
  (cleanse-path p))

(define/method (CrossPath.normal_case p)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (path-for-some-system? p) (raise-annotation-failure who p "CrossPath"))
  (normal-case-path p))

(define/method (CrossPath.simplify p)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (path-for-some-system? p) (raise-annotation-failure who p "CrossPath"))
  (simplify-path p #f))

(define/method (CrossPath.as_relative_to p base-p
                                         #:more_than_root [more-than-root? #f]
                                         #:more_than_same [more-than-same? #t]
                                         #:normal_case [normalize-case? #t])
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (path-for-some-system? p) (raise-annotation-failure who p "CrossPath"))
  (unless (path-for-some-system? base-p) (raise-annotation-failure who base-p "CrossPath"))
  (find-relative-path base-p p
                      #:more-than-root? more-than-root?	 
                      #:more-than-same? more-than-same?	 
                      #:normalize-case? normalize-case?))

(define (path-is-unix? v)
  (and (path-for-some-system? v)
       (eq? 'unix (path-convention-type v))))

(define (path-is-windows? v)
  (and (path-for-some-system? v)
       (eq? 'windows (path-convention-type v))))

(define-annotation-syntax SomeSystmePath.Unix
  (identifier-annotation path-is-unix? #,(get-path-for-some-system-static-infos)))

(define-annotation-syntax SomeSystmePath.Windows
  (identifier-annotation path-is-windows? #,(get-path-for-some-system-static-infos)))

(define/arity (SomeSystmePath.Unix bstr)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (bytes? bstr) (raise-annotation-failure who bstr "Bytes"))
  (bytes->path bstr 'unix))

(define/arity (SomeSystmePath.Windows bstr)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (bytes? bstr) (raise-annotation-failure who bstr "Bytes"))
  (bytes->path bstr 'windows))

(set-primitive-contract! 'path-string? "PathString")
