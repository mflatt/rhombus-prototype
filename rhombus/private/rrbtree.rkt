#lang racket/base
(require (for-syntax racket/base)
         racket/fixnum
         racket/vector)

(provide make-rrbtree
         rrbtree-add
         rrbtree-cons
         rrbtree-append
         rrbtree-insert
         rrbtree-drop
         rrbtree->list)

(#%declare #:unsafe)

(define BITS 5)
(define MAX_WIDTH (expt 2 BITS))
(define MASK (- MAX_WIDTH 1))
(define MAX_ERROR 2)

(define (radix index height)
  (bitwise-and (fxrshift index (fx* BITS height)) MASK))

;; a node in the RRB Tree
;;
;;  - a node is fully dense if it has exactly `m` children where `m` is the branching factor of the overall Tree
;;    and each child is also fully dense
;;  - a node is leftwise dense if its first `n - 1` children, where `n` is its total number of children,
;;    are fully dense, and its `n`th child is leftwise-dense or fully dense. `n` is allowed to be < `m`
;;  - a node is balanced if it is leftwise dense or fully dense (note that leaves are always at least leftwise dense)
;;  - unbalanced nodes contain a size array `sizes`, balanced nodes do not

(define-syntax Node
  (syntax-rules ()
    [(Node) empty-node]
    [(Node children) children]
    [(Node children sizes) (let ([cs children]
                                 [szs sizes])
                             (unless (variable-reference-from-unsafe? (#%variable-reference))
                               #;(unless (vector? cs) (error 'Node "bad children vector: ~v" cs))
                               (unless (or (not szs) (vector? szs)) (error 'Node "bad sizes vector: ~v" szs)))
                             (if szs (cons cs szs) cs))]))

(define (vector*-take vec n) (vector*-copy vec 0 n))
(define (vector*-drop vec n) (vector*-copy vec n (vector*-length vec)))
(define (vector*-drop-right vec n) (vector*-copy vec 0 (- (vector*-length vec) n)))
(define (vector*-add-left val a) (vector*-append (vector val) a))
(define (vector*-add-right a val) (vector*-append a (vector val)))

(define (assert-node n)
  (unless (variable-reference-from-unsafe? (#%variable-reference))
    (unless (or (vector? n) (and (pair? n) (vector? (car n)) (vector? (cdr n))))
      (error 'node "expected a node: ~v" n))))

(define (node-balanced? n) (assert-node n) (not (pair? n)))
(define (node-children n) (assert-node n) (if (pair? n) (car n) n))
(define (node-sizes n) (assert-node n) (and (pair? n) (cdr n)))
(define (node-size n) (assert-node n) (vector*-length (node-children n)))
(define (node-first n) (assert-node n) (vector*-ref (node-children n) 0))
(define (node-last n) (assert-node n) (let ([cs (node-children n)])
                                        (vector*-ref cs (fx- (vector*-length cs) 1))))
(define (node-ref n i) (assert-node n) (vector*-ref (node-children n) i))
(define (node-set n i v) (assert-node n) (vector*-set/copy (node-children n) i v))
(define (node-length n) (assert-node n) (vector*-length (node-children n)))

;; `node*` refers to a balanced node
(define (assert-node* n)
  (unless (variable-reference-from-unsafe? (#%variable-reference))
    (unless (vector? n)
      (error 'node* "expected a node*: ~v" n))))

(define (node*-children n) (assert-node* n) n)
(define (node*-ref n i) (assert-node* n) (vector*-ref (node*-children n) i))
(define (node*-set n i v) (assert-node* n) (vector*-set/copy (node*-children n) i v))
(define (node*-length n) (assert-node* n) (vector*-length (node*-children n)))
(define (node*-size n) (assert-node* n) (vector*-length (node*-children n)))
(define (node*-last n) (assert-node* n) (let ([cs (node*-children n)])
                                          (vector*-ref cs (fx- (vector*-length cs) 1))))

(define empty-node (Node (vector)))

(define (leaf v) (Node (vector v)))

(struct rrbtree (root size height)
  #:authentic
  #:property prop:equal+hash (list
                              ;; TODO: make faster
                              (lambda (v other recur)
                                (and (fx= (rrbtree-size v)
                                          (rrbtree-size other))
                                     (for/and ([a (in-rrbtree v)]
                                               [b (in-rrbtree other)])
                                       (recur a b))))
                              ;; TODO: hash only subset
                              (lambda (v recur)
                                (recur (rrbtree->list v)))
                              ;; TODO: hash only subset
                              (lambda (v recur)
                                (recur (rrbtree->list v)))))

(define empty-rrbtree (rrbtree empty-node 0 0))

(define make-rrbtree
  (case-lambda
    [() empty-rrbtree]
    [(a) (rrbtree (leaf a) 1 0)]
    [(a b) (rrbtree (Node (vector a b)) 2 0)]
    [(a b c) (rrbtree (Node (vector a b c)) 3 0)]
    [(a b c . ds) (rrbtree-add-all (rrbtree (Node (vector a b c)) 3 0) ds)]))

(define (check-rrbtree who rrbt)
  (unless (rrbtree? rrbt)
    (raise-argument-error who "rrbtree?" rrbt)))

(define (check-rrbtree-index who rrbt index)
  (unless (fixnum? index)
    (if (exact-nonnegative-integer? index)
        (raise-argument-error who "exact-nonnegative-integer?" index)
        (error who "index out of range: ~v" index)))
  (define size (rrbtree-size rrbt))
  (when (or (index . fx< . 0) (index . fx>= . size))
    (error who "index out of range: ~v / ~s" index size)))

(define (check-rrbtree-end-index who rrbt index)
  (unless (fixnum? index)
    (if (exact-nonnegative-integer? index)
        (raise-argument-error who "exact-nonnegative-integer?" index)
        (error who "count out of range: ~v" index)))
  (define size (rrbtree-size rrbt))
  (when (or (index . fx< . 0) (index . fx> . size))
    (error who "count out of range: ~v / ~v" index size)))

(define-sequence-syntax in-rrbtree
  (lambda () #'in-rrbtree/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(d) (_ rrbt-expr)]
       #'[(d)
          (:do-in
           ([(rrbt) rrbt-expr])
           (unless (variable-reference-from-unsafe? (#%variable-reference))
             (check-rrbtree 'in-rrbtree rrbt))
           ([pos 0]
            [node empty-node]
            [node-pos 0])
           (pos . fx< . (rrbtree-size rrbt))
           ([(d next-node next-node-pos)
             (if (node-pos . fx< . (node-size node))
                 (values (node-ref node node-pos) node (fx+ node-pos 1))
                 (let-values ([(node node-pos) (rrbtree-node-for 'in-rrbtree rrbt pos)])
                   (values (node-ref node node-pos) node (fx+ node-pos 1))))])
           #t
           #t
           ((fx+ pos 1) next-node next-node-pos))]])))

(define (in-rrbtree/proc rrbt)
  ;; Slower strategy than the inline version, but the
  ;; `make-do-sequence` protocol requires a single value for the
  ;; position, and allocating a value to track index plus node defeats
  ;; the benefit of threading the current node
  (check-rrbtree 'in-rrbtree rrbt)
  (make-do-sequence
   (lambda ()
     (values
      (lambda (i) (rrbtree-ref rrbt i))
      (lambda (i) (fx+ i 1))
      0
      (lambda (i) (i . fx< . (rrbtree-size rrbt)))
      #f
      #f))))

(define (rrbtree-ref rrbt index)
  (check-rrbtree 'rrbtree-ref rrbt)
  (check-rrbtree-index 'rrbtree-ref rrbt index)
  (define-values (node pos) (rrbtree-node-for 'node-ref rrbt index))
  (node-ref node pos))

(define (rrbtree-node-for who rrbt index)
  (define height (rrbtree-height rrbt))
  (define size (rrbtree-size rrbt))
  (cond
    [(fx= height 0)
     (values (rrbtree-root rrbt) index)]
    [else
     (let walk ([node (rrbtree-root rrbt)]
                [index index]
                [height height])
       (cond
         [(node-balanced? node)
          (values (let sub ([n node] [height height])
                    (cond
                      [(fx= height 0) n]
                      [else (sub (node*-ref n (radix index height))
                                 (fx- height 1))]))
                  (bitwise-and index MASK))]
         [(fx= height 1)
          (define-values (bi si) (step node index height))
          (values (node-ref node bi) (bitwise-and si MASK))]
         [else
          (define-values (bi si) (step node index height))
          (walk (node-ref node bi) si (fx- height 1))]))]))

;; functionally update the slot at `index` to `el`
(define (rrbtree-set rrbt index el)
  (check-rrbtree 'rrbtree-set rrbt)
  (check-rrbtree-index 'rrbtree-set rrbt index)
  (define new-node
    (let set ([node (rrbtree-root rrbt)]
              [index index]
              [el el]
              [height (rrbtree-height rrbt)])
      (cond
        [(fx= height 0)
         (node-set node (radix index height) el)]
        [(node-balanced? node)
         (define branch-index (radix index height))
         (node*-set node branch-index (set (node*-ref node branch-index) index el (fx- height 1)))]
        [else
         (define-values (branch-index subindex) (step node index height))
         (node-set node branch-index (set (node-ref node branch-index) subindex el (fx- height 1)))])))
  (rrbtree new-node (rrbtree-size rrbt) (rrbtree-height rrbt)))

;; add `el` to end of vector
(define (rrbtree-add rrbt el)
  (check-rrbtree 'rrbtree-set rrbt)
  (define size (rrbtree-size rrbt))
  (cond
    [(fx= size 0)
     (rrbtree (leaf el) 1 0)]
    [else
     (define new-root (build (rrbtree-root rrbt) (rrbtree-height rrbt) el))
     (if new-root
         ;; enough space in original tree
         (rrbtree new-root (fx+ size 1) (rrbtree-height rrbt))
         ;; not enough space in original tree
         (rrbtree (Node (vector (rrbtree-root rrbt)
                                (new-branch el (rrbtree-height rrbt))))
                  (fx+ size 1)
                  (fx+ (rrbtree-height rrbt) 1)))]))

;; TODO chunk adding here by 32 and add whole nodes at a time?
(define (rrbtree-add-all rrbt els)
  (check-rrbtree 'rrbtree-add-all rrbt)
  (for/fold ([rrbt rrbt]) ([el (in-list els)])
    (rrbtree-add rrbt el)))

(define (rrbtree->list rrbt)
  (check-rrbtree 'rrbtree->list rrbt)
  (for/list ([el (in-rrbtree rrbt)])
    el))

(define (rrbtree-length rrbtree)
  (rrbtree-size rrbtree))

;; trees that are a result of this method may not meet invariants, but rebalancing is costly
;; and future concatenations would restore the invariants due to rebalancing being done on concats.
;; TODO write some tests showing this
(define (rrbtree-take rrbt pos)
  (check-rrbtree 'rrbtree-take rrbt)
  (check-rrbtree-end-index 'rrbtree-take rrbt pos)
  (cond
    [(fx= pos 0)
     empty-rrbtree]
    [(fx= pos (rrbtree-size rrbt))
     rrbt]
    [else
     (define new-root
       (let take ([node (rrbtree-root rrbt)]
                  [index (fx- pos 1)]
                  [height (rrbtree-height rrbt)])
         (cond
           [(fx= height 0)
            (Node (vector*-take (node-children node) (fx+ (radix index 0) 1)))]
           [(node-balanced? node)
            (define branch-index (radix index height))
            (define new-children (vector*-take (node*-children node) (fx+ branch-index 1)))
            (vector*-set! new-children branch-index (take (vector*-ref new-children branch-index) index (fx- height 1)))
            (Node new-children)]
           [else
            (define-values (branch-index subindex) (step node index height))
            (define new-children (vector*-take (node-children node) (fx+ branch-index 1)))
            (define new-sizes (vector*-take (node-sizes node) (fx+ branch-index 1)))
            (vector*-set! new-children branch-index (take (node-ref new-children branch-index) subindex (fx- height 1)))
            (vector*-set! new-sizes branch-index (fx+ index 1))
            (Node new-children new-sizes)])))
     (squash new-root pos (rrbtree-height rrbt))]))

(define (rrbtree-drop rrbt pos)
  (check-rrbtree 'rrbtree-drop rrbt)
  (check-rrbtree-end-index 'rrbtree-drop rrbt pos)
  (cond
    [(fx= pos 0)
     rrbt]
    [(fx= pos (rrbtree-size rrbt))
     empty-rrbtree]
    [else
     (define new-root
       (let drop ([node (rrbtree-root rrbt)]
                  [index pos]
                  [height (rrbtree-height rrbt)])
         (cond
           [(fx= height 0)
            (Node (vector*-drop (node-children node) (radix index 0)))]
           [(node-balanced? node)
            (define branch-index (radix index height))
            (define new-children (vector*-drop (node*-children node) branch-index))
            (define new-child (drop (node*-ref node branch-index) index (fx- height 1)))
            (vector*-set! new-children 0 new-child)

            (define size0 (size-subtree new-child (fx- height 1)))
            (define new-len (fx- (node-size node) branch-index))
            (define new-sizes (make-vector new-len size0))

            (cond
              [(fx= new-len 1)
               (void)]
              [else
               (define step (fxlshift 1 (fx* height BITS)))
               (for ([i (in-range 0 (fx- new-len 1))])
                 (vector*-set! new-sizes i (fx+ size0 (fx* i step))))
               (define sizeN (size-subtree (vector*-ref new-children (fx- new-len 1)) (fx- height 1)))
               (vector*-set! new-sizes (fx- new-len 1) (fx+ size0 (fx* (fx- new-len 2) step) sizeN))
               (Node new-children new-sizes)])]
           [else
            (define-values (branch-index subindex) (step node index height))
            (define new-children (vector*-drop (node-children node) branch-index))
            (define old-len (vector*-length (node-sizes node)))
            (define new-sizes (for/vector #:length (- old-len branch-index)
                                          ([i (in-range branch-index old-len)])
                                          (fx- (vector*-ref (node-sizes node) i) index)))
            (define new-child (drop (node-ref node branch-index) subindex (fx- height 1)))
            (vector*-set! new-children 0 new-child)
            (Node new-children new-sizes)])))
     (squash new-root (fx- (rrbtree-size rrbt) pos) (rrbtree-height rrbt))]))

(define (rrbtree-split rrbt at)
  (check-rrbtree 'rrbtree-split rrbt)
  (check-rrbtree-end-index 'rrbtree-split rrbt at)
  (cond
    [(fx= at 0) (values empty-rrbtree rrbt)]
    [(fx= at (rrbtree-size rrbt)) (values rrbt empty-rrbtree)]
    [else (values (rrbtree-take rrbt at) (rrbtree-drop rrbt at))]))

(define (rrbtree-insert rrbt at el)
  (check-rrbtree 'rrbtree-insert rrbt)
  (check-rrbtree-end-index 'rrbtree-insert rrbt at)
  (cond
    [(fx= at 0) (rrbtree-cons rrbt el)]
    [(fx= at (rrbtree-size rrbt)) (rrbtree-add rrbt el)]
    [else (rrbtree-append (rrbtree-add (rrbtree-take rrbt at) el)
                          (rrbtree-drop rrbt at))]))

(define (rrbtree-cons rrbt el)
  (check-rrbtree 'rrbtree-insert rrbt)
  (cond
    [(fx= 0 (rrbtree-size rrbt))
     (rrbtree (leaf el) 1 0)]
    [else
     ;; insert in leftmost node, if it has space; this
     ;; will always work for small lists
     (define new-root
       (let insert-left ([a (rrbtree-root rrbt)]
                         [height (rrbtree-height rrbt)])
         (cond
           [(fx= height 0)
            (and ((node-size a) . < . MAX_WIDTH)
                 (Node (vector*-add-left el (node-children a))))]
           [else
            (define left (insert-left (vector*-ref (node-children a) 0) (fx- height 1)))
            (and left
                 (Node (vector*-set/copy (node-children a) 0 left)
                       (let ([sizes (node-sizes a)])
                         (for/vector #:length (vector*-length sizes) ([n (in-vector sizes)])
                                     (fx+ n 1)))))])))
     (cond
       [new-root
        (rrbtree new-root (fx+ (rrbtree-size rrbt) 1) (rrbtree-height rrbt))]
       [else
        (rrbtree-append (rrbtree (leaf el) 1 0) rrbt)])]))

(define (rrbtree-append rrbt rhs)
  (check-rrbtree 'rrbtree-append rrbt)
  (check-rrbtree 'rrbtree-append rhs)
  (cond
    [(fx= 0 (rrbtree-size rrbt)) rhs]
    [(fx= 0 (rrbtree-size rhs)) rrbt]
    [else
     (define-values (new-children new-height)
       (concat-subtree (rrbtree-root rrbt)
                       (rrbtree-height rrbt)
                       (rrbtree-root rhs)
                       (rrbtree-height rhs)))
     (rrbtree new-children
              (fx+ (rrbtree-size rrbt)
                   (rrbtree-size rhs))
              new-height)]))


;; after take or drop, squash tree if it can be shorter:
(define (squash node new-size new-height)
  (cond
    [(and (fx= (node-size node) 1)
          (fx> new-height 0))
     (squash (node-first node) new-size (fx- new-height 1))]
    [else
     (rrbtree node new-size new-height)]))

;; result height is either max of two heights or one more
;; than the max of the heights
(define (concat-subtree left
                        height-l
                        right
                        height-r)
  ;; only trees of the same height can be concatenated
  (cond
    [(fx> height-l height-r)
     (define-values (mid height-m)
       (concat-subtree (node-last left)
                       (fx- height-l 1)
                       right
                       height-r))
     (rebalance left
                mid
                #f
                height-l
                height-m)]
    [(fx< height-l height-r)
     (define-values (mid height-m)
       (concat-subtree left
                       height-l
                       (node-first right)
                       (fx- height-r 1)))
     (rebalance #f
                mid
                right
                height-r
                height-m)]
    [(fx= height-l 0)
     (cond
       [(fx<= (fx+ (node-size left) (node-size right)) MAX_WIDTH)
        (values (Node (vector*-append (node-children left) (node-children right)))
                0)]
       [else
        (values (Node (vector (node-children left) (node-children right))
                      (vector (node-size left) (fx+ (node-size left) (node-size right))))
                1)])]
    [else
     ;; two internal nodes with same height
     (define-values (mid height-m)
       (concat-subtree (node-last left)
                       (fx- height-l 1)
                       (node-first right)
                       (fx- height-r 1)))
     (rebalance left
                mid
                right
                height-l
                height-m)]))

;; keeps all but last of `left`, all but first of `right`,
;; and all of `center`; height is the same for `left` and
;; `right`, which `center` height might be one less; height
;; is at least 1; the resulting height grows by either 0 or 1
(define (rebalance left
                   center
                   right
                   height
                   height_c)
  (define all-slots (merge-nodes left
                                 (if (fx< height_c height)
                                     (Node (vector center))
                                     center)
                                 right))
  (define plan (concat-plan all-slots))
  (define rebalanced-slots (exec-concat-plan all-slots plan height))

  (cond
    [(fx<= (vector*-length rebalanced-slots) MAX_WIDTH)
     (values (set-sizes rebalanced-slots height)
             height)]
    [else
     (define new-left (vector*-take rebalanced-slots MAX_WIDTH))
     (define new-right (vector*-drop rebalanced-slots MAX_WIDTH))
     (values (set-sizes (vector (set-sizes new-left height)
                                (set-sizes new-right height))
                        (fx+ height 1))
             (fx+ height 1))]))

;; merge all children except for the rightmost in `left` and leftmost in `right`
(define (merge-nodes left center right)
  (vector*-append (if (not left) (vector) (vector*-drop-right (node-children left) 1))
                  (node-children center)
                  (if (not right) (vector) (vector*-drop (node-children right) 1))))

;; TODO how to avoid setting sizes when the tree is leftwise dense?
(define (set-sizes children height)
  (cond
    [(fx= height 0)
     (Node children)]
    [else
     (define sizes (make-vector (vector*-length children)))
     (for/fold ([sum 0]) ([i (in-range 0 (vector*-length children))])
       (define new-sum (fx+ sum (size-subtree (vector*-ref children i) (fx- height 1))))
       (vector*-set! sizes i new-sum)
       new-sum)
     (Node children sizes)]))

;; TODO redesign this to be less imperative?
;; receives a node that is temporarily allowed to have > max_width children, redistributes it to conform to invariant
(define (concat-plan slots)
  (define plan (make-vector (vector*-length slots)))
  (define child-count
    (for/fold ([count 0]) ([i (in-range 0 (vector*-length slots))])
      (define sz (node-size (vector*-ref slots i)))
      (vector*-set! plan i sz)
      (fx+ count sz)))

  (define optimal-node-len (fxquotient (fx+ child-count MAX_WIDTH -1) MAX_WIDTH))
  (define target-node-len (fx+ optimal-node-len MAX_ERROR))
  
  (if (fx>= target-node-len (vector*-length plan))
      #false
      (distribute plan target-node-len (vector*-length plan))))

(define (distribute plan target count [node-idx 0])
  (cond
    [(fx>= target count)
     (vector*-take plan count)]
    [else
     (define init-i (short-node plan node-idx))
     (define-values (i r)
       (let loop ([i init-i]
                  [r (vector*-ref plan init-i)])
         (cond
           [(fx= r 0)
            (values i r)]
           [else
            (define min-size (min (fx+ r (vector*-ref plan (fx+ i 1))) MAX_WIDTH))
            (vector*-set! plan i min-size)
            (loop (fx+ i 1) (fx- (fx+ r (vector*-ref plan (fx+ i 1))) min-size))])))

     ;; we've removed a node (conceptually) at this point,
     ;; so move nodes to the right of current node left by one
     (for ([j (in-range i (fx- count 1))])
       (vector*-set! plan j (vector*-ref plan (fx+ j 1))))

     (distribute plan target (fx- count 1) (fx- i 1))]))

(define (short-node plan i)
  (if (fx< (vector*-ref plan i) (fx- MAX_WIDTH 1))
      i
      (short-node plan (fx+ i 1))))

(define (exec-concat-plan slots plan height)
  (cond
    [(not plan) slots]
    [else
     (define flattened-size
       (for/fold ([sum 0]) ([node (in-vector slots)])
         (fx+ sum (node-size node))))
     (define flattened
       (for*/vector #:length flattened-size ([node (in-vector slots)]
                                             [child (in-vector (node-children node))])
         child))

     (define new-slots (make-vector (vector*-length plan)))
     (for/fold ([sum 0]) ([i (in-range 0 (vector*-length plan))])
       (define new-sum (fx+ sum (vector*-ref plan i)))
       (define new-node
         (for/vector #:length (fx- new-sum sum)
                     ([j (in-range sum new-sum)])
                     (vector*-ref flattened j)))
       (vector*-set! new-slots i (set-sizes new-node (fx- height 1)))
       new-sum)
     
     new-slots]))

(define (size-subtree node height)
  (cond
    [(fx= height 0)
     (vector*-length (node-children node))]
    [(node-sizes node)
     => (lambda (sizes)
          (vector*-ref sizes (fx- (vector*-length sizes) 1)))]
    [else
     ;; if sizes is #false, then we know we have a leftwise-dense subtree
     (fx+ (fxlshift (fx- (node*-size node) 1) (fx* height BITS))
          (size-subtree (node*-last node) (fx- height 1)))]))

;; helper functions

(define (scan-sizes sizes target-index [i 0])
  (if (fx<= (vector*-ref sizes i) target-index)
      (scan-sizes sizes target-index (fx+ i 1))
      i))

;; calculate next branch to take and subindex of `index` along that path
(define (step node index height)
  (define sizes (node-sizes node))
  (define branch (scan-sizes sizes index (radix index height)))
  (values branch
          (if (fx= branch 0)
              index
              (fx- index (vector*-ref sizes (fx- branch 1))))))

;; add if there's room, return #false otherwise
(define (build n height el)
  (cond
    [(fx= height 0)
     (if (fx< (node-size n) MAX_WIDTH)
         (Node (vector*-add-right (node-children n) el))
         #false)]
    [else
     (define size (node-size n))
     (define child (and (fx> size 0)
                        (build (node-ref n (fx- size 1)) (fx- height 1) el)))
     (cond
       [child
        (Node (vector*-set/copy (node-children n) (fx- size 1) child)
              (let ([sizes (node-sizes n)])
                (and sizes
                     (vector*-set/copy sizes
                                       (fx- (vector*-length sizes) 1)
                                       (fx+ (vector*-ref sizes (fx- (vector*-length sizes) 1)) 1)))))]
       [(fx< (node-size n) MAX_WIDTH)
        (Node (vector*-add-right (node-children n)
                           (new-branch el (fx- height 1)))
              (let ([sizes (node-sizes n)])
                (and sizes
                     (vector*-add-right sizes
                                        (fx+ (vector*-ref sizes (fx- (vector*-length sizes) 1)) 1)))))]
       [else
        #false])]))

;; create a branch of height `height` terminating in a unary leaf node containing `el`
(define (new-branch el height)
  (if (fx= height 0)
      (leaf el)
      (Node (vector (new-branch el (fx- height 1))))))
