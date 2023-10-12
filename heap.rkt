#lang racket/base
(require racket/contract/base
         racket/vector
         racket/match)

(define MIN-SIZE 4)

;; TODO: add pretty printing

(struct heap ([vec #:mutable] [count #:mutable] <=? notify))
;; length(vec)/4 <= size <= length(vec), except size >= MIN-SIZE
;; size = next available index

;; A VT is a binary tree represented as a vector.

;; VT Index functions

(define (vt-root) 0)

(define (vt-parent n) (quotient (sub1 n) 2))
(define (vt-leftchild n) (+ (* n 2) 1))
(define (vt-rightchild n) (+ (* n 2) 2))

(define (vt-root? n) (zero? n))
(define (vt-leftchild? n) (odd? n))
(define (vt-rightchild? n) (even? n))


;; Operations

(define (heapify-up <=? vec n notify)
  (unless (vt-root? n)
    (let* ([parent (vt-parent n)]
           [n-key (vector-ref vec n)]
           [parent-key (vector-ref vec parent)])
      (unless (<=? parent-key n-key)
        (vector-set! vec parent n-key)
	(notify n-key parent)
        (vector-set! vec n parent-key)	
        (notify parent-key n)
        (heapify-up <=? vec parent notify)))))

(define (heapify-down <=? vec n size notify)
  (let ([left (vt-leftchild n)]
        [right (vt-rightchild n)]
        [n-key (vector-ref vec n)])
    (when (< left size)
      (let ([left-key (vector-ref vec left)])
        (let-values ([(child child-key)
                      (if (< right size)
                          (let ([right-key (vector-ref vec right)])
                            (if (<=? left-key right-key)
                                (values left left-key)
                                (values right right-key)))
                          (values left left-key))])
          (unless (<=? n-key child-key)
            (vector-set! vec n child-key)
	    (notify child-key n)
            (vector-set! vec child n-key)
	    (notify n-key child)
            (heapify-down <=? vec child size notify)))))))

(define (subheap? <=? vec n size)
  (let ([left (vt-leftchild n)]
        [right (vt-rightchild n)])
    (and (if (< left size)
             (<=? (vector-ref vec n) (vector-ref vec left))
             #t)
         (if (< right size)
             (<=? (vector-ref vec n) (vector-ref vec right))
             #t))))

(define (grow-vector v1 new-size-min)
  (let ([new-size (let loop ([size (vector-length v1)])
                    (if (>= size new-size-min)
                        size
                        (loop (* size 2))))])
    (let ([v2 (make-vector new-size #f)])
      (vector-copy! v2 0 v1 0)
      v2)))

(define (shrink-vector v1)
  (let ([v2 (make-vector (quotient (vector-length v1) 2) #f)])
    (vector-copy! v2 0 v1 0 (vector-length v2))
    v2))

;; Heaps

(define dont-notify (lambda (key index) (void)))

(define (make-heap <=? [notify dont-notify])
  (heap (make-vector MIN-SIZE #f) 0 <=? notify))

(define (list->heap <=? lst [notify dont-notify])
  (vector->heap <=? (list->vector lst) 0 (length lst) notify))

(define (vector->heap <=? vec0 [start 0] [end (vector-length vec0)]
		      [notify dont-notify])
  (define size (- end start))
  (define len (let loop ([len MIN-SIZE])
		(if (<= size len) len (loop (* 2 len)))))
  (define vec (make-vector len #f))
  ;; size <= length(vec)
  (vector-copy! vec 0 vec0 start end)
  (for ([n (in-range 0 (- end start))])
       (notify (vector-ref vec n) n))
  (for ([n (in-range (sub1 size) -1 -1)])
    (heapify-down <=? vec n size notify))
  (heap vec size <=? notify))

(define (heap-copy h)
  (match h
    [(heap vec count <=? notify)
     (heap (vector-copy vec) count <=? notify)]))

(define (heap-add! h . keys)
  (heap-add-all! h (list->vector keys)))

(define (heap-add-all! h keys)
  (let-values ([(keys keys-size)
                (cond [(list? keys)
                       (let ([keys-v (list->vector keys)])
                         (values keys-v (vector-length keys-v)))]
                      [(vector? keys)
                       (values keys (vector-length keys))]
                      [(heap? keys)
                       (values (heap-vec keys) (heap-count keys))])])
    (match h
      [(heap vec size <=? notify)
       (let* ([new-size (+ size keys-size)]
              [vec (if (> new-size (vector-length vec))
                       (let ([vec (grow-vector vec new-size)])
                         (set-heap-vec! h vec)
                         vec)
                       vec)])
         (vector-copy! vec size keys 0 keys-size)
	 (for ([n (in-range size (+ size keys-size))])
	      (notify (vector-ref vec n) n))
         (for ([n (in-range size new-size)])
           (heapify-up <=? vec n notify))
         (set-heap-count! h new-size))])))

(define (heap-min h)
  (match h
    [(heap vec size <=? notify)
     (when (zero? size)
       (error 'heap-min "empty heap"))
     (vector-ref vec 0)]))

(define (heap-remove-min! h)
  (match h
    [(heap vec size <=? notify)
     (when (zero? size)
       (error 'heap-remove-min! "empty heap"))
     (heap-remove-index! h 0)]))

(define (heap-remove-index! h index)
  (match h
    [(heap vec size <=? notify)
     (unless (< index size)
       (if (zero? size)
           (error 'heap-remove-index!
                  "empty heap: ~s" index)
           (error 'heap-remove-index!
                  "index out of bounds [0,~s]: ~s" (sub1 size) index)))
     (define sub1-size (sub1 size))
     (vector-set! vec index (vector-ref vec sub1-size))
     (notify (vector-ref vec sub1-size) index)
     (vector-set! vec sub1-size #f)
     (cond
       [(= sub1-size index)
        ;; easy to remove the right-most leaf
        (void)]
       [(= index 0)
        ;; can only go down when at the root
        (heapify-down <=? vec index sub1-size notify)]
       [else
        (define index-parent (vt-parent index))
        (cond
          ;; if we are in the right relationship with our parent,
          ;; try to heapify down
          [(<=? (vector-ref vec index-parent) (vector-ref vec index))
           (heapify-down <=? vec index sub1-size notify)]
          [else
           ;; otherwise we need to heapify up
           (heapify-up <=? vec index notify)])])
     (when (< MIN-SIZE size (quotient (vector-length vec) 4))
       (set-heap-vec! h (shrink-vector vec)))
     (set-heap-count! h sub1-size)]))

(define (heap-get-index h v same?)
  (match h
    [(heap vec size <=? notify)
     (and (not (eq? 0 size))
          (let search ([n 0] [n-key (vector-ref vec 0)])
            (cond
             [(same? n-key v) n]
             ;; The heap property ensures n-key <= all its children
             [else
              (define (search-right)
                (define right (vt-rightchild n))
                (and (< right size)
                     (let ([right-key (vector-ref vec right)])
                       (and (<=? right-key v)
                            (search right right-key)))))
              ;; Try going left if the left child is <= v
              (define left (vt-leftchild n))
              (and (< left size) ;; if no left, there can't be a right.
                   (let ([left-key (vector-ref vec left)])
                     ;; If left <= v, try left side.
                     (if (<=? left-key v)
                         (or (search left left-key) (search-right))
                         (search-right))))])))]))

(define (heap-remove! h v #:same? [same? equal?])
  (match (heap-get-index h v same?)
    [#f (void)]
    [n (heap-remove-index! h n)]))

(define (in-heap h)
  (in-heap/consume! (heap-copy h)))

(define (in-heap/consume! h)
  (make-do-sequence
   (lambda ()
     (values (lambda (_) (heap-min h))
             (lambda (_) (heap-remove-min! h) #t)
             #t
             (lambda (_) (> (heap-count h) 0))
             (lambda _ #t)
             (lambda _ #t)))))

;; --------

;; preferred order is (heap-sort vec <=?), but allow old order too
(define (heap-sort! x y [notify dont-notify])
  (cond [(and (vector? x) (procedure? y))
         (heap-sort!* x y notify)]
        [(and (vector? y) (procedure? x))
         (heap-sort!* y x notify)]
        [else
         (unless (vector? x)
           (raise-argument-error 'heap-sort! "vector?" x))
         (raise-argument-error 'heap-sort! "procedure?" y)]))

(define (heap-sort!* v <=? notify)
  ;; to get ascending order, need max-heap, so reverse comparison
  (define (>=? x y) (<=? y x))
  (define size (vector-length v))
  (for ([n (in-range (sub1 size) -1 -1)])
    (heapify-down >=? v n size notify))
  (for ([last (in-range (sub1 size) 0 -1)])
    (let ([tmp (vector-ref v 0)])
      (vector-set! v 0 (vector-ref v last))
      (notify (vector-ref v last) 0)
      (vector-set! v last tmp)
      (notify tmp last)
      )
    (heapify-down >=? v 0 last notify)))

(define (heap->vector h)
  (match h
    [(heap vec size <=? notify)
     (let ([v (vector-copy vec 0 size)])
       (heap-sort!* v <=? notify)
       v)]))

(define (heap-decrease-key h index)
  (match h
    [(heap vec size <=? notify)
     (heapify-up <=? vec index notify)]))

;; --------

(provide/contract
 [make-heap (->* ((-> any/c any/c any/c)) ((-> any/c any/c any/c)) heap?)]
 [heap? (-> any/c boolean?)]
 [heap-count (-> heap? exact-nonnegative-integer?)]
 [heap-add! (->* (heap?) () #:rest list? void?)]
 [heap-add-all! (-> heap? (or/c list? vector? heap?) void?)]
 [heap-min (-> heap? any/c)]
 [heap-remove-min! (-> heap? void?)]
 [heap-remove! (->* (heap? any/c) [#:same? (-> any/c any/c any/c)] void?)]

 [vector->heap (->* ((-> any/c any/c any/c) vector?) 
		    (integer? integer? any/c) heap?)]
 [list->heap (->* ((-> any/c any/c any/c) list?) (any/c) heap?)]
 [heap->vector (-> heap? vector?)]
 [heap-copy (-> heap? heap?)]

 [in-heap (-> heap? sequence?)]
 [in-heap/consume! (-> heap? sequence?)]

 [heap-decrease-key (-> heap? exact-nonnegative-integer? void?)])

(provide heap-sort! heap-vec)

(module+ test-util
  (provide valid-heap?)
  (define (valid-heap? a-heap)
    (match a-heap
      [(heap vec size <=? notify)
       (let loop ([i 0]
                  [parent -inf.0])
         (cond
           [(< i size)
            (define this (vector-ref vec i))
            (and (<=? parent this)
                 (loop (vt-leftchild i) this)
                 (loop (vt-rightchild i) this))]
           [else #t]))])))
