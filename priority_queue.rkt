#lang racket
(require "heap.rkt")
(provide make-pqueue pqueue-push! pqueue-pop! pqueue-decrease-key! pqueue-count
	 set-node-key! node-key)

;; priority queue (smallest priority first)

;; TODO: add pretty printing

(struct node ([key #:mutable] [index #:mutable #:auto])
	#:methods gen:custom-write
	[(define (write-proc node port mode)
	   (fprintf port "(pq-node ~a)" (node-key node)))])

(define (make-node<=? <=?)
  (lambda (x y)
    (<=? (node-key x) (node-key y))))

(define (notify-node n index)
  (set-node-index! n index))

(define (make-pqueue <=? [init '()])
  (list->heap (make-node<=? <=?) init notify-node))

(define (pqueue-push! q key)
  (let ([n (node key)])
    (heap-add! q n)
    n))

(define (pqueue-pop! q)
  (let ([ret (heap-min q)])
    (heap-remove-min! q)
    (node-key ret)))

(define (pqueue-decrease-key! q node)
  (heap-decrease-key q (node-index node)))

(define (pqueue-count q)
  (heap-count q))
