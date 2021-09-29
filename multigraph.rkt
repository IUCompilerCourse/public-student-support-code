#lang racket
(require graph)
(provide (all-defined-out))

(define (remove-all v lst)
  (cond [(null? lst) lst]
        [(equal? v (car lst)) (remove-all v (cdr lst))]
        [else (cons (car lst) (remove-all v (cdr lst)))]))

(struct multigraph (adj)
  #:methods gen:graph
  [(define (add-directed-edge! g u v [weight #f])
     (define adj (multigraph-adj g))
     (hash-update! adj u (lambda (vs) (cons v vs)) (list)))

   (define (remove-directed-edge! g u v)
     (define adj (multigraph-adj g))
     (hash-update! adj u (lambda (vs) (remove v vs)) (list)))
   
   (define (add-vertex! g u)
     (define adj (multigraph-adj g))
     (hash-update! adj u (lambda (vs) vs) (list)))
   
   (define (remove-vertex! g u)
     (define adj (multigraph-adj g))
     (hash-remove! adj u)
     (for ([(v vs) (in-hash adj)]) 
       (hash-set! adj v (remove-all u vs))))
   
   (define (in-vertices g)
     (in-hash-keys (multigraph-adj g)))

   (define (get-vertices g)
     (hash-keys (multigraph-adj g)))
   
   (define (in-neighbors g u)
     (hash-ref (multigraph-adj g) u))
   
   (define (get-neighbors g u)
     (hash-ref (multigraph-adj g) u))

   (define (vertex=? g u v)
     (equal? u v))

   (define (transpose g)
     (define g^T (multigraph (make-hash)))
     (for ([u (in-vertices g)])
       (add-vertex! g^T u))
     (for ([u (in-vertices g)])
       (for ([v (in-neighbors g u)])
         (add-directed-edge! g^T v u)))
     g^T)
   ])

(define (make-multigraph edge-list)
  (define adj (make-hash))
  (define g (multigraph adj))
  (for ([e edge-list])
    (add-vertex! g (first e))
    (add-vertex! g (second e))
    (add-directed-edge! g (first e) (second e)))
  g)
