#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "interp_Lvec.rkt")
(provide interp_Lvecof interp_Lvecof-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp_Lvecof-class
  (class interp_Lvec-class
    (super-new)

    (define/override (interp-op op)
      (verbose "Lvecof/interp-op" op)
      (match op
        ['make-vector make-vector]
        ['vectorof-length vector-length]
        ['vectorof-ref
         (lambda (v i)
           (if (< i (vector-length v))
               (vector-ref v i)
               (error 'trapped-error "vectorof-ref: index ~a out of bounds\nin ~v" i v)))]
        ['vectorof-set!
         (lambda (v i e)
           (if (< i (vector-length v))
               (vector-set! v i e)
               (error 'trapped-error "vectorof-set!: index ~a out of bounds\nin ~v" i v)))]
        ['* fx*]
        ['exit (lambda () (error 'interp "exiting"))]
        [else (super interp-op op)]))
    ))

(define (interp_Lvecof p)
  (send (new interp_Lvecof-class) interp-program p))
