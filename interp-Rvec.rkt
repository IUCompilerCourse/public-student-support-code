#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "interp-Rwhile.rkt")
(provide interp-Rvec interp-Rvec-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp-Rvec-class
  (class interp-Rwhile-class
    (super-new)

    (define/override (interp-op op)
      (verbose "Rvec/interp-op" op)
      (match op
        ['eq? (lambda (v1 v2)
                (cond [(or (and (fixnum? v1) (fixnum? v2))
                           (and (boolean? v1) (boolean? v2))
                           (and (vector? v1) (vector? v2))
                           (and (void? v1) (void? v2)))
                       (eq? v1 v2)]))]
        ['vector vector]
        ['vector-length vector-length]
        ['vector-ref vector-ref]
        ['vector-set! vector-set!]
        [else (super interp-op op)]
        ))

    (define/override ((interp-exp env) e)
      (define recur (interp-exp env))
      (verbose "Rvec/interp-exp" e)
      (match e
        [(HasType e t)  (recur e)]
        [else ((super interp-exp env) e)]
        ))
    ))

(define (interp-Rvec p)
  (send (new interp-Rvec-class) interp-program p))
