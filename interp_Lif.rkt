#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "interp_Lvar.rkt")
(provide interp_Lif interp_Lif-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp_Lif-class
  (class interp_Lvar-class
    (super-new)

    (define/public (interp-op op)
      (verbose "Lif/interp-op" op)
      (match op
        ['+ fx+]
        ['- fx-]
        ['read read-fixnum]
        ['not (lambda (v) (match v [#t #f] [#f #t]))]
        ['eq? (lambda (v1 v2)
                (cond [(or (and (fixnum? v1) (fixnum? v2))
                           (and (boolean? v1) (boolean? v2))
                           (and (vector? v1) (vector? v2)))
                       (eq? v1 v2)]))]
        ['< (lambda (v1 v2)
              (cond [(and (fixnum? v1) (fixnum? v2))
                     (< v1 v2)]))]
        ['<= (lambda (v1 v2)
               (cond [(and (fixnum? v1) (fixnum? v2))
                      (<= v1 v2)]))]
        ['> (lambda (v1 v2)
              (cond [(and (fixnum? v1) (fixnum? v2))
                     (> v1 v2)]))]
        ['>= (lambda (v1 v2)
               (cond [(and (fixnum? v1) (fixnum? v2))
                      (>= v1 v2)]))]
        [else (error 'interp-op "unknown operator ~a" op)]
        ))

    (define/override ((interp_exp env) e)
      (define recur (interp_exp env))
      (match e
        [(Bool b) b]
        [(If cnd thn els)
         (define b (recur cnd))
         (match b
           [#t (recur thn)]
           [#f (recur els)])]
        [(Prim 'and (list e1 e2))
         (define v1 (recur e1))
         (match v1
           [#t (match (recur e2) [#t #t] [#f #f])]
           [#f #f])]
        [(Prim 'or (list e1 e2))
         (define v1 (recur e1))
         (match v1
           [#t #t]
           [#f (match (recur e2) [#t #t] [#f #f])])]
        [(Prim op args)
         (apply (interp-op op) (for/list ([e args]) (recur e)))]
        [else ((super interp_exp env) e)]
        ))
    ))

(define (interp_Lif p)
  (send (new interp_Lif-class) interp-program p))

#;(define (interp_exp env)
  (send (new interp_Lif-class) interp_exp env))
