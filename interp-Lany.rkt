#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "interp-Llambda.rkt")
(provide interp-Lany interp-Lany-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp-Lany-class
  (class interp-Llambda-class
    (super-new)

    (define/override (interp-op op)
      (verbose "Lany/interp-op" op)
      (match op
        ['boolean? (match-lambda [(Tagged v1 tg) (eq? tg (any-tag 'Boolean))])]
        ['integer? (match-lambda [(Tagged v1 tg) (eq? tg (any-tag 'Integer))])]
        ['vector? (match-lambda
                    [(Tagged v1 tg) (eq? tg (any-tag `(Vector Any)))])]
        ['procedure? (match-lambda
                       [(Tagged v1 tg) (eq? tg (any-tag `(Any -> Any)))])]
        ['void? (match-lambda [(Tagged v1 tg) (eq? tg (any-tag 'Void))])]
        ['eq? (match-lambda*
                [(list (Tagged v1^ tg1) (Tagged v2^ tg2))
                 (and (equal? tg1 tg2) ((interp-op 'eq?) v1^ v2^))]
                [ls (apply (super interp-op op) ls)])]
        ['any-vector-ref (lambda (v i)
                           (match v [(Tagged v^ tg) (vector-ref v^ i)]))]
        ['any-vector-set! (lambda (v i a)
                            (match v [(Tagged v^ tg) (vector-set! v^ i a)]))]
        ['any-vector-length (lambda (v)
                              (match v [(Tagged v^ tg) (vector-length v^)]))]
        ['any-vectorof-ref (lambda (v i)
                             (match v [(Tagged v^ tg) (vector-ref v^ i)]))]
        ['any-vectorof-set! (lambda (v i a)
                              (match v [(Tagged v^ tg) (vector-set! v^ i a)]))]
        ['any-vectorof-length (lambda (v)
                              (match v [(Tagged v^ tg) (vector-length v^)]))]
        [else (super interp-op op)]))

    (define/public (apply-inject v tg) (Tagged v tg))

    (define/public (apply-project v ty2)
      (define tag2 (any-tag ty2))
      (match v
        [(Tagged v1 tag1)
         (cond
           [(eq? tag1 tag2)
            (match ty2
              [`(Vector ,ts ...)
               (define l1 ((interp-op 'vector-length) v1))
               (cond
                 [(eq? l1 (length ts)) v1]
                 [else (error 'apply-project "vector length mismatch, ~a != ~a"
                              l1 (length ts))])]
              [`(,ts ... -> ,rt)
               (match v1
                 [(Function xs body env)
                  (cond [(eq? (length xs) (length ts)) v1]
                        [else
                         (error 'apply-project "arity mismatch ~a != ~a"
                                (length xs) (length ts))])]
                 [else (error 'apply-project "expected function not ~a" v1)])]
              [else v1])]
           [else (error 'apply-project "tag mismatch ~a != ~a" tag1 tag2)])]
        [else (error 'apply-project "expected tagged value, not ~a" v)]))
    
    (define/override ((interp-exp env) e)
      (define recur (interp-exp env))
      (verbose "Lany/interp-exp" e)
      (match e
        [(Inject e ty) (apply-inject (recur e) (any-tag ty))]
        [(Project e ty2)  (apply-project (recur e) ty2)]
        [else ((super interp-exp env) e)]))
    ))

(define (interp-Lany p)
  (send (new interp-Lany-class) interp-program p))
