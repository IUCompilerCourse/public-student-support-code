#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "interp-Lvecof.rkt")
(provide interp-Lfun interp-Lfun-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp-Lfun-class
  (class interp-Lvecof-class
    (super-new)

    (define/public (apply-fun fun-val arg-vals e)
      (match fun-val
        [(Function xs body fun-env)
         (define params-args (for/list ([x xs] [arg arg-vals])
                               (cons x (box arg))))
         (define new-env (append params-args fun-env))
         ((interp-exp new-env) body)]
        [else (error 'interp-exp "expected function, not ~a\nin ~v"
                     fun-val e)]))
    
    (define/override ((interp-exp env) e)
      (define recur (interp-exp env))
      (verbose "Lfun/interp-exp" e)
      (match e
        [(Apply fun args)
         (define fun-val (recur fun))
         (define arg-vals (for/list ([e args]) (recur e)))
         (apply-fun fun-val arg-vals e)]
        [else ((super interp-exp env) e)]))

    (define/public (interp-def d)
      (match d
        [(Def f (list `[,xs : ,ps] ...) rt _ body)
         (cons f (box (Function xs body '())))]
        ))

    (define/override (interp-program p)
      (verbose "interp-Lfun" p)
      (match p
        [(ProgramDefsExp info ds body)
         (let ([top-level (for/list ([d ds]) (interp-def d))])
           (for/list ([f (in-dict-values top-level)])
             (set-box! f (match (unbox f)
                           [(Function xs body '())
                            (Function xs body top-level)])))
           ((interp-exp top-level) body))]
        
        ;; For after the shrink pass.
        [(ProgramDefs info ds)
         (define top-level (for/list ([d ds]) (interp-def d)))
         (for ([f (in-dict-values top-level)])
           (set-box! f (match (unbox f)
                         [(Function xs body '())
                          (Function xs body top-level)])))
         ;; call the main function
         ((interp-exp top-level) (Apply (Var 'main) '()))]
        ))
    ))

(define (interp-Lfun p)
  (send (new interp-Lfun-class) interp-program p))
