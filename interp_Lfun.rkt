#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "interp_Lvecof.rkt")
(provide interp_Lfun interp_Lfun-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp_Lfun-class
  (class interp_Lvecof-class
    (super-new)

    (define/public (apply-fun fun-val arg-vals e)
      (match fun-val
        [(Function xs body fun-env)
         (define params-args (for/list ([x xs] [arg arg-vals])
                               (cons x (box arg))))
         (define new-env (append params-args fun-env))
         ((interp_exp new-env) body)]
        [else (error 'interp_exp "expected function, not ~a\nin ~v"
                     fun-val e)]))
    
    (define/override ((interp_exp env) e)
      (define recur (interp_exp env))
      (verbose "Lfun/interp_exp" e)
      (match e
        [(Apply fun args)
         (define fun-val (recur fun))
         (define arg-vals (for/list ([e args]) (recur e)))
         (apply-fun fun-val arg-vals e)]
        [else ((super interp_exp env) e)]))

    (define/public (interp-def d)
      (match d
        [(Def f (list `[,xs : ,ps] ...) rt _ body)
         (cons f (box (Function xs body '())))]
        ))

    (define/override (interp-program p)
      (verbose "interp_Lfun" p)
      (match p
        [(ProgramDefsExp info ds body)
         (let ([top-level (for/list ([d ds]) (interp-def d))])
           (for/list ([f (in-dict-values top-level)])
             (set-box! f (match (unbox f)
                           [(Function xs body '())
                            (Function xs body top-level)])))
           ((interp_exp top-level) body))]
        
        ;; For after the shrink pass.
        [(ProgramDefs info ds)
         (define top-level (for/list ([d ds]) (interp-def d)))
         (for ([f (in-dict-values top-level)])
           (set-box! f (match (unbox f)
                         [(Function xs body '())
                          (Function xs body top-level)])))
         ;; call the main function
         ((interp_exp top-level) (Apply (Var 'main) '()))]
        ))
    ))

(define (interp_Lfun p)
  (send (new interp_Lfun-class) interp-program p))
