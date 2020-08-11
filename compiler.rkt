#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-R0.rkt")
(require "interp-R1.rkt")
(require "interp.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; R0 examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following compiler pass is just a silly one that doesn't change
;; anything important, but is nevertheless an example of a pass. It
;; flips the arguments of +. -Jeremy
(define (flip-exp e)
  (match e
    [(? fixnum?) e]
    [`(read) `(read)]
    [`(- ,e1) `(- ,(flip-exp e1))]
    [`(+ ,e1 ,e2) `(+ ,(flip-exp e2) ,(flip-exp e1))]
    ))

(define (flip-R0 e)
  (match e
    [`(program ,e) `(program ,(flip-exp e))]
    ))


;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (cond [(fixnum? r) (fx- 0 r)]
	[else `(- ,r)]))

(define (pe-add r1 r2)
  (cond [(and (fixnum? r1) (fixnum? r2)) (fx+ r1 r2)]
	[else `(+ ,r1 ,r2)]))

(define (pe-exp e)
  (match e
    [(? fixnum?) e]
    [`(read) `(read)]
    [`(- ,e1) (pe-neg (pe-exp e1))]
    [`(+ ,e1 ,e2) (pe-add (pe-exp e1) (pe-exp e2))]
    ))

(define (pe-R0 p)
  (match p
    [`(program ,e) `(program ,(pe-exp e))]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (uniquify-exp symtab)
  (lambda (e)
    (match e
      [(? symbol?)
       (error "TODO: code goes here (uniquify-exp, symbol?)")]
      [(? integer?) e]
      [`(let ([,x ,e]) ,body)
       (error "TODO: code goes here (uniquify-exp, let)")]
      [`(,op ,es ...)
       `(,op ,@(for/list ([e es]) ((uniquify-exp symtab) e)))]
      )))

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [`(program ,info ,e)
     `(program ,info ,((uniquify-exp '()) e))]
    ))

;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (error "TODO: code goes here (remove-complex-opera*)"))

;; explicate-control : R1 -> C0
(define (explicate-control p)
  (error "TODO: code goes here (explicate-control)"))

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (error "TODO: code goes here (select-instructions)"))

;; assign-homes : pseudo-x86 -> pseudo-x86
(define (assign-homes p)
  (error "TODO: code goes here (assign-homes)"))

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
  (error "TODO: code goes here (patch-instructions)"))

;; print-x86 : x86 -> string
(define (print-x86 p)
  (error "TODO: code goes here (print-x86)"))
