#lang racket
(require racket/fixnum)
(require "utilities.rkt" (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-F1 interp-F2 interp-F3 interp-F4
         interp-pseudo-x86-0 interp-x86-0
         interp-pseudo-x86-1 interp-x86-1
         interp-pseudo-x86-2 interp-x86-2
         interp-pseudo-x86-3 interp-x86-3
         interp-pseudo-x86-4 interp-x86-4
         interp-pseudo-x86-5 interp-x86-5
         interp-R6-class-alt
         interp-pseudo-x86-python interp-x86-python
         )

;; The interpreters in this file are for the intermediate languages
;; produced by the various passes of the compiler.
;; 
;; The interpreters for the source languages (Lvar, Lif, ...)
;; and the C intermediate languages Cvar and Cif
;; are in separate files, e.g., interp-Rvar.rkt.

#;(define interp-R3-prime
  (lambda (p)
    ((send (new interp-R3-class) interp-scheme '()) p)))

(define interp-F1
  (lambda (p)
    ((send (new interp-R4-class) interp-F '()) p)))

(define interp-F2
  (lambda (p)
    ((send (new interp-R5-class) interp-F '()) p)))

(define interp-F3
  (lambda (p)
    ((send (new interp-R6-class-alt) interp-F '()) p)))

(define interp-F4
  (lambda (p)
    ((send (new interp-R8-class) interp-F '()) p)))

;; Interpreters for C2 and C3.

#;(define interp-C2
  (lambda (p)
    (send (new interp-R3-class) interp-C p)))
  
#;(define interp-C3
  (lambda (p)
    (send (new interp-R4-class) interp-C p)))

#;(define interp-C4
  (lambda (p)
    (send (new interp-R5-class) interp-C p)))

#;(define interp-C5
  (lambda (p)
    (send (new interp-R6-class-alt) interp-C p)))

#;(define interp-C7
  (lambda (p)
    (send (new interp-R8-class) interp-C p)))

;; Interpreters for x86 with names that correspond to the book.

;; TODO: update the following names!

(define interp-pseudo-x86-0
  (lambda (p)
    ((send (new interp-R1-class) interp-pseudo-x86 '()) p)))

(define interp-x86-0
  (lambda (p)
    ((send (new interp-R1-class) interp-x86 '()) p)))

(define interp-pseudo-x86-1
  (lambda (p)
    ((send (new interp-R2-class) interp-pseudo-x86 '()) p)))

(define interp-x86-1
  (lambda (p)
    ((send (new interp-R2-class) interp-x86 '()) p)))

(define interp-pseudo-x86-2
  (lambda (p)
    ((send (new interp-R3-class) interp-pseudo-x86 '()) p)))

;; The interp-x86-2 interpreter takes a program of the form
;; (X86Program info blocks)
;; Also, the info field must be an association list
;; with a key 'num-root-spills whose values is 
;; the number of spills to the root stack.
(define interp-x86-2
  (lambda (p)
    ((send (new interp-R3-class) interp-x86 '()) p)))

(define interp-pseudo-x86-3
  (lambda (p)
    ((send (new interp-R4-class) interp-pseudo-x86 '()) p)))

;; The interp-x86-3 interpreter requires that the info field of the
;; Def struct be an association list with a key 'num-root-spills whose
;; values is the number of spills to the root stack.
(define interp-x86-3
  (lambda (p)
    ((send (new interp-R4-class) interp-x86 '()) p)))

(define interp-pseudo-x86-4
  (lambda (p)
    ((send (new interp-R6-class-alt) interp-pseudo-x86 '()) p)))

(define interp-x86-4
  (lambda (p)
    ((send (new interp-R6-class-alt) interp-x86 '()) p)))

(define interp-pseudo-x86-5
  (lambda (p)
    ((send (new interp-gradual-class) interp-pseudo-x86 '()) p)))

(define interp-x86-5
  (lambda (p)
    ((send (new interp-gradual-class) interp-x86 '()) p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R1: integer arithmetic and 'let'

(define interp-R1-class
  (class object%
    (super-new)

    (field (result (gensym 'result)))

    ;; Hide details for debug output.
    (define/public (observe-value v)
      v)

    (define/public (return-from-tail v env)
      (cons (cons result v) env))

    (define/public (is-return? e)
      (match e
	[(cons (cons res v) env) (equal? res result)]
	[else #f]))

    (define/public (primitives)
      (set '+ '- 'read))

    (define/public (interp-op op)
      (match op
	 ['+ fx+]
	 ['- fx-]
	 ['read read-fixnum]
	 [else (error "in interp-op R1, unmatched" op)]))

    (define/public (interp-scheme-exp env)
      (lambda (ast)
        (define recur (interp-scheme-exp env))
	(verbose "R1/interp-scheme-exp" ast)
	(match ast
           [(Var x)
	    (lookup x env)]
	   [(Int n) n]
	   [(Let x e body)
            (define v (recur e))
	    ((interp-scheme-exp (cons (cons x v) env)) body)]
	   [(Prim op args)
	    (apply (interp-op op)
                   (for/list ([e args]) (recur e)))]
	   [else
	    (error (format "R1/no match in interp-scheme-exp for ~a" ast))]
	   )))

    (define/public (interp-scheme env)
      (lambda (ast)
	(verbose "R1/interp-scheme" ast)
	(match ast
	   [(Program _ e)
            ((interp-scheme-exp '()) e)]
	   [else
	    (error (format "R1/no match in interp-scheme for ~a" ast))]
	   )))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; C0

    (define/public (interp-C-exp env)
      (lambda (ast)
        (define result
        (match ast
          [(Var x) (lookup x env)]
          [(Int n) n]
          [(Prim op args)
	   (apply (interp-op op) (map (interp-C-exp env) args))]
          [else
           (error "C0/interp-C-exp unhandled" ast)]
          ))
        (verbose "C0/interp-C-exp" ast result)
        result))
          
    (define/public (interp-C-tail env)
      (lambda (ast)
        (match ast
          [(Return e)
           ((interp-C-exp env) e)]
          ;; (return-from-tail v env)  hmm -Jeremy
          [(Seq s t)
           (define new-env ((interp-C-stmt env) s))
           ((interp-C-tail new-env) t)]
          [else
           (error "interp-C-tail unhandled" ast)]
          )))
    
    (define/public (interp-C-stmt env)
      (lambda (ast)
        (verbose "C0/interp-C-stmt" ast)
        (match ast
          [(Assign (Var x) e)
           (let ([v ((interp-C-exp env) e)])
             (cons (cons x v) env))]
          [(Prim op args)
           ((interp-C-exp env) ast)
           env]
          [else
           (error "interp-C-stmt unhandled" ast)]
          )))
          
    (define/public (interp-C ast)
      (debug "R1/interp-C" ast)
      (match ast
        [(CProgram info blocks)
         (define start (dict-ref blocks 'start))
         ((interp-C-tail '()) start)]
        [else (error "no match in interp-C for " ast)]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; psuedo-x86 and x86
    ;; s,d ::= (var x) | (int n) | (reg r) | (deref r n)
    ;; i   ::= (movq s d) | (addq s d) | (subq s d) | (imulq s d)
    ;;       | (negq d) | (callq f)
    ;; psuedo-x86 ::= (program info i ...)

    (define/public (get-name ast)
      (match ast
	[(or (Var x) (Reg x)) x]
	[(Deref 'rbp n) n]
	[else
	 (error 'interp-R1-class/get-name "doesn't have a name: ~a" ast)]))

    (field [x86-ops (make-immutable-hash
		     `((addq 2 ,+)
		       (imulq 2 ,*)
		       (subq 2 ,(lambda (s d) (- d s)))
		       (negq 1 ,-)))])

    (define/public (interp-x86-op op)
      (define (err)
	(error 'interp-R1-class/interp-x86-op "unmatched ~a" op))
      (cadr (hash-ref x86-ops op err)))

    (define/public (interp-x86-exp env)
      (lambda (ast)
	(copious "interp-x86-exp" ast)
        (define result
	(match ast
	   [(or (Var x) (Reg x))
	    (lookup (get-name ast) env)]
	   [(Deref r n)
	    (lookup (get-name ast) env)]
	   [(Imm n) n]
	   [else
	    (error 'interp-R1-class/interp-x86-exp "unhandled ~a" ast)]))
        (copious "R1/interp-x86-exp" (observe-value result))
        result))

    (define/public (interp-x86-instr env)
      (lambda (ast)
        (when (pair? ast)
          (copious "R1/interp-x86-instr" (car ast)))
        (match ast
	   ['() env]
	   [(cons (Callq 'read_int _) ss)
            (let ([v (read)])
              (copious "read " v)
              ((interp-x86-instr (cons (cons 'rax v) env)) ss))]
           [(cons (Instr 'movq (list s d)) ss)
            (define x (get-name d))
	    (define v ((interp-x86-exp env) s))
            (copious "move " (observe-value v))
	    ((interp-x86-instr (cons (cons x v) env)) ss)]
           [(cons (Jmp conclusion) ss)
            #:when (string-suffix? (symbol->string conclusion) "conclusion")
            env]
           [(cons (Jmp label) ss)
            ((interp-x86-block env) (goto-label label))]
	   [(X86Program info ss)
	    (let ([env ((interp-x86-instr '()) ss)])
              (lookup 'rax env))]
	   [(cons (Instr binary-op (list s d)) ss)
	    (let ([s ((interp-x86-exp env) s)]
		  [d ((interp-x86-exp env) d)]
		  [x (get-name d)]
		  [f (interp-x86-op binary-op)])
              (let ([v (f s d)])
                (copious "binary-op result " (observe-value v))
                ((interp-x86-instr (cons (cons x v) env)) ss)))]
	   [(cons (Instr unary-op (list d)) ss)
	    (let ([d ((interp-x86-exp env) d)]
		  [x (get-name d)]
		  [f (interp-x86-op unary-op)])
              (let ([v (f d)])
                (copious "unary-op result " (observe-value v))
                ((interp-x86-instr (cons (cons x v) env)) ss)))]
	   [else (error "R1/interp-x86-instr no match for" ast)]
	   )))

    (define/public (interp-pseudo-x86 env)
      (lambda (ast)
        ((interp-x86 env) ast)))

    (define/public (interp-x86-block env)
      (lambda (ast)
        (match ast
          [(Block info ss)
           ((interp-x86-instr env) ss)]
          [else
           (error "R1/interp-x86-block unhandled" ast)])))
      
    (define/public (interp-x86 env)
      (lambda (ast)
        (when (pair? ast)
          (copious "R1/interp-x86" (car ast)))
        (match ast
          [(X86Program info blocks)
           (parameterize ([get-basic-blocks blocks])
             (define start-block (dict-ref blocks 'start))
             (define result-env ((interp-x86-block '()) start-block))
             (lookup 'rax result-env))]
          [else (error "R1/interp-x86 no match in for" ast)]
          )))
    
    )) ;; class interp-R1-class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R2: Booleans and conditionals

(define interp-R2-class
  (class interp-R1-class
    (super-new)

    (inherit interp-x86-block observe-value)
    (inherit-field x86-ops)

    ;; We do not include 'and' because it has a funky order of evaluation.
    ;; -Jeremy
    (define/override (primitives)
      (set-union (super primitives)
		 (set 'eq? 'not 'or '< '<= '> '>=)))

    (define/override (interp-op op)
      (match op
	 ['eq? (lambda (v1 v2)
		 (cond [(and (fixnum? v1) (fixnum? v2)) (eq? v1 v2)]
		       [(and (boolean? v1) (boolean? v2)) (eq? v1 v2)]
                       [else (error 'interp-op "unhandled case")]))]
	 ['and (lambda (v1 v2)
		 (cond [(and (boolean? v1) (boolean? v2))
			(and v1 v2)]
                       [else (error 'interp-op "unhandled case")]
                       ))]
	 ['not (lambda (v) (match v
                             [#t #f] [#f #t]
                             [else (error 'interp-op "unhandled case")]))]
         ['or (lambda (v1 v2)
                (cond [(and (boolean? v1) (boolean? v2))
                       (or v1 v2)]
                      [else (error 'interp-op "unhandled case")]))]
	 ['< (lambda (v1 v2)
	       (cond [(and (fixnum? v1) (fixnum? v2)) (< v1 v2)]
                     [else (error 'interp-op "unhandled case")]
                     ))]
	 ['<= (lambda (v1 v2)
	       (cond [(and (fixnum? v1) (fixnum? v2)) (<= v1 v2)]
                     [else (error 'interp-op "unhandled case")]
                     ))]
	 ['> (lambda (v1 v2)
	       (cond [(and (fixnum? v1) (fixnum? v2)) (> v1 v2)]
                     [else (error 'interp-op "unhandled case")]
                     ))]
	 ['>= (lambda (v1 v2)
	       (cond [(and (fixnum? v1) (fixnum? v2)) (>= v1 v2)]
                     [else (error 'interp-op "unhandled case")]
                     ))]
	 [else (super interp-op op)]))

    (define/override (interp-scheme-exp env)
      (lambda (ast)
        (define recur (interp-scheme-exp env))
	(verbose "R2/interp-scheme-exp" ast)
	(match ast
          [(HasType e t) (recur e)]
          [(Bool b) b]
          [(Prim 'and (list e1 e2))
           (match (recur e1)
             [#t (match (recur e2)
                   [#t #t] [#f #f])]
             [#f #f])]
          [(If cnd thn els)
           (match (recur cnd) 
             [#t (recur thn)]
             [#f (recur els)]
             [else
              (error 'interp-scheme-exp "R2 expected Boolean, not ~a" cnd)])]
          [else ((super interp-scheme-exp env) ast)]
          )))

    (define/override (interp-C-exp env)
      (lambda (ast)
        (define result
	(match ast
          [(HasType e t) ((interp-C-exp env) e)]
          [(Bool b) b]
          [else ((super interp-C-exp env) ast)]
          ))
        (copious "R2/interp-C-exp" ast result)
        result))

    (define/override (interp-C-tail env)    
      (lambda (ast)
	(copious "R2/interp-C-tail" ast)
	(match ast
          [(IfStmt cnd thn els)
           (if ((interp-C-exp env) cnd)
               ((interp-C-tail env) thn)
               ((interp-C-tail env) els))]
          [(Goto label)
           ((interp-C-tail env) (goto-label label))]
          [else ((super interp-C-tail env) ast)]
          )))
      
    (define/override (interp-C ast)
      (copious "R2/interp-C" ast)
      (match ast
        [(CProgram info blocks)
         (parameterize ([get-basic-blocks blocks])
           (super interp-C (CProgram info blocks)))]
        [else (error "R2/interp-C unhandled" ast)]
        ))

    (define byte2full-reg
      (lambda (r)
	(match r
	   ['al 'rax]
	   ['bl 'rbx]
	   ['cl 'rcx]
	   ['dl 'rdx]
	   )))

    (define/override (get-name ast)
      (match ast
        [(ByteReg r)
         (super get-name (Reg (byte2full-reg r)))]
        [else (super get-name ast)]))
    
    ;; Extending the set of known operators is essentially the
    ;; same as overriding the interp-x86-op with new functionallity
    (set! x86-ops (hash-set* x86-ops
		   'notq `(1 ,bitwise-not)
		   'andq `(2 ,bitwise-and)
		   'xorq `(2 ,bitwise-xor)))

    (define/override (interp-x86-exp env)
      (lambda (ast)
        (copious "R2/interp-x86-exp" ast)
        (define result
	(match ast
	  [(ByteReg r)
	   ((interp-x86-exp env) (Reg (byte2full-reg r)))]
	  [(Bool #t) 1]
	  [(Bool #f) 0]
	  [(Prim 'eq? (list e1 e2))
	   (if (eq? ((interp-x86-exp env) e1)
		    ((interp-x86-exp env) e2))
	       1 0)]
	  [(Prim '< (list e1 e2))
	   (if (< ((interp-x86-exp env) e1)
		    ((interp-x86-exp env) e2))
	       1 0)]
	  [(Prim '<= (list e1 e2))
	   (if (<= ((interp-x86-exp env) e1)
		    ((interp-x86-exp env) e2))
	       1 0)]
	  [(Prim '> (list e1 e2))
	   (if (> ((interp-x86-exp env) e1)
		    ((interp-x86-exp env) e2))
	       1 0)]
	  [(Prim '>= (list e1 e2))
	   (if (>= ((interp-x86-exp env) e1)
		    ((interp-x86-exp env) e2))
	       1 0)]
	  [else ((super interp-x86-exp env) ast)]
	  ))
        (copious "R2/interp-x86-exp" (observe-value result))
        result))

    (define/public (eflags-status env cc)
      (define eflags ((interp-x86-exp env) (Reg '__flag)))
      (match cc
	 ['e (if (equal? eflags 'equal) 1 0)]
	 ['l (if (equal? eflags 'less) 1 0)]
	 ['le
	  (if (or (eq? 1 (eflags-status env 'e))
		  (eq? 1 (eflags-status env 'l)))
	      1 0)]
	 ['g
	  (if (not (eq? 1 (eflags-status env 'le)))
	      1 0)]
	 ['ge
	  (if (not (eq? 1 (eflags-status env 'l)))
	      1 0)]))

    (define/override (interp-x86-instr env)
      (lambda (ast)
        (when (pair? ast)
	      (copious "R2/interp-x86-instr" (car ast)))
        (match ast
          [(Block `(lives ,lives) ss)
           ((interp-x86-instr env) ss)]
          [(Block `(lives ,lives) ss)
           ((interp-x86-instr env) ss)]
          [(cons (Instr 'set (list cc d)) ss)
           (define name (get-name d))
	   (define val (eflags-status env cc))
	   (verbose "set" cc val)
           ((interp-x86-instr (cons (cons name val) env)) ss)]
          [(cons (IfStmt cnd thn els) ss)
           (if (not (eq? 0 ((interp-x86-exp env) cnd)))
               ((interp-x86-instr env) (append thn ss))
               ((interp-x86-instr env) (append els ss)))]
          ;; Notice that the argument order of cmpq is confusing:
          ;; (cmpq ,s2 ,s1) (jl thn) (jmp els)
          ;; is eqivalent to
          ;; (if (< s1 s2) thn els)
          [(cons (Instr 'cmpq (list s2 s1)) ss)
           (let* ([v1 ((interp-x86-exp env) s1)]
                  [v2 ((interp-x86-exp env) s2)]
                  [eflags 
                   (cond [(< v1 v2) 'less]
                         [(> v1 v2) 'greater]
                         [else 'equal])])
             ((interp-x86-instr (cons (cons '__flag eflags) env)) ss))]

          [(cons (Instr 'movzbq (list s d)) ss)
           (define x (get-name d))
           (define v ((interp-x86-exp env) s))
           ((interp-x86-instr (cons (cons x v) env)) ss)]
          [(cons (JmpIf cc label) ss)
	   (cond [(eq? (eflags-status env cc) 1)
		  ((interp-x86-block env) (goto-label label))]
		 [else ((interp-x86-instr env) ss)])]
          [else ((super interp-x86-instr env) ast)]
          )))

    ));; class interp-R2-class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R3: Vectors

(define interp-R3-class
  (class interp-R2-class
    (super-new)
    (inherit get-name interp-x86-op interp-x86-block observe-value)
    (inherit-field x86-ops)

    ;; The simulated global state of the program
    ;; define produces private fields
    (define memory (box '()))
    ;; field is like define but public
    (field [stack-size (runtime-config:rootstack-size)]
	   [heap-size  (runtime-config:heap-size)]
	   [uninitialized 'uninitialized-value-from-memory]
	   [fromspace_begin (box uninitialized)]
	   [rootstack_end   (box uninitialized)]
	   [free_ptr	    (box uninitialized)]
	   [fromspace_end   (box uninitialized)]
	   [rootstack_begin (box uninitialized)]
	   [global-label-table
	    (make-hash
	     `((free_ptr	 . ,free_ptr)
	       (fromspace_begin	 . ,fromspace_begin)
	       (fromspace_end	 . ,fromspace_end)
	       (rootstack_begin	 . ,rootstack_begin)
	       (rootstack_end	 . ,rootstack_end)))])

    (define/public (memory-read)
      (lambda (addr)
	(let-values ([(start stop name vect) (fetch-page addr)])
	  (let ([value (vector-ref vect (arithmetic-shift (- addr start) -3))])
	    (when (equal? value uninitialized)
	      (error 'interp-R3-class/memory-read
		     "read uninitialized memory at address ~s"
		     addr))
	    value))))

    (define/public (memory-write!)
      (lambda (addr value)
	(let-values ([(start stop name vect) (fetch-page addr)])
	  (vector-set! vect (arithmetic-shift (- addr start) -3) value))))

    (define/public (collect!)
      (lambda (rootset bytes-requested)
	(verbose "collect!" bytes-requested)
	;; after a call to collect we must guarantee there is enough
	;; memory to allocate the requested block of memory
	(let double-heap ([hs heap-size])
	  (if (< hs bytes-requested)
	      (double-heap (* 2 hs))
	      (let ((h-begin (allocate-page! 'fromspace hs)))
		;; I am only advancing the end of the heap because we
		;; are not reclaiming memory
		(set-box! fromspace_end	  (+ h-begin hs))
		(set-box! free_ptr	  h-begin))))))

    (define/public (initialize!)
      (lambda (stack-length heap_length)
	(verbose "initialize!")
	(set-box! memory '())
	(let* ([s-begin (allocate-page! 'rootstack stack-size)]
	       [h-begin (allocate-page! 'fromspace heap-size)])
	  (set-box! rootstack_begin s-begin)
	  (set-box! rootstack_end   (+ s-begin stack-size))
	  (set-box! fromspace_begin h-begin)
	  (set-box! fromspace_end   (+ h-begin heap-size))
	  (set-box! free_ptr	    h-begin))))

    (define (allocate-page! name size)
      (verbose "allocate-page!" name size)
      (unless (and (fixnum? size)
		   (positive? size)
		   (= 0 (modulo size 8)))
	(error 'allocate-page! "expected non-negative fixnum in ~a" size))
      ;; Find the last address
      (define max-addr
	(for/fold ([next 8])
		  ([page (in-list (unbox memory))])
	  (match-let ([`(page ,_ ,stop ,_ ,_) page])
	    (max next stop))))
      ;; Allocate with a small pad 100 words so that it isn't likely to
      ;; accidentally use another region.
      ;; The randomness is to dispell any reliance on interp always allocating
      ;; the same way. -Andre
      (define start-addr (+ max-addr 800))
      ;; The range is of valid addresses in memory are [start, stop)
      (define stop-addr (+ start-addr size))
      (define vect (make-vector (arithmetic-shift size -3) uninitialized))
      (verbose "allocated" name start-addr stop-addr)
      (set-box! memory (cons `(page ,start-addr ,stop-addr ,name ,vect)
			     (unbox memory)))
      start-addr)

    (define (free! addr)
      (set-box! memory
	(let loop ([memory (unbox memory)])
	  (match memory
	    [`() (error 'free "invalid address ~a, not currently allocated")]
	    [`(,(and page `(page ,ptr ,_ ,_ ,_)) . ,pages)
	     (if (= addr ptr)
		 pages
		 (cons page (loop pages)))]))))

    (define (fetch-page addr)
      ;; Create a string containing
      (define (fmt-err addr memory)
	(apply
	 string-append
	 (cons (format "address ~a out of bounds\n\tcurrent memory regions:\n"
		       addr)
	       (for/list ([page (in-list (unbox memory))])
		 (match-let ([`(page ,start ,stop ,name ,_) page])
		   (format "\t\t~a\t\t[~a,~a)\n" name start stop))))))
      (unless (fixnum? addr)
	(error 'fetch-page "invalid address ~a, not a fixnum" addr))
      (unless (positive? addr)
	(error 'fetch-page "invalid address ~a, negative" addr))
      (unless (= 0 (modulo addr 8))
	(error 'fetch-page "invalid address ~a, not 8-byte aligned" addr))
      (let search ([m (unbox memory)])
        (match m
          [`() (error 'fetch-page (fmt-err addr memory))]
          [`((page ,min ,max ,name ,vect) . ,rest-memory)
           ;(copious "R3/fetch page" addr min max name vect)
           ; vect is too large to print, makes things hard to read.
           ;(copious "R3/fetch page" addr min max name)
           (if (and (<= min addr) (< addr max))
               (values min max name vect)
               (search rest-memory))]
          [other (error 'fetch-page "unmatched ~a" m)])))

    (define/override (primitives)
      (set-union (super primitives)
		 (set 'vector  'vector-ref 'vector-set! vector-length
              ;; todo: move the following to a different interpreter -Jeremy
                      'vector-proxy)))

    (define/override (interp-op op)
      (match op
	['eq? (lambda (v1 v2)
		(cond [(or (and (fixnum? v1) (fixnum? v2))
			   (and (boolean? v1) (boolean? v2))
			   (and (vector? v1) (vector? v2))
                           (and (void? v1) (void? v2)))
		       (eq? v1 v2)]))]
        ['vector-proxy
         (lambda (vec rs ws)
           `(vector-proxy ,vec ,rs ,ws))]
        ['vector vector]
	['vector-length vector-length]
	['vector-ref vector-ref]
	['vector-set! vector-set!]
	#;['vector-proxy-set! vector-set!]
	[else (super interp-op op)]))

    #;(define/public (scheme-vector-ref vec i)
      (match vec
        [`(vector-proxy ,v ,rs ,ws)
         (define v^ (scheme-vector-ref v i))
         (define r (vector-ref rs i))
         (apply-fun (lambda (env) (interp-scheme-exp env)) 
                    r (list v^))]
        [else
         (vector-ref vec i)]))

    #;(define/public (scheme-vector-set! vec i arg)
      (match vec
        [`(vector-proxy ,v ,rs ,ws)
         (define w (vector-ref ws i))
         (define arg^ (apply-fun (lambda (env) (interp-scheme-exp env))
                                 w (list arg)))
         (scheme-vector-set! v i arg^)]
        [else
         (vector-set! vec i arg)]))

    (define/override (interp-scheme-exp env)
      (lambda (ast)
        (define recur (interp-scheme-exp env))
	(verbose "R3/interp-scheme" ast)
	(match ast
	  [(Void) (void)]
	  [(GlobalValue 'free_ptr)
	   (unbox free_ptr)]
	  [(GlobalValue 'fromspace_end)
	   (unbox fromspace_end)]
          [(Allocate l ty) (build-vector l (lambda a uninitialized))]
          [(AllocateClosure l ty arity)
           (define vec (build-vector (add1 l) (lambda a uninitialized)))
           (vector-set! vec l `(arity ,arity))
           vec]
          [(AllocateProxy ty) (build-vector 3 (lambda a uninitialized))]
          [(Collect size)
           (unless (exact-nonnegative-integer? size)
             (error 'interp-C "invalid argument to collect in ~a" ast))
           (void)]
          #;[`(vector-ref ,e-vec ,e-i)
           (define vec (recur e-vec))
           (define i (recur e-i))
           (scheme-vector-ref vec i)]
          #;[`(vector-set! ,e-vec ,e-i ,e-arg)
           (define vec (recur e-vec))
           (define i (recur e-i))
           (define arg (recur e-arg))
           (scheme-vector-set! vec i arg)]
	  [else ((super interp-scheme-exp env) ast)]
	  )))

    (define/override (interp-scheme env)
      (lambda (ast)
	(verbose "R3/interp-scheme" ast)
	(match ast
          [(Program info e)
	   ((initialize!) runtime-config:rootstack-size
                          runtime-config:heap-size)
	   ((interp-scheme-exp '()) e)]
	  [else ((super interp-scheme env) ast)]
	  )))

    (define (mem-error message expr)
      (lambda (who fmt . args)
	(error who "~a in ~a raise error:\n~a"
	       message expr
	       (apply format (cons fmt args)))))

    (define (global-value-err ast)
      (lambda ()
	(error 'interp-R3-class "global label is unknown in ~a" ast)))


    (define/public (fetch-global label)
      (let* ([err (global-value-err label)]
	     [ref (hash-ref global-label-table label err)]
	     [value (unbox ref)])
	(when (equal? value uninitialized)
	  (debug "fetch" global-label-table)
	  (error 'interp-R3-class/fetch-global
		 "global value, ~a, used before initialization"
		 label))
	value))

    (define/override (interp-C-exp env)
      (lambda (ast)
        (define result
        (match ast
          [(Void) (void)]
	  [(GlobalValue 'free_ptr)
	   (unbox free_ptr)]
	  [(GlobalValue 'fromspace_end)
	   (unbox fromspace_end)]
	  [(CollectionNeeded? size)
	   (when (or (eq? (unbox free_ptr) uninitialized)
		     (eq? (unbox fromspace_end) uninitialized))
	     (error 'interp-C "uninitialized state in ~a" ast))
	   #t]
	  ;; allocate a vector of length l and type t that is initialized.
	  [(Allocate l ty) (build-vector l (lambda a uninitialized))]
          [(AllocateClosure l ty arity)
           (define vec (build-vector (add1 l) (lambda a uninitialized)))
           (vector-set! vec l `(arity ,arity))
           vec]
	  #;[(AllocateClosure l ty arity)
           (build-vector l (lambda a uninitialized))]
	  [(AllocateProxy ty) (build-vector 3 (lambda a uninitialized))]
          [else
           ((super interp-C-exp env) ast)]
          ))
        (copious "R3/interp-C-exp" ast result)
        result))


    (define/override (interp-C-stmt env)
      (lambda (ast)
        (copious "R3/interp-C-stmt" ast)
        (match ast
	  ;; Determine if a collection is needed.
	  ;; Which it isn't because vectors stored in the environment
	  ;; is the representation of the heap in the C language,
	  ;; but collection is a no-op so we should check to see if
	  ;; everything is well formed anyhow.
	  ;; Collection isn't needed or possible in this representation
	  [(Collect size)
	   (unless (exact-nonnegative-integer? size)
	     (error 'interp-C "invalid argument to collect in ~a" ast))
	   env]
          [else
           ((super interp-C-stmt env) ast)]
          )))

    (define/override (interp-C-tail env)
      (lambda (ast)
        (copious "R3/interp-C-tail" ast)
        (match ast
          [(Seq s t)
           (define new-env ((interp-C-stmt env) s))
           ((interp-C-tail new-env) t)]
          [else ((super interp-C-tail env) ast)])))
    
    (define/override (interp-C ast)
      (copious "R3/interp-C" ast)
      (match ast
        [(CProgram info blocks)
         ((initialize!) runtime-config:rootstack-size
                        runtime-config:heap-size)
         (super interp-C (CProgram info blocks))]
        [else (error "R3/interp-C unhandled" ast)]))

    (define/override (interp-x86-exp env)
      (lambda (ast)
	(copious "interp-x86-exp" ast)
        (define result
	(match ast
	  [(Global label) (fetch-global label)]
	  [(Deref r i) #:when (not (eq? r 'rbp))
	   (define base ((interp-x86-exp env) (Reg r)))
	   (define addr (+ base i))
	   ((memory-read) addr)]
	  [else ((super interp-x86-exp env) ast)]))
        (copious "R3/interp-x86-exp" (observe-value result))
        result))

    (define/public (interp-x86-store env)
      (lambda (ast value)
	(copious "interp-x86-store" ast (observe-value value))
	(match ast
	  [(Global label)
	   (define loc (hash-ref global-label-table label
                                 (global-value-err ast)))
	   (set-box! loc value)
	   env]
	  [(Deref r i)
           #:when (not (eq? r 'rbp))
	   (define base ((interp-x86-exp env) (Reg r)))
	   (define addr (+ base i))
	   ((memory-write!) addr value)
	   env]
	  [dest
	   (define name (get-name dest))
	   (cons (cons name value) env)])))

    (define (x86-binary-op? x)
      (let ([val (hash-ref x86-ops x #f)])
	(and val (= (car val) 2))))

    (define (x86-unary-op? x)
      (let ([val (hash-ref x86-ops x #f)])
	(and val (= (car val) 1))))

    (define/override (interp-x86-instr env)
      (lambda (ast)
        (when (pair? ast)
          (copious "R3/interp-x86-instr" (car ast)))
	(match ast
          #;[(cons (Callq 'malloc) ss)
           (define num-bytes ((interp-x86-exp env) (Reg 'rdi)))
           ((interp-x86-instr
             `((rax . ,(allocate-page! 'malloc num-bytes)) . ,env))
	    ss)]
          #;[(cons (Callq 'alloc) ss)
           (define num-bytes ((interp-x86-exp env) (Reg 'rdi)))
           ((interp-x86-instr
             `((rax . ,(allocate-page! 'alloc num-bytes)) . ,env))
	    ss)]
          [(cons (Callq 'collect _) ss)
           (define rootstack ((interp-x86-exp env) (Reg 'rdi)))
           (define bytes-requested ((interp-x86-exp env) (Reg 'rsi)))
           ((collect!) rootstack bytes-requested)
           ((interp-x86-instr env) ss)]
          [(cons (Instr 'movq (list s d)) ss)
           (define value   ((interp-x86-exp env) s))
           (define new-env ((interp-x86-store env) d value))
           ((interp-x86-instr new-env) ss)]
          [(cons (Instr (? x86-binary-op? binop) (list s d)) ss)
           (define src ((interp-x86-exp env) s))
           (define dst ((interp-x86-exp env) d))
           (define op  (interp-x86-op binop))
           (define new-env ((interp-x86-store env) d (op src dst)))
           ((interp-x86-instr new-env) ss)]
          [(cons (Instr (? x86-unary-op? unary-op) (list d)) ss)
           (define dst ((interp-x86-exp env) d))
           (define op  (interp-x86-op unary-op))
           (define new-env ((interp-x86-store env) d (op dst)))
           ((interp-x86-instr new-env) ss)]
          [else
           ((super interp-x86-instr env) ast)]
          )))

    ;; before register allocation
    (define/override (interp-pseudo-x86 env)
      (lambda (ast)
        (copious "R3/interp-pseudo-x86" ast)
	(match ast
	  [(X86Program info blocks)
           ;(define ty (dict-ref info 'type))
	    ((initialize!) runtime-config:rootstack-size
	     runtime-config:heap-size)
	   (define env (cons (cons 'r15 (unbox rootstack_begin)) '()))
	   (parameterize ([get-basic-blocks blocks])
	      (let ([env^ ((interp-x86-block env) (dict-ref blocks 'start))])
		(lookup 'rax env^)))]
          )))

    ;; after register allocation
    (define/override (interp-x86 env)
      (lambda (ast)
        (copious "R3/interp-x86" ast)
	(match ast
	  [(X86Program info blocks)
           ;;#:when (dict-has-key? info 'num-spills)
           (define root-spills (dict-ref info 'num-root-spills))
           (define variable-size 8) ;; ugh -Jeremy
           (define root-space (* variable-size root-spills))
           ((initialize!) runtime-config:rootstack-size
                          runtime-config:heap-size)
	   (define env (cons (cons 'r15 (+ root-space (unbox rootstack_begin)))
			     '()))
	   (parameterize ([get-basic-blocks blocks])
	      (let ([env^ ((interp-x86-block env) (dict-ref blocks 'start))])
		(lookup 'rax env^)))]
          )))

    (set! x86-ops
          (hash-set* x86-ops
                     'sarq `(2 ,(lambda (n v) (arithmetic-shift v (- n))))
                     'salq `(2 ,(lambda (n v)
                                  (crop-to-64bits (arithmetic-shift v n))))
                     'orq `(2 ,bitwise-ior)
                     ))
    
    ));; interp-R3-class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R4: functions

(define interp-R4-class
  (class interp-R3-class
    (super-new)
    (inherit primitives interp-op initialize! 
             return-from-tail interp-x86-block memory-read memory-write!
             interp-x86-store)

    (inherit-field result rootstack_begin free_ptr fromspace_end
		   uninitialized global-label-table)

    (define/public (apply-fun interp fun-val arg-vals)
      (match fun-val
        #;[`(tagged ,fun-val^ ,t) ;; for dynamically typed
           (apply-fun interp fun-val^ arg-vals)]
        [(Function xs body lam-env)
         (define new-env (append (map cons xs arg-vals) lam-env))
         ((interp new-env) body)]
        [else (error 'apply-fun "expected function, not ~a" fun-val)]))
    
    (define/public (non-apply-ast)
      (set-union (primitives)
		 (set 'if 'let 'define 'program 'has-type 'void)))

    (define/public (interp-scheme-def d)
      (match d
        [(Def f `([,xs : ,ps] ...) rt info body)
         (mcons f (Function xs body '()))]
        ))

    (define/override (interp-scheme-exp env)
      (lambda (ast)
	(verbose "R4/interp-scheme" ast)
        (define recur (interp-scheme-exp env))
	(match ast
	  [(Apply fun args)
	   (define fun-val (recur fun))
	   (define new-args (for/list ([e args]) (recur e)))
           (apply-fun (lambda (env) (interp-scheme-exp env)) fun-val new-args)]
          [else ((super interp-scheme-exp env) ast)]
          )))

    (define/override (interp-scheme env)
      (lambda (ast)
	(verbose "R4/interp-scheme" ast)
	(match ast
	  #;[`(program (type ,ty) ,ds ... ,body)
	   ((interp-scheme '()) `(program ,@ds ,body))]
	  [(Program info (cons ds body))
	   ((initialize!) runtime-config:rootstack-size
	    runtime-config:heap-size)
           (let ([top-level (for/list ([d ds]) (interp-scheme-def d))])
             (for ([b top-level])
               (set-mcdr! b (match (mcdr b)
                              [(Function xs body '())
                               (Function xs body top-level)])))
	     ((interp-scheme-exp top-level) body))]
	  [else ((super interp-scheme env) ast)]
	  )))

    #;(define/public (F-vector-ref vec i)
      (match vec
        [`(vector-proxy ,v ,rs ,ws)
         (define v^ (F-vector-ref v i))
         (define r (vector-ref rs i))
         (apply-fun (lambda (env) (interp-F env)) 
                    r (list v^))]
        [else
         (vector-ref vec i)]))

    #;(define/public (F-vector-set! vec i arg)
      (match vec
        [`(vector-proxy ,v ,rs ,ws)
         (define w (vector-ref ws i))
         (define arg^ (apply-fun (lambda (env) (interp-F env))
                                 w (list arg)))
         (F-vector-set! v i arg^)]
        [else
         (vector-set! vec i arg)]))

    (define/public (interp-F env)
      (lambda (ast)
	(define result
	(match ast
	  ;; For R4
	  [(Def f `([,xs : ,ps] ...) rt info body)
	   (cons f (Function xs body '()))]
	  [(FunRef f n)
	   (lookup f env)]
	  [(Apply fun args)
	    (define fun-val ((interp-F env) fun))
	    (define arg-vals (map (interp-F env) args))
	    (match fun-val
	       [(Function xs body fenv)
		(define new-env (append (map cons xs arg-vals) env))
		((interp-F new-env) body)]
	       [else (error "interp-F, expected function, not" fun-val)])]
	  [(ProgramDefs info ds)
	   ((initialize!) runtime-config:rootstack-size
                          runtime-config:heap-size)
	   (let ([top-level (map  (interp-F '()) ds)])
             ((interp-F top-level) (Apply (Var 'main) '())))]
	  ;; For R3
	  [(GlobalValue 'free_ptr)
	   (unbox free_ptr)]
	  [(GlobalValue 'fromspace_end)
	   (unbox fromspace_end)]
	  [(Allocate l ty) (build-vector l (lambda a uninitialized))]
	  [(AllocateClosure l ty arity)
           (define vec (build-vector (add1 l) (lambda a uninitialized)))
           (vector-set! vec l `(arity ,arity))
           vec]
          #;[(AllocateClosure l ty arity)
           (build-vector l (lambda a uninitialized))]
	  [(AllocateProxy ty) (build-vector 3 (lambda a uninitialized))]
	  [(Collect size)
	   (unless (exact-nonnegative-integer? size)
	     (error 'interp-F "invalid argument to collect in ~a" ast))
	   (void)]
	  [(Void) (void)]
	  ;; For R2
	  [(HasType e t) ((interp-F env) e)]
	  [(Bool b) b]
	  [(Prim 'and (list e1 e2))
	   (match ((interp-F env) e1)
	     [#t (match ((interp-F env) e2)
		   [#t #t] [#f #f])]
	     [#f #f])]
	  [(If cnd thn els)
	   (if ((interp-F env) cnd)
	       ((interp-F env) thn)
	       ((interp-F env) els))]
	  ;; For R1
	  [(Var x)
	   (lookup x env)]
	  [(Int n) n]
	  [(Let x e body)
	   (let ([v ((interp-F env) e)])
	     ((interp-F (cons (cons x v) env)) body))]
	  [(Prim op args)
           (apply (interp-op op) (for/list ([e args]) ((interp-F env) e)))]
          [(Apply f args)
           ((interp-F env) (Apply f args))]
          [else
           (error 'interp-F "R4 unmatched ~a" ast)]
	  ))
	(verbose "R4/interp-F" ast result)
	result
	))

    (define/override (interp-C-exp env)
      (lambda (ast)
        (define result
	(match ast
          [(FunRef f n)
           (lookup f env)]
          [(Call f args)
           (define arg-vals (map (interp-C-exp env) args))
           (define f-val ((interp-C-exp env) f))
           (match f-val
             [(CFunction xs info blocks def-env)
              (define f (dict-ref info 'name))
              (define f-start (symbol-append f '_start))
              (define new-env (append (map cons xs arg-vals) def-env))
              (parameterize ([get-basic-blocks blocks])
                ((interp-C-tail new-env) (dict-ref blocks f-start)))]
             [else (error "interp-C, expected a function, not" f-val)])]
          [else
           ((super interp-C-exp env) ast)]
           ))
	(verbose "R4/interp-C-exp" ast result)
        result))

    (define/override (interp-C-tail env)
      (lambda (ast)
        (define result
	(match ast
          [(TailCall f args)
           (define arg-vals (map (interp-C-exp env) args))
           (define f-val ((interp-C-exp env) f))
           (match f-val
             [(CFunction xs info blocks def-env)
              (define f (dict-ref info 'name))
              (define f-start (symbol-append f '_start))
              (define new-env (append (map cons xs arg-vals) def-env))
              (parameterize ([get-basic-blocks blocks])
                ((interp-C-tail new-env) (dict-ref blocks f-start)))]
             [else (error "interp-C, expected a funnction, not" f-val)])]
          [else
           ((super interp-C-tail env) ast)]
          ))
	(verbose "R4/interp-C-tail" ast result)
        result))

    (define/public (interp-C-def ast)
      (verbose "R4/interp-C-def" ast)
      (match ast
        [(Def f `([,xs : ,ps] ...) rt info blocks)
         (mcons f (CFunction xs `((name . ,f)) blocks '()))]
        [else
         (error "R4/interp-C-def unhandled" ast)]
        ))
    
    (define/override (interp-C ast)
      (verbose "R4/interp-C" ast)
      (match ast
        [(ProgramDefs info ds)
         ((initialize!) runtime-config:rootstack-size
                        runtime-config:heap-size)
         (define top-level (for/list ([d ds]) (interp-C-def d)))
         ;; tie the knot
         (for/list ([b top-level])
           (set-mcdr! b (match (mcdr b)
                          [(CFunction xs info blocks '())
                           (CFunction xs info blocks top-level)])))
         ((interp-C-tail top-level) (TailCall (Var 'main) '()))]
        [else
         (error "R4/interp-C unhandled" ast)]
        ))

    (define (stack-arg-name n)
      (string->symbol (string-append "rsp_" (number->string n))))

    (define/public (builtin-funs)
      (set 'malloc 'alloc 'collect 'initialize 'read_int 'exit))

    (define/override (get-name ast)
      (match ast
        [(StackArg n) (stack-arg-name n)]
	 [else (super get-name ast)]))

    ;; Hide function details to avoid spamming the debug output.
    (define/override (observe-value v)
      (match v
	[(X86Function info blocks def-env)
         `(function ,(dict-ref info 'name))]
        [else v]))

    (define root-stack-pointer 0)
    
    (define/public (call-function f-val cont-ss env)
      (match f-val
	[(X86Function info blocks def-env)
	 (debug "interp-x86 call-function" (observe-value f-val))
         (define n (dict-ref info 'num-params))
         (define f (dict-ref info 'name))
         (define root-spills (dict-ref info 'num-root-spills #f))         
	 ;; copy argument registers over to new-env
	 (define passing-regs
	   (filter (lambda (p) p)
		   (for/list ([r (vector-take arg-registers n)])
                     (let ([v (lookup r env #f)])
                       (if v (cons r v) #f)))))
         (debug "interp-x86 call-function" passing-regs)
         (define variable-size 8) ;; ugh -Jeremy
         (define root-size
           (cond [root-spills (* variable-size root-spills)]
                 [else 0]))
         (set! root-stack-pointer (+ root-stack-pointer root-size))
         (define new-env (cons (cons 'r15 root-stack-pointer)
                               (append passing-regs def-env)))
         ;; interpret the body of the function in the new-env
         (define result-env
           (parameterize ([get-basic-blocks blocks])
             ((interp-x86-block new-env)
              (dict-ref blocks (symbol-append f '_start)))))
         (set! root-stack-pointer (- root-stack-pointer root-size))
         (define res (lookup 'rax result-env))
         ;; return and continue after the function call, back in env
         ((interp-x86-instr (cons (cons 'rax res) env)) cont-ss)]
        [else (error "interp-x86, expected a function, not" f-val)]))

    (define/override (interp-x86-exp env)
      (lambda (ast)
        (copious "R4/interp-x86-exp" ast)
        (define result
          (match ast
            [(StackArg n)
             (define x (stack-arg-name n))
             (lookup x env)]
            #;[(FunRef f n)
             (lookup f env)]
            [else ((super interp-x86-exp env) ast)]))
        (copious "R4/interp-x86-exp" (observe-value result))
        result))

    #;(define (apply-vector-ref vec i cont-ss env)
      (define tag ((memory-read) vec))
      (verbose 'apply-vector-ref ((memory-read) vec))
      (cond [(equal? (arithmetic-shift tag -63) 1)
             (define vec^ ((memory-read) (+ vec (* 1 8))))
             (define val^ (apply-vector-ref vec^ i '() env))
             (define read-clos ((memory-read) (+ vec (* 2 8))))
             (apply-closure read-clos val^ cont-ss env)]
            [else
             (define res ((memory-read) (+ vec (* (add1 i) 8))))
             ((interp-x86-instr (cons (cons 'rax res) env)) cont-ss)]
            ))
    
    #;(define (apply-vector-set! vec i val cont-ss env)
      (define tag ((memory-read) vec))
      (cond [(equal? (arithmetic-shift tag -63) 1)
             (define write-clos ((memory-read) (+ vec (* 3 8))))
             (define val^ (apply-closure write-clos val '() env))
             (apply-vector-set! vec i val^ cont-ss env)]
            [else
             ((memory-write!) (+ vec (* (add1 i) 8)) val)
             ((interp-x86-instr env) cont-ss)]
            ))

    (define/override (interp-x86-instr env)
      (lambda (ast)
        (when (pair? ast)
          (copious "R4/interp-x86-instr" (car ast)))
	(match ast
          ;; Treat leaq like movq -Jeremy
          [(cons (Instr 'leaq (list s d)) ss)
           (define value   ((interp-x86-exp env) s))
           (define new-env ((interp-x86-store env) d value))
           ((interp-x86-instr new-env) ss)]
          [(cons (IndirectCallq f _) ss)
           (debug "indirect callq" ast)
           (define f-val ((interp-x86-exp env) f))
           (call-function f-val ss env)]
          [(cons (TailJmp f n) ss)
           (debug "tail jmp" ast)
           (define f-val ((interp-x86-exp env) f))
           (call-function f-val '() env)]
          [(cons (Callq f _) ss) 
           #:when (not (set-member? (builtin-funs) f))
           (call-function (lookup f env) ss env)]
          [(cons (Callq 'exit _) ss)
           (error 'interp-x86 "exiting")]
          [else
           ((super interp-x86-instr env) ast)]
          )))

    (define/public (interp-x86-def ast)
      (match ast
        [(Def f ps rt info blocks)
         (cons f (X86Function (dict-set info 'name f) blocks '()))]
        ))
        
    ;; The below applies before register allocation
    (define/override (interp-pseudo-x86 env)
      (lambda (ast)
        (copious "R4/interp-pseudo-x86" ast)
	(match ast
          [(X86ProgramDefs info ds)
           ((initialize!) runtime-config:rootstack-size
                          runtime-config:heap-size)
           (set! root-stack-pointer (unbox rootstack_begin))
           (define top-level (for/list ([d ds]) (interp-x86-def d)))
           ;; Put the functions in the globals table
           (for ([(label fun) (in-dict top-level)])
             (dict-set! global-label-table label (box fun)))
             
           (define env^ (list (cons 'r15 (unbox rootstack_begin))))
           (define result-env (call-function (lookup 'main top-level) '() env^))
           (lookup 'rax result-env)]
          )))
    
    ;; The below applies after register allocation -Jeremy
    (define/override (interp-x86 env)
      (lambda (ast)
        (verbose "R4/interp-x86" ast)
	(match ast
          [(X86ProgramDefs info ds)
           ((initialize!) runtime-config:rootstack-size
                          runtime-config:heap-size)
           (set! root-stack-pointer (unbox rootstack_begin))
           (define top-level (for/list ([d ds]) (interp-x86-def d)))
           ;; Put the functions in the globals table
           (for ([(label fun) (in-dict top-level)])
             (dict-set! global-label-table label (box fun)))
           (define env^ '())
           (define result-env
             (call-function (lookup 'main top-level) '() env^))
           (lookup 'rax result-env)]
          )))

    )) ;; end  interp-R4-class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R5: lambda

(define interp-R5-class
  (class interp-R4-class
    (super-new)
    (inherit initialize! return-from-tail apply-fun)
    (inherit-field result)

    (define/override (primitives)
      (set-union (super primitives)
		 (set 'procedure-arity)))
    
    (define/override (non-apply-ast)
      (set-union (super non-apply-ast)
		 (set 'global-value 'allocate 'collect)))

    (define/override (interp-op op)
      (match op
         ['procedure-arity (lambda (v)
                            (match v
                              [(Function xs body lam-env)
                               (length xs)]
                              [(vector (Function ps body env) vs ... `(arity ,n))
                               n]
                              [else
                               (error 'interp-op "expected function, not ~a" v)]
                              ))]
	 [else (super interp-op op)]
	 ))

    
    (define/override (interp-scheme-exp env)
      (lambda (ast)
	(verbose "R5/interp-scheme" ast)
	(match ast
	  [(Lambda `([,xs : ,Ts] ...) rT body)
	   (Function xs body env)]
	  [else ((super interp-scheme-exp env) ast)]
          )))

    (define/override (interp-F env)
      (lambda (ast)
        (define result
	(match ast
	  [(Lambda `([,xs : ,Ts] ...) rT body)
	   (Function xs body env)]
	  [(Def f `([,xs : ,ps] ...) rt info body)
	   (mcons f (Function xs body '()))]
	  [(ProgramDefs info ds)
	   ((initialize!) runtime-config:rootstack-size
	    runtime-config:heap-size)
	   (let ([top-level (map (interp-F '()) ds)])
	     ;; tie the knot
	     (for/list ([b top-level])
	       (set-mcdr! b (match (mcdr b)
			      [(Function xs body '())
			       (Function xs body top-level)])))
	     ((interp-F top-level) (Apply (Var 'main) '())))]
          [(Closure arity args)
	   (define arg-vals (map (interp-F env) args))
           (apply vector (append arg-vals (list `(arity ,arity))))]
	  [(Apply fun args)
	   (define fun-val ((interp-F env) fun))
	   (define arg-vals (map (interp-F env) args))
           (apply-fun (lambda (env) (interp-F env)) fun-val arg-vals)]
	  [else ((super interp-F env) ast)]
          ))
        (verbose "R5/interp-F result of" ast result)
        result))

    )) ;; end interp-R5-class


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R6: type Any and inject/project

(define (crop-to-64bits n)
  (let ([s (string->list (number->string n 2))])
    (cond [(> (length s) 64)
           (let ([s^ (list-tail s (- (length s) 64))])
             (string->number (list->string s^) 2))]
          [else n])))

(define interp-R6-class-alt
  (class interp-R5-class
    (super-new)
    (inherit initialize!)
    (inherit-field result)

    (define/override (primitives)
      (set-union (super primitives)
		 (set 'boolean? 'integer? 'vector? 'procedure?
                      'tag-of-any 'value-of-any 'tag-of-vector)))

    (define/override (interp-op op)
      (match op
	 ['boolean? (lambda (v)
		      (match v
			 [`(tagged ,v1 Boolean) #t]
			 [else #f]))]
	 ['integer? (lambda (v)
		      (match v
			 [`(tagged ,v1 Integer) #t]
			 [else #f]))]
	 ['vector? (lambda (v)
		      (match v
			 [`(tagged ,v1 (Vector ,ts ...)) #t]
			 [else #f]))]
	 ['procedure? (lambda (v)
		      (match v
			 [`(tagged ,v1 (,ts ... -> ,rt)) #t]
			 [else #f]))]
	 ['eq? (lambda (v1 v2)
		 (match (list v1 v2)
		   [`((tagged ,v1^ ,ty1) (tagged ,v2^ ,ty2))
		    (and (eq? v1^ v2^) (equal? ty1 ty2))]
		   [else
		    (cond [(or (and (fixnum? v1) (fixnum? v2))
			       (and (boolean? v1) (boolean? v2))
			       (and (vector? v1) (vector? v2))
                               (and (void? v1) (void? v2)))
			   (eq? v1 v2)])]))]
         ['make-any (lambda (v tg) `(tagged ,v ,tg))]
         ['tag-of-any (lambda (v)
                        (match v
                          [`(tagged ,v^ ,tg)
                           tg]
                          [else
                           (error "interp expected tagged value, not" v)]))]
         ['value-of-any (lambda (v)
                          (match v
                            [`(tagged ,v^ ,tg)
                             v^]
                            [else
                             (error "interp expected tagged value, not" v)]))]
         ['tag-of-vector (lambda (v)
                           (match v
                             [`(vector-proxy ,vec ,rs ,ws) 1]
                             [else 0]))]
         ['any-vector-ref (lambda (v i)
                            (match v [`(tagged ,v^ ,tg) (vector-ref v^ i)]))]
         ['any-vector-set! (lambda (v i a)
                             (match v [`(tagged ,v^ ,tg)
                                       (vector-set! v^ i a)]))]
         ['any-vector-length (lambda (v)
                               (match v [`(tagged ,v^ ,tg)
                                         (vector-length v^)]))]
         ['any-vectorof-length (lambda (v)
                                 (match v [`(tagged ,v^ ,tg)
                                           (vector-length v^)]))]
	 [else (super interp-op op)]
	 ))

    ;; Equality for flat types.
    (define/public (tyeq? t1 t2)
      (match `(,t1 ,t2)
	[`((Vectorof Any) (Vector ,t2s ...))
         (for/and ([t2 t2s])
           (eq? t2 'Any))]
	[`((Vector ,t1s ...) (Vectorof Any))
	 (for/and ([t1 t1s])
           (eq? t1 'Any))]
	[else (equal? t1 t2)]))

    (define/override (interp-scheme-exp env)
      (lambda (ast)
	(verbose "R6/interp-scheme" ast)
	(define recur (interp-scheme-exp env))
	(match ast
	  [(Inject e t)
	   `(tagged ,(recur e) ,t)]
	  [(Project e t2)
           (define v (recur e))
	   (match v
	      [`(tagged ,v1 ,t1)
	       (cond [(tyeq? t1 t2) v1]
		     [else (error "in project, type mismatch" t1 t2)])]
	      [else (error "in project, expected injected value" v)])]
	  [else
	   ((super interp-scheme-exp env) ast)]
	  )))

    (define/override (interp-F env)
      (lambda (ast)
	(define recur (interp-F env))
        (define result
	(match ast
	  [(Inject e t)
	   `(tagged ,(recur e) ,t)]
	  [(Project e t2)
           (define v (recur e))
	   (match v
	      [`(tagged ,v1 ,t1)
	       (cond [(tyeq? t1 t2) v1]
		     [else (error "in project, type mismatch" t1 t2)])]
	      [else (error "in project, expected injected value" v)])]
          [(ValueOf e ty)
           (define v ((interp-F env) e))
           ((interp-op 'value-of-any) v)]
          ;; The following belongs in a new R7 interp class -Jeremy
	  [(Def f xs _ info body)
           #:when (andmap symbol? xs)
           #;(define anys (for/list ([x xs]) 'Any))
           #;(mcons f `(tagged (lambda ,xs ,body) (,anys -> Any)))
           (mcons f (Function xs body '()))]
	  [(Lambda xs _ body)
           #:when (andmap symbol? xs)
           #;(define anys (for/list ([x xs]) 'Any))
	   #;`(tagged ,(lambda ,xs ,body ,env) (,anys -> Any))
           (Function xs body env)]
	  [(FunRef f n)
	   (lookup f env)]
          [(Prim 'and (list e1 e2))
           (if ((interp-F env) e1)
               ((interp-F env) e2)
               #f)]
	  [(Prim op args)
           (apply (interp-op op) (map (interp-F env) args))]
	  [(ProgramDefs info ds)
	   ((initialize!) runtime-config:rootstack-size
	    runtime-config:heap-size)
	   (let ([top-level (map (interp-F '()) ds)])
	     ;; tie the knot
	     (for/list ([b top-level])
	       (set-mcdr! b (match (mcdr b)
                              ;; the following belongs in new R7 interp
                             #;[`(tagged (lambda ,xs ,body) ,t)
                               `(tagged (lambda ,xs ,body ,top-level) ,t)]
			      [(Function xs body '())
			       (Function xs body top-level)])))
	     ((interp-F top-level) (Apply (Var 'main) '())))]
	  [else ((super interp-F env) ast)]
	  ))
        (verbose "R6/interp-F result of" ast result)
        result))

    (define/override (interp-C-exp env)
      (lambda (ast)
        (define result
	(match ast
	  #;[(Inject e t)
	   `(tagged ,((interp-C-exp env) e) ,t)]
	  #;[(Project e t2)
	   (define v ((interp-C-exp env) e))
	   (match v
	      [`(tagged ,v1 ,t1)
	       (cond [(tyeq? t1 t2)
		      v1]
		     [else
		      (error "in project, type mismatch" t1 t2)])]
	      [else
	       (error "in project, expected injected value" v)])]
          [(ValueOf e ty)
           ((interp-op 'value-of-any) ((interp-C-exp env) e))]
	  [else
	   ((super interp-C-exp env) ast)]
	  ))
        (verbose "R6/interp-C-exp ===> " ast result)
        result))

    #;(define/override (display-by-type ty val)
      (match ty
	['Any
	 (define tag (bitwise-and val 7))
	 (cond [(eq? tag 1) ;; integer
		`(tagged ,(arithmetic-shift val (- 3)) Integer)]
	       [(eq? tag 4) ;; boolean
		(if (eq? 0 (arithmetic-shift val (- 3)))
		    `(tagged #f Boolean)
		    `(tagged #t Boolean))]
	       [(eq? tag 2) ;; vector
		;; This needs work. I need to find out how to get the
		;; length of the vector from in memory. -Jeremy
		`(tagged vector (Vectorof Any))]
	       [(eq? tag 3) ;; procedure (represented by a closure)
		`(tagged procedure ,ty)]
	       [(eq? tag 5) ;; void
		`(tagged void Void)])]
	[else (super display-by-type ty val)]))

    )) ;; interp-R6-class-alt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R8: for loops

(define interp-R8-class
  (class interp-R6-class-alt
    (super-new)
    (inherit initialize! interp-C-exp)
    (inherit-field result)

    (define/override (apply-fun interp fun-val arg-vals)
      (match fun-val
        #;[`(tagged ,fun-val^ ,t) ;; for dynamically typed
           (apply-fun interp fun-val^ arg-vals)]
        [(Function xs body lam-env)
         (define new-env (append (for/list ([x xs] [arg arg-vals])
                                   (cons x (box arg)))
                                 lam-env))
         ((interp new-env) body)]
        [else (error 'apply-fun "expected function, not ~a" fun-val)]))
    
    (define/override (interp-F env)
      (lambda (ast)
        (verbose "R8/interp-F starting" ast)
	(define recur (interp-F env))
        (define result
	(match ast
          [(Var x) (unbox (lookup x env))]
          [(Let x e body)
           (define new-env (cons (cons x (box (recur e))) env))
           ((interp-F new-env) body)]
	  [(ProgramDefs info ds)
	   ((initialize!) runtime-config:rootstack-size
	    runtime-config:heap-size)
	   (let ([top-level (map (interp-F '()) ds)])
	     ;; tie the knot
	     (for/list ([b top-level])
	       (set-mcdr! b (match (mcdr b)
			      [(Function xs body '())
			       (Function xs body top-level)])))
	     ((interp-F top-level) (Apply (FunRef 'main 0) '())))]
          [(WhileLoop cnd body)
           (define (loop)
             (cond [((interp-F env) cnd)
                    ((interp-F env) body)
                    (loop)]
                   [else
                    (void)]))
           (loop)]
          #;[(ForLoop x seq body)
           (define vec (recur seq))
           (for ([i vec])
             (define new-env (cons (cons x (box i)) env))
             ((interp-F new-env) body))
           (void)]
          [(Begin es body)
           (for ([e es]) (recur e))
           (recur body)]
          [(SetBang x rhs)
           (set-box! (lookup x env) (recur rhs))]
	  [else ((super interp-F env) ast)]
	  ))
        (verbose "R8/interp-F result of" ast result)
        result))

    (define/override (interp-C-stmt env)
      (lambda (ast)
        (copious "R8/interp-C-stmt" ast)
        (match ast
          [(Call f args)
           ((interp-C-exp env) ast)
           env]
          [else ((super interp-C-stmt env) ast)]
          )))
    
    )) ;; interp-R8-class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for gradual

(define interp-gradual-class
  (class interp-R8-class
    (super-new)
    (inherit memory-read memory-write! call-function interp-x86-exp)

    (define (interp-vector-length vec)
      (copious "gradual/interp-vector-length" vec)
      (define tag ((memory-read) vec))
      (bitwise-and #b111111 (arithmetic-shift tag -1)))

    (define (interp-vector-ref vec i)
      (define offset (* (+ i 1) 8))
      ((memory-read) (+ vec offset)))

    (define (interp-vector-set! vec i arg)
      (define offset (* (+ i 1) 8))
      ((memory-write!) (+ vec offset) arg))

    (define (apply-closure clos arg cont-ss env)
      (define f (interp-vector-ref clos 0))
      (define env^ (append (list (cons 'rdi clos) (cons 'rsi arg)) env))
      (define result-env (call-function f cont-ss env^))
      (lookup 'rax result-env))

    (define (vector-proxy? vec)
      (define tag ((memory-read) vec))
      (equal? 1 (bitwise-and (arithmetic-shift tag -57) 1)))
    
    (define (proxy-vector-length vec)
      (copious "gradual/proxy-vector-length" vec)
      (cond [(vector-proxy? vec)
             (define vec^ (interp-vector-ref vec 0))
             (copious "proxy: underlying vec " vec^)
             (proxy-vector-length vec^)]
            [else (interp-vector-length vec)]))
    
    (define (proxy-vector-ref vec i)
      (cond [(vector-proxy? vec)
             (define vec^ (interp-vector-ref vec 0))
             (define val (proxy-vector-ref vec^ i))
             (define rd (interp-vector-ref (interp-vector-ref vec 1) i))
             (apply-closure rd val '() '())]
            [else (interp-vector-ref vec i)]))

    (define (proxy-vector-set! vec i arg)
      (cond [(vector-proxy? vec)
             (define vec^ (interp-vector-ref vec 0))
             (define wr (interp-vector-ref (interp-vector-ref vec 2) i))
             (define arg^ (apply-closure wr arg '() '()))
             (proxy-vector-set! vec^ i arg^)]
            [else (interp-vector-set! vec i arg)]))
    
    (define/override (interp-x86-instr env)
      (lambda (ast)
        (when (pair? ast)
          (copious "gradual/interp-x86-instr" (car ast)))
	(match ast
          [(cons (Callq 'proxy_vector_length _) ss)
           (define vec ((interp-x86-exp env) (Reg 'rdi)))
           (define v (proxy-vector-length vec))
           ((interp-x86-instr (cons (cons 'rax v) env)) ss)]
          [(cons (Callq 'proxy_vector_ref _) ss)
           (define vec ((interp-x86-exp env) (Reg 'rdi)))
           (define i ((interp-x86-exp env) (Reg 'rsi)))
           (define v (proxy-vector-ref vec i))
           ((interp-x86-instr (cons (cons 'rax v) env)) ss)]
          [(cons (Callq 'proxy_vector_set _) ss)
           (define vec ((interp-x86-exp env) (Reg 'rdi)))
           (define i ((interp-x86-exp env) (Reg 'rsi)))
           (define arg ((interp-x86-exp env) (Reg 'rdx)))
           (define v (proxy-vector-set! vec i arg))
           ((interp-x86-instr (cons (cons 'rax v) env)) ss)]
          [else ((super interp-x86-instr env) ast)]
          )))
    
    ))

(define min-int64 (- (arithmetic-shift 1 63)))
(define max-int64 (sub1 (arithmetic-shift 1 63)))
(define mask-64 (sub1 (arithmetic-shift 1 64)))
(define offset-64 (arithmetic-shift 1 63))
(define (to-signed x)
  (- (bitwise-and (+ x offset-64) mask-64) offset-64))
(define (+i64 x y)
  (to-signed (+ x y)))
(define (-i64 x y)
  (to-signed (- x y)))
(define (*i64 x y)
  (to-signed (* x y)))
(define (neg-i64 x)
  (to-signed (- x)))

;; Interpreter interface so that python-generated x86 code can be interpreted
;; using the racket x86 emulator
(define (interp-x86-python-mixin base-class)
  (class base-class
    (super-new)
    (inherit-field x86-ops)
    (set! x86-ops (hash-set* x86-ops
                             'addq `(2 ,+i64)
                             'imulq `(2 ,*i64)
                             'subq `(2 ,(lambda (s d) (-i64 d s)))
                             'negq `(1 ,neg-i64)))

    (define/override (interp-x86-instr env)
      (lambda (ast)
        (when (pair? ast)
          (copious "interp-x86-python/interp-x86-instr" (car ast)))
        (match ast
          [(cons (Callq 'read_int _) ss)
           (let ([v (min max-int64 (max min-int64 (read)))])
             (copious "read " v)
             ((interp-x86-instr (cons (cons 'rax v) env)) ss))]
          [(cons (Callq 'print_int _) ss)
           (printf "~a" (lookup 'rdi env))
           ((interp-x86-instr env) ss)]
          [else ((super interp-x86-instr env) ast)])))
    (define/override (eflags-status env cc)
      (match cc
        ['ne (if (not (eqv? 1 (eflags-status env 'e))) 1 0)]
        [else (super eflags-status env cc)]))))

(define choose-base-interp-class
  (λ (program)
    (match program
      [(X86Program info body) interp-R3-class]
      [(X86ProgramDefs info blocks) interp-gradual-class]
      [else (error "choose-base-interp no match in for" program)])))

(define interp-pseudo-x86-python
  (lambda (p)
    ((send (new (interp-x86-python-mixin (choose-base-interp-class p)))
           interp-pseudo-x86 '()) p)))

;; The interp-x86-python interpreter takes a program of the form
;; (X86Program info blocks)
;; Also, the info field must be an association list
;; with a key 'num-root-spills whose values is
;; the number of spills to the root stack.
(define interp-x86-python
  (lambda (p)
    ((send (new (interp-x86-python-mixin (choose-base-interp-class p)))
           interp-x86 '()) p)))
