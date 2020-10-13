#lang racket
(require racket/fixnum)
(require "utilities.rkt" (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-scheme interp-C interp-x86 interp-pseudo-x86
         R1-interp-x86 R2-interp-x86 R3-interp-x86
         interp-R1-class interp-R2-class interp-R3-class
	 interp-R4-class interp-R5-class interp-R6-class
         interp-C2 interp-C3
         interp-pseudo-x86-0 interp-x86-0
         interp-pseudo-x86-1 interp-x86-1
         interp-pseudo-x86-2 interp-x86-2
         interp-pseudo-x86-3 interp-x86-3)

;; The interpreters in this file are for interpreting
;; the intermediate languages produced by the various
;; passes of the compiler.
;; 
;; The interpreters for the source languages (R0, R1, ..., R7)
;; and the C intermediate languages C0 and C1
;; are in separate files, e.g., interp-R0.rkt.

;; Interpreters for C2 and C3.

(define interp-C2
  (lambda (p)
    (send (new interp-R3-class) interp-C p)))
  
(define interp-C3
  (lambda (p)
    (send (new interp-R4-class) interp-C p)))

;; Interpreters for x86 with names that correspond to the book.

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

(define interp-x86-2
  (lambda (p)
    ((send (new interp-R3-class) interp-x86 '()) p)))

(define interp-pseudo-x86-3
  (lambda (p)
    ((send (new interp-R4-class) interp-pseudo-x86 '()) p)))

(define interp-x86-3
  (lambda (p)
    ((send (new interp-R4-class) interp-x86 '()) p)))

;; The following interpreters handle the final languages.

(define interp-scheme
  (lambda (p)
    ((send (new interp-R6-class) interp-scheme '()) p)))

(define interp-C
  (lambda (p)
    (send (new interp-R6-class) interp-C  p)))

(define interp-x86
  (lambda (p)
    ((send (new interp-R6-class) interp-x86 '()) p)))

(define interp-pseudo-x86
  (lambda (p)
    ((send (new interp-R6-class) interp-pseudo-x86 '()) p)))

;; The following interpreter names are obsolete,
;; remove after Fall 2020. -Jeremy 

(define R1-interp-x86
  (lambda (p)
    ((send (new interp-R1-class) interp-x86 '()) p)))

(define R2-interp-x86
  (lambda (p)
    ((send (new interp-R2-class) interp-x86 '()) p)))

(define R3-interp-x86
  (lambda (p)
    ((send (new interp-R3-class) interp-x86 '()) p)))

(define fst
  (lambda (p)
    (cond [(pair? p)
           (car p)]
          [(mpair? p)
           (mcar p)]
          [else
           (error 'fst "not a pair of any sort" p)])))

(define (apply-fun interp fun-val arg-vals)
  (match fun-val
    [`(lambda (,xs ...) ,body ,lam-env)
     (define new-env (append (map cons xs arg-vals) lam-env))
     ((interp new-env) body)]
    [else (error 'apply-fun "expected function, not ~a" fun-val)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R1: integer arithmetic and 'let'

(define interp-R1-class
  (class object%
    (super-new)

    (field (result (gensym 'result)))

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
	 [else (error "in interp-op S0, unmatched" op)]))

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
        (verbose "C0/interp-C-exp" ast)
        (match ast
          [(Var x) (lookup x env)]
          [(Int n) n]
          [(Prim op args)
	   (apply (interp-op op) (map (interp-C-exp env) args))]
          [else
           (error "C0/interp-C-exp unhandled" ast)]
          )))
          
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
          [else
           (error "interp-C-stmt unhandled" ast)]
          )))
          
    (define/public (interp-C ast)
      (debug "R1/interp-C" ast)
      (match ast
        [(Program info (CFG G))
         (define start (dict-ref G 'start))
         ((interp-C-tail '()) start)]
        [else
         (error "no match in interp-C for " ast)]
        ))

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
	(vomit "interp-x86-exp" ast)
	(match ast
	   [(or (Var x) (Reg x))
	    (lookup (get-name ast) env)]
	   [(Deref r n)
	    (lookup (get-name ast) env)]
	   [(Imm n) n]
	   [else
	    (error 'interp-R1-class/interp-x86-exp "unhandled ~a" ast)])))

    (define/public (interp-x86-instr env)
      (lambda (ast)
        (when (pair? ast)
          (vomit "R1/interp-x86-instr" (car ast)))
        (match ast
	   ['() env]
	   [(cons (Callq 'read_int) ss)
	    ((interp-x86-instr (cons (cons 'rax (read)) env)) ss)]
           [(cons (Instr 'movq (list s d)) ss)
            (define x (get-name d))
	    (define v ((interp-x86-exp env) s))
	    ((interp-x86-instr (cons (cons x v) env)) ss)]
           [(cons (Jmp conclusion) ss)
            #:when (string-suffix? (symbol->string conclusion) "conclusion")
            env]
           [(cons (Jmp label) ss)
            ((interp-x86-block env) (goto-label label))]
	   [(Program info ss)
	    (let ([env ((interp-x86-instr '()) ss)])
              (lookup 'rax env))]
	   [(cons (Instr binary-op (list s d)) ss)
	    (let ([s ((interp-x86-exp env) s)]
		  [d ((interp-x86-exp env) d)]
		  [x (get-name d)]
		  [f (interp-x86-op binary-op)])
	      ((interp-x86-instr (cons (cons x (f d s)) env)) ss))]
	   [(cons (Instr unary-op (list d)) ss)
	    (let ([d ((interp-x86-exp env) d)]
		  [x (get-name d)]
		  [f (interp-x86-op unary-op)])
	      ((interp-x86-instr (cons (cons x (f d)) env)) ss))]
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
          (vomit "R1/interp-x86" (car ast)))
        (match ast
          [(Program info (CFG G))
           (parameterize ([get-CFG G])
             (define start-block (dict-ref G 'start))
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

    (inherit interp-x86-block)
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
	(vomit "R2/interp-C-exp" ast)
	(match ast
          [(HasType e t) ((interp-C-exp env) e)]
          [(Bool b) b]
          [else ((super interp-C-exp env) ast)]
          )))

    (define/override (interp-C-tail env)    
      (lambda (ast)
	(vomit "R2/interp-C-tail" ast)
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
      (vomit "R2/interp-C" ast)
      (match ast
        [(Program info (CFG G))
         (parameterize ([get-CFG G])
           (super interp-C (Program info (CFG G))))]
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
        (vomit "R2/interp-x86-exp" ast)
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
	  )))

    ;; The following is not maintainable -Jeremy
    #;(define (eflags-status env cc)
      (match cc
	 ['e
	  (define eflags ((interp-x86-exp env) '(reg __flag)))
	  (arithmetic-shift (bitwise-and eflags #b1000000) -6)]
	 ['l
	  ;; Get the value of the lt flag which doesn't actually exist
	  ;; the lt flag is simulated by overflow == sign for x86
	  (define eflags ((interp-x86-exp env) '(reg __flag)))
	  (define overflow (bitwise-and eflags #b100000000000))
	  (define sign	   (bitwise-and eflags #b000010000000))
	  (if (= overflow sign) 1 0)]
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

    (define (eflags-status env cc)
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
	      (vomit "R2/interp-x86-instr" (car ast)))
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
          ;; if's are present before patch-instructions
          [(cons (IfStmt cnd thn els) ss)
           ;; todo: add AST node for following?
           #;(cons (If cnd thn ,_ ,els ,_) . ,ss)
           (if (not (eq? 0 ((interp-x86-exp env) cnd)))
               ((interp-x86-instr env) (append thn ss))
               ((interp-x86-instr env) (append els ss)))]

          ;; cmpq performs a subq operation and examimines the state
          ;; of the result, this is done without overwriting the second
          ;; register. -andre
          ;; Notice that the syntax is very confusing
          ;; (cmpq ,s2 ,s1) (jl then) (jmp else) ...
          ;; (if (< s1 s2) then else)
          ;; The following is not maintainable -Jeremy
          #;[`((cmpq ,s2 ,s1) . ,ss)
           (let* ([v1 ((interp-x86-exp env) s1)]
                  [v2 ((interp-x86-exp env) s2)]
                  [v3 (- v2 v1)]
                  [zero     (arithmetic-shift (b2i (eq? v3 0)) 6)]
                  [sign     (arithmetic-shift (b2i (< v3 0)) 7)]
                  ;; Our numbers do not overflow so this bit is always 0
                  [overflow (arithmetic-shift 0 11)]
                  [eflags (bitwise-ior overflow sign zero)])
             ((interp-x86-instr (cons (cons '__flag eflags) env)) ss))]
          
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

    (define/override (interp-x86 env)
      (lambda (ast)
        (when (pair? ast)
	      (vomit "R2/interp-x86" (car ast)))
        (match ast
          [(Program info (CFG G))
           #:when (dict-has-key? info 'type)
           (define ty (lookup 'type info))
           (define new-info (dict-remove info 'type))
           (display-by-type ty ((interp-x86 env) (Program new-info (CFG G))))]
          [else ((super interp-x86 env) ast)]
          )))

    (define/public (display-by-type ty val)
      (match ty
	['Boolean (if val #t #f)]
	['Integer val]
	[else (error (format "don't know how to display type ~a" ty))]))

    ));; class interp-R2-class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R3: Vectors

(define interp-R3-class
  (class interp-R2-class
    (super-new)
    (inherit get-name interp-x86-op interp-x86-block)
    (inherit-field x86-ops)

    (define/override (display-by-type ty val)
      (match ty
	['Void (void)]
	[`(Vector ,tys ...)
	 (list->vector
	  (map (lambda (ty index)
		 (display-by-type ty ((memory-read)
				      (+ val 8 (* 8 index)))))
	       tys (range (length tys))))]
	[else (super display-by-type ty val)]))

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
	    (make-immutable-hash
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
	    [`() (error 'free "address ~a isn't currently allocated")]
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
      (unless (and (fixnum? addr)
		   (positive? addr))
	(error 'fetch-page "expected non-negative fixnum in ~a" addr))
      (unless (= 0 (modulo addr 8))
	(error 'fetch-page "expected quadword alligned address in ~a" addr))
      (let search ([m (unbox memory)])
        (match m
          [`() (error 'fetch-page (fmt-err addr memory))]
          [`((page ,min ,max ,name ,vect) . ,rest-memory)
           (vomit "R3/fetch page" addr min max name vect)
           (if (and (<= min addr) (< addr max))
               (values min max name vect)
               (search rest-memory))]
          [other (error 'fetch-page "unmatched ~a" m)])))

    (define/override (primitives)
      (set-union (super primitives)
		 (set 'vector  'vector-ref 'vector-set!
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

    #;(define/public (C-vector-ref vec i)
      (match vec
        [`(vector-proxy ,v ,rs ,ws)
         (define v^ (C-vector-ref v i))
         (define r (vector-ref rs i))
         (apply-fun (lambda (env) (interp-C-exp env)) 
                    r (list v^))]
        [else
         (vector-ref vec i)]))

    #;(define/public (C-vector-set! vec i arg)
      (match vec
        [`(vector-proxy ,v ,rs ,ws)
         (define w (vector-ref ws i))
         (define arg^ (apply-fun (lambda (env) (interp-C-exp env))
                                 w (list arg)))
         (C-vector-set! v i arg^)]
        [else
         (vector-set! vec i arg)]))

    (define/override (interp-C-exp env)
      (lambda (ast)
        (vomit "R3/interp-C-exp" ast)
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
	  [(AllocateProxy ty) (build-vector 3 (lambda a uninitialized))]
          #;[`(vector-ref ,e-vec ,e-i)
           (define vec ((interp-C-exp env) e-vec))
           (define i ((interp-C-exp env) e-i))
           (C-vector-ref vec i)]
          #;[`(vector-set! ,e-vec ,e-i ,e-arg)
           (define vec ((interp-C-exp env) e-vec))
           (define i ((interp-C-exp env) e-i))
           (define arg ((interp-C-exp env) e-arg))
           (C-vector-set! vec i arg)]
          [else
           ((super interp-C-exp env) ast)]
          )))

    (define/override (interp-C-stmt env)
      (lambda (ast)
        (vomit "R3/interp-C-stmt" ast)
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
          ;; todo: add AST for following
	  #;[`(collect ,rs ,size)
	   (unless (and (exact-nonnegative-integer? ((interp-C-exp env) rs))
			(exact-nonnegative-integer? ((interp-C-exp env) size)))
	     (error 'interp-C "invalid argument(s) to collect in ~a" ast))
	   env]
          [else
           ((super interp-C-stmt env) ast)]
          )))

    (define/override (interp-C-tail env)
      (lambda (ast)
        (vomit "R3/interp-C-tail" ast)
        (match ast
          [(Seq s t)
           (define new-env ((interp-C-stmt env) s))
           ((interp-C-tail new-env) t)]
          [else
           ((super interp-C-tail env) ast)]
          )))
    
    (define/override (interp-C ast)
      (vomit "R3/interp-C" ast)
      (match ast
        [(Program info (CFG G))
         ((initialize!) runtime-config:rootstack-size
                        runtime-config:heap-size)
         (super interp-C (Program info (CFG G)))]
        [else
         (error "R3/interp-C unhandled" ast)]
        ))

    (define/override (interp-x86-exp env)
      (lambda (ast)
	(vomit "interp-x86-exp" ast)
	(match ast
	  [(Global label) (fetch-global label)]
	  [(Deref r i) #:when (not (eq? r 'rbp))
	   (define base ((interp-x86-exp env) (Reg r)))
	   (define addr (+ base i))
	   ((memory-read) addr)]
	  [else ((super interp-x86-exp env) ast)])))

    (define/public (interp-x86-store env)
      (lambda (ast value)
	(vomit "interp-x86-store" ast value)
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
          (vomit "R3/interp-x86-instr" (car ast)))
	(match ast
          [(cons (Callq 'malloc) ss)
           (define num-bytes ((interp-x86-exp env) (Reg 'rdi)))
           ((interp-x86-instr
             `((rax . ,(allocate-page! 'malloc num-bytes)) . ,env))
	    ss)]
          [(cons (Callq 'alloc) ss)
           (define num-bytes ((interp-x86-exp env) (Reg 'rdi)))
           ((interp-x86-instr
             `((rax . ,(allocate-page! 'alloc num-bytes)) . ,env))
	    ss)]
          [(cons (Callq 'collect) ss)
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
        (vomit "R3/interp-pseudo-x86" ast)
	(match ast
	  [(Program info (CFG G))
           ;(define ty (dict-ref info 'type))
	    ((initialize!) runtime-config:rootstack-size
	     runtime-config:heap-size)
	   (define env (cons (cons 'r15 (unbox rootstack_begin)) '()))
	   (parameterize ([get-CFG G])
	      (let ([env^ ((interp-x86-block env) (dict-ref G 'start))])
		(display-by-type 'Integer (lookup 'rax env^))))]
          [else ((super interp-pseudo-x86 env) ast)])))

    ;; after register allocation
    (define/override (interp-x86 env)
      (lambda (ast)
        (vomit "R3/interp-x86" ast)
	(match ast
	  [(Program info (CFG G))
           #:when (dict-has-key? info 'num-spills)
           ;(define ty (dict-ref info 'type))
           (define spills (dict-ref info 'num-spills))
           (define variable-size 8) ;; ugh -Jeremy
           (define root-space (* variable-size (cdr spills)))
           ((initialize!) runtime-config:rootstack-size
                          runtime-config:heap-size)
	   (define env (cons (cons 'r15 (+ root-space (unbox rootstack_begin)))
			     '()))
	   (parameterize ([get-CFG G])
	      (let ([env^ ((interp-x86-block env) (dict-ref G 'start))])
		(display-by-type 'Integer (lookup 'rax env^))))]
          [else
           ((super interp-x86 env) ast)]
          )))
    
    ));; interp-R3-class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R4: functions

(define interp-R4-class
  (class interp-R3-class
    (super-new)
    (inherit primitives interp-op initialize! display-by-type
             return-from-tail interp-x86-block memory-read memory-write!)

    (inherit-field result rootstack_begin free_ptr fromspace_end
		   uninitialized)

    (define/public (non-apply-ast)
      (set-union (primitives)
		 (set 'if 'let 'define 'program 'has-type 'void)))

    (define/public (interp-scheme-def d)
      (match d
        [(Def f `([,xs : ,ps] ...) rt info body)
         (mcons f `(lambda ,xs ,body ()))]
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
                              [`(lambda ,xs ,body ())
                               `(lambda ,xs ,body ,top-level)])))
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
	(verbose "R4/interp-F" ast)
	(define result
	(match ast
	  ;; For R4
	  [(Def f `([,xs : ,ps] ...) rt info body)
	   (cons f `(lambda ,xs ,body))]
	  [(FunRef f)
	   (lookup f env)]
	  [(Apply fun args)
	    (define fun-val ((interp-F env) fun))
	    (define arg-vals (map (interp-F env) args))
	    (match fun-val
	       [`(lambda (,xs ...) ,body)
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
	(verbose "R4/interp-C-exp" ast (map fst env))
	(match ast
          [(FunRef f)
           (lookup f env)]
          [(Call f args)
           (define arg-vals (map (interp-C-exp env) args))
           (define f-val ((interp-C-exp env) f))
           (match f-val
             [`(lambda (,xs ...) ,info ,G ,def-env)
              (define f (dict-ref info 'name))
              (define f-start (symbol-append f 'start))
              (define new-env (append (map cons xs arg-vals) def-env))
              (parameterize ([get-CFG G])
                ((interp-C-tail new-env) (dict-ref G f-start)))]
             [else (error "interp-C, expected a function, not" f-val)])]
          [else
           ((super interp-C-exp env) ast)]
           )))

    (define/override (interp-C-tail env)
      (lambda (ast)
	(verbose "R4/interp-C-tail" ast (map fst env))
	(match ast
          [(TailCall f args)
           (define arg-vals (map (interp-C-exp env) args))
           (define f-val ((interp-C-exp env) f))
           (match f-val
             [`(lambda (,xs ...) ,info ,G ,def-env)
              (define f (dict-ref info 'name))
              (define f-start (symbol-append f 'start))
              (define new-env (append (map cons xs arg-vals) def-env))
              (parameterize ([get-CFG G])
                ((interp-C-tail new-env) (dict-ref G f-start)))]
             [else (error "interp-C, expected a funnction, not" f-val)])]
          [else
           ((super interp-C-tail env) ast)]
          )))

    (define/public (interp-C-def ast)
      (verbose "R4/interp-C-def" ast)
      (match ast
        [(Def f `([,xs : ,ps] ...) rt info G)
         (mcons f `(lambda ,xs ((name . ,f)) ,G ()))]
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
                          [`(lambda ,xs ,info ,G ())
                           `(lambda ,xs ,info ,G ,top-level)])))
         ((interp-C-tail top-level) (TailCall (Var 'main) '()))]
        [else
         (error "R4/interp-C unhandled" ast)]
        ))

    (define (stack-arg-name n)
      (string->symbol (string-append "rsp_" (number->string n))))

    (define/public (builtin-funs)
      (set 'malloc 'alloc 'collect 'initialize 'read_int))

    (define/override (get-name ast)
      (match ast
        [(StackArg n) (stack-arg-name n)]
	 [else (super get-name ast)]))

    (define (call-function f-val cont-ss env)
      (match f-val
	[`(lambda ,info ,G ,def-env)
	 (debug "interp-x86 call-function" f-val)
         (define n (dict-ref info 'num-params))
         (define f (dict-ref info 'name))
         (define spills (dict-ref info 'num-spills #f))
	 ;; copy argument registers over to new-env
	 (define passing-regs
	   (filter (lambda (p) p)
		   (for/list ([r arg-registers])
                     (let ([v (lookup r env #f)])
                       (if v (cons r v) #f)))))
         (debug "interp-x86 call-function" passing-regs)
         (define new-env
           (cond [spills
                  (define variable-size 8) ;; ugh -Jeremy
                  (define root-size (* variable-size (cdr spills)))
                  (cons (cons 'r15 (+ root-size (unbox rootstack_begin)))
                        (append passing-regs def-env))]
                 [else
                  (cons (cons 'r15 (unbox rootstack_begin)) ;; ??? -Jeremy
                        (append passing-regs def-env))]))
         (define result-env
           (parameterize ([get-CFG G])
             ((interp-x86-block new-env)
              (dict-ref G (symbol-append f 'start)))))
         (define res (lookup 'rax result-env))
         ((interp-x86-instr (cons (cons 'rax res) env)) cont-ss)]
        [else (error "interp-x86, expected a function, not" f-val)]))

    (define/override (interp-x86-exp env)
      (lambda (ast)
        (vomit "R4/interp-x86-exp" ast)
	(match ast
	   [(StackArg n)
	    (define x (stack-arg-name n))
	    (lookup x env)]
	   [(FunRef f)
	    (lookup f env)]
	   [else ((super interp-x86-exp env) ast)])))

    (define (apply-closure clos arg cont-ss env)
      (define f ((memory-read) clos))
      (define env^ (append (list (cons 'rdi clos) (cons 'rsi arg)) env))
      (call-function f cont-ss env^))

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
          (vomit "R4/interp-x86-instr" (car ast)))
	(match ast
          ;; Treat lea like mov -Jeremy
          [(cons (Instr 'leaq (list s d)) ss)
           (define x (get-name d))
           (define v ((interp-x86-exp env) s))
           ((interp-x86-instr (cons (cons x v) env)) ss)]
          [(cons (IndirectCallq f) ss)
           (debug "indirect callq" ast)
           (define f-val ((interp-x86-exp env) f))
           (call-function f-val ss env)]
          [(cons (TailJmp f) ss)
           (debug "tail jmp" ast)
           (define f-val ((interp-x86-exp env) f))
           (call-function f-val '() env)]
          [(cons (Callq f) ss) 
           #:when (not (set-member? (builtin-funs) f))
           (call-function (lookup f env) ss env)]
          [else
           ((super interp-x86-instr env) ast)]
          )))

    (define/public (interp-x86-def ast)
      (match ast
        [(Def f ps rt info G)
         (mcons f `(lambda ,(dict-set info 'name f) ,G ()))]
        ))
        
    ;; The below applies before register allocation
    (define/override (interp-pseudo-x86 env)
      (lambda (ast)
        (vomit "R4/interp-pseudo-x86" ast)
	(match ast
          [(ProgramDefs info ds)
           ((initialize!) runtime-config:rootstack-size
                          runtime-config:heap-size)
            (define top-level (for/list ([d ds]) (interp-x86-def d)))
            ;; tie the knot
            (for/list ([b top-level])
              (set-mcdr! b (match (mcdr b)
                             [`(lambda ,xs ,body ())
                              `(lambda ,xs ,body ,top-level)])))
           (define env^ (list (cons 'r15 (unbox rootstack_begin))))
           (define result-env (call-function (lookup 'main top-level) '() env^))
           (display-by-type 'Integer (lookup 'rax result-env))]
          )))
    
    ;; The below applies after register allocation -JGS
    (define/override (interp-x86 env)
      (lambda (ast)
        (verbose "R4/interp-x86" ast)
	(match ast
          [(ProgramDefs info ds)
           ((initialize!) runtime-config:rootstack-size
                          runtime-config:heap-size)
           (define top-level (for/list ([d ds]) (interp-x86-def d)))
           ;; tie the knot
           (for/list ([b top-level])
             (set-mcdr! b (match (mcdr b)
                            [`(lambda ,xs ,body ())
                             `(lambda ,xs ,body ,top-level)])))
           ;; (define spills (dict-ref info 'num-spills))
           ;; (define variable-size 8) ;; ugh -Jeremy
           ;; (define root-size (* variable-size (cdr spills)))
           ;; (define env^ (list (cons 'r15 (+ root-size
           ;;                                  (unbox rootstack_begin)))))
           (define env^ '())
           (define result-env
             (call-function (lookup 'main top-level) '() env^))
           (display-by-type 'Integer (lookup 'rax result-env))]
          )))

    )) ;; end  interp-R4-class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R5: lambda

(define interp-R5-class
  (class interp-R4-class
    (super-new)
    (inherit initialize! return-from-tail)
    (inherit-field result)

    (define/override (non-apply-ast)
      (set-union (super non-apply-ast)
		 (set 'global-value 'allocate 'collect)))
    
    (define/override (interp-scheme-exp env)
      (lambda (ast)
	(verbose "R5/interp-scheme" ast)
	(match ast
	  [(Lambda `([,xs : ,Ts] ...) rT body)
	   `(lambda ,xs ,body ,env)]
	  [else ((super interp-scheme-exp env) ast)]
          )))

    (define/override (interp-F env)
      (lambda (ast)
	(verbose "R5/interp-F" ast)
        (define result
	(match ast
	  [(Lambda `([,xs : ,Ts] ...) rT body)
	   `(lambda ,xs ,body ,env)]
	  [(Def f `([,xs : ,ps] ...) rt info body)
	   (mcons f `(lambda ,xs ,body))]
	  [(ProgramDefs info ds)
	   ((initialize!) runtime-config:rootstack-size
	    runtime-config:heap-size)
	   (let ([top-level (map (interp-F '()) ds)])
	     ;; tie the knot
	     (for/list ([b top-level])
	       (set-mcdr! b (match (mcdr b)
			      [`(lambda ,xs ,body)
			       `(lambda ,xs ,body ,top-level)])))
	     ((interp-F top-level) (Apply (Var 'main) '())))]
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

(define interp-R6-class
  (class interp-R5-class
    (super-new)
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
         ['tag-of-any (lambda (v)
                        (match v
                          [`(tagged ,v^ ,ty)
                           (any-tag ty)]
                          [else
                           (error "interp expected tagged value, not" v)]))]
         ['value-of-any (lambda (v)
                          (match v
                            [`(tagged ,v^ ,ty)
                             v^]
                            [else
                             (error "interp expected tagged value, not" v)]))]
         ['tag-of-vector (lambda (v)
                           (match v
                             [`(vector-proxy ,vec ,rs ,ws) 1]
                             [else 0]))]
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
	(verbose "R6/interp-F" ast)
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
          #;[(TagOf e)
           (define v ((interp-F env) e))
           (match v
             [`(tagged ,v^ ,ty)
              (any-tag ty)]
             [else
              (error "interp expected tagged value, not" v)])]
          ;; The following belongs in a new R7 interp class -Jeremy
	  #;[(Def f xs _ info body)
	   (mcons f `(lambda ,xs ,body))]
	  #;[(Lambda xs _ body)
	   `(lambda ,xs ,body ,env)]
	  [(FunRefArity f n)
	   (lookup f env)]
          [(Prim 'and (list e1 e2))
           (if ((interp-F env) e1)
               ((interp-F env) e2)
               #f)]
	  [(Prim op args)
           (apply (interp-op op) (map (interp-F env) args))]
	  [else ((super interp-F env) ast)]
	  ))
        (verbose "R6/interp-F result of" ast result)
        result))

    (define/override (interp-C-exp env)
      (lambda (ast)
	(verbose "R6/interp-C-exp" ast)
	(match ast
	  [(Inject e t)
	   `(tagged ,((interp-C-exp env) e) ,t)]
	  [(Project e t2)
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
          #;[(TagOf e)
           ((interp-op 'tag-of-any) ((interp-C-exp env) e))]
	  [else
	   ((super interp-C-exp env) ast)]
	  )))

    (define/override (display-by-type ty val)
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

    (inherit-field x86-ops)
    (set! x86-ops (hash-set* x86-ops
		   'orq `(2 ,bitwise-ior)
		   'salq `(2 ,(lambda (n v) (arithmetic-shift v n)))
		   'sarq `(2 ,(lambda (n v) (arithmetic-shift v (- n))))
		   ))

    )) ;; interp-R6-class

