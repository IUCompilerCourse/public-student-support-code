#lang racket
(require racket/fixnum)
(require "utilities.rkt" (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-scheme interp-C interp-x86 interp-R0 interp-R1 interp-R2 interp-R3
     interp-R4 interp-R6 interp-r7)

(define interp-scheme
  (lambda (p)
    ((send (new interp-R6) interp-scheme '()) p)))

(define interp-C
  (lambda (p)
    ((send (new interp-R6) interp-C '()) p)))

(define interp-x86
  (lambda (p)
    ((send (new interp-R6) interp-x86 '()) p)))

;; This (dynamically scoped) parameter is used for goto
(define program (make-parameter '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R0: integer arithmetic and 'let'

(define interp-R0
  (class object%
    (super-new)

    (field (result (gensym 'result)))

    (define/public (primitives)
      (set '+ '- 'read))

    (define/public (interp-op op)
      (match op
         ['+ fx+]
     ['- (case-lambda
               [(n) (fx- 0 n)]
               [(n m) (fx- n m)])]
     ['read read-fixnum]
     [else (error "in interp-op S0, unmatched" op)]))

    (define/public (interp-scheme env)
      (lambda (ast)
        (verbose "R0/interp-scheme" ast)
    (match ast
           [(? symbol?)
        (lookup ast env)]
       [(? integer?) ast]
       [`(let ([,x ,(app (interp-scheme env) v)]) ,body)
        ((interp-scheme (cons (cons x v) env)) body)]
       [`(program ,e) ((interp-scheme '()) e)]
       [`(,op ,(app (interp-scheme env) args) ...)
        #:when (set-member? (primitives) op)
        (apply (interp-op op) args)]
       [else
        (error (format "no match in interp-scheme S0 for ~a" ast))]
       )))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; C0
    ;;
    ;; atomic   a  ::= n | x
    ;; expr     e  ::= a | (op a ...)
    ;; stmt     s  ::= (assign x e) | (return a)
    ;; program  p  ::= (program (x ...) s ...)

    (define/public (seq-C env)
      (lambda (ss)
    (let loop ([env env] [ss ss])
      (cond [(null? ss)
         env]
        [else
         (loop ((interp-C env) (car ss))
               (cdr ss))]))))

    (define/public (interp-C env)
      (lambda (ast)
        (vomit "R0/interp-C" ast env)
        (match ast
          [(? symbol? x) (lookup x env)]
          [(? integer? i) i]
          [`(assign ,x ,e)
           (let ([v ((interp-C env) e)])
             (cons (cons x v) env))]
          [`(return ,e)
           (let ([v ((interp-C env) e)])
             (cons (cons result v) env))]
          [`(program ,xs ,ss ...)
           (define env ((seq-C '()) ss))
           (lookup result env)]
          [`(,op ,args ...) #:when (set-member? (primitives) op)
       (apply (interp-op op) (map (interp-C env) args))]
          [else
           (error "no match in interp-C0 for " ast)]
          )))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; psuedo-x86 and x86
    ;; s,d ::= (var x) | (int n) | (reg r) | (deref r n)
    ;; i   ::= (movq s d) | (addq s d) | (subq s d) | (imulq s d)
    ;;       | (negq d) | (callq f)
    ;; psuedo-x86 ::= (program (x ...) i ...)

    (define/public (get-name ast)
      (match ast
        [(or `(var ,x) `(reg ,x)) x]
    [`(deref rbp ,n) n]
        [else
         (error 'interp-R0/get-name "doesn't have a name: ~a" ast)]))

    (field [x86-ops (make-immutable-hash
                     `((addq 2 ,+)
               (imulq 2 ,*)
                       (subq 2 ,(lambda (s d) (- d s)))
                       (negq 1 ,-)))])

    (define/public (interp-x86-op op)
      (define (err)
        (error 'interp-R0/interp-x86-op "unmatched ~a" op))
      (cadr (hash-ref x86-ops op err)))



    (define/public (interp-x86-exp env)
      (lambda (ast)
        (vomit "interp-x86-exp" ast)
    (match ast
       [(or `(var ,x) `(reg ,x))
            (lookup (get-name ast) env)]
       [`(deref ,r ,n)
            (lookup (get-name ast) env)]
       [`(int ,n) n]
       [else
        (error 'interp-R0/interp-x86-exp "unhandled ~a" ast)])))

    (define/public (interp-x86 env)
      (lambda (ast)
        (when (pair? ast)
          (vomit "R0/interp-x86" (car ast)))
        (match ast
       ['() env]
       [`((callq read_int) . ,ss)
        ((interp-x86 (cons (cons 'rax (read)) env)) ss)]
           [`((movq ,s ,d) . ,ss)
            (define x (get-name d))
        (define v ((interp-x86-exp env) s))
        ((interp-x86 (cons (cons x v) env)) ss)]
       [`(program ,xs ,ss ...)
        (let ([env ((interp-x86 '()) ss)])
              (lookup 'rax env))]
       [`((,binary-op ,s ,d) . ,ss)
        (let ([s ((interp-x86-exp env) s)]
          [d ((interp-x86-exp env) d)]
                  [x (get-name d)]
          [f (interp-x86-op binary-op)])
          ((interp-x86 (cons (cons x (f d s)) env)) ss))]
       [`((,unary-op ,d) . ,ss)
        (let ([d ((interp-x86-exp env) d)]
          [x (get-name d)]
          [f (interp-x86-op unary-op)])
          ((interp-x86 (cons (cons x (f d)) env)) ss))]
       [else (error "no match in interp-x86 S0 for " ast)]
       )))

    )) ;; class interp-R0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R1: Booleans and conditionals

(define interp-R1
  (class interp-R0
    (super-new)

    (inherit seq-C)
    (inherit-field x86-ops)

    (define/override (primitives)
      (set-union (super primitives)
         (set 'eq? 'and 'not '< '<= '> '>=)))

    (define/override (interp-op op)
      (match op
         ['eq? (lambda (v1 v2)
                 (cond [(and (fixnum? v1) (fixnum? v2)) (eq? v1 v2)]
                       [(and (boolean? v1) (boolean? v2)) (eq? v1 v2)]))]
     ['and (lambda (v1 v2)
         (cond [(and (boolean? v1) (boolean? v2))
            (and v1 v2)]))]
         ['not (lambda (v) (match v [#t #f] [#f #t]))]
         ['< (lambda (v1 v2)
           (cond [(and (fixnum? v1) (fixnum? v2)) (< v1 v2)]))]
         ['<= (lambda (v1 v2)
           (cond [(and (fixnum? v1) (fixnum? v2)) (<= v1 v2)]))]
         ['> (lambda (v1 v2)
           (cond [(and (fixnum? v1) (fixnum? v2)) (> v1 v2)]))]
         ['>= (lambda (v1 v2)
           (cond [(and (fixnum? v1) (fixnum? v2)) (>= v1 v2)]))]
     [else (super interp-op op)]))

    (define/override (interp-scheme env)
      (lambda (ast)
        (verbose "R1/interp-scheme" ast)
    (match ast
          [`(has-type ,e ,t) ((interp-scheme env) e)]
          [#t #t]
          [#f #f]
          [`(and ,e1 ,e2)
           (match ((interp-scheme env) e1)
             [#t (match ((interp-scheme env) e2)
                   [#t #t] [#f #f])]
             [#f #f])]
          [`(if ,cnd ,thn ,els)
           (if ((interp-scheme env) cnd)
               ((interp-scheme env) thn)
               ((interp-scheme env) els))]
          [`(program (type ,ty) ,e) ((interp-scheme '()) e)]
          [else ((super interp-scheme env) ast)]
          )))

    (define/override (interp-C env)
      (lambda (ast)
    (vomit "R1/interp-C" ast)
    (match ast
          [`(has-type ,e ,t) ((interp-C env) e)]
          [#t #t]
          [#f #f]
          [`(if ,cnd ,thn ,els)
           (if ((interp-C env) cnd)
               ((seq-C env) thn)
               ((seq-C env) els))]
          [`(program ,xs (type ,ty) ,ss ...)
           ((super interp-C env) `(program ,xs ,@ss))]
          [else ((super interp-C env) ast)]
          )))


    (define (goto-label label ss)
      (cond [(null? ss)
         (error "goto-label, couldn't find label" label)]
        [else
         (match (car ss)
        [`(label ,l) #:when (eq? l label)
         (cdr ss)]
        [else
         (goto-label label (cdr ss))])]))

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
     [`(byte-reg ,r)
      (super get-name `(reg ,(byte2full-reg r)))]
     [else (super get-name ast)]))

    ;; Extending the set of known operators is essentially the
    ;; same as overriding the interp-x86-op with new functionallity
    (set! x86-ops (hash-set* x86-ops
                   'notq `(1 ,bitwise-not)
                   'andq `(2 ,bitwise-and)
                   'xorq `(2 ,bitwise-xor)))

    (define/override (interp-x86-exp env)
      (lambda (ast)
        (vomit "interp-x86-exp" ast)
    (match ast
          [`(byte-reg ,r)
           ((interp-x86-exp env) `(reg ,(byte2full-reg r)))]
          [#t 1]
          [#f 0]
          [`(eq? ,e1 ,e2)
           (if (eq? ((interp-x86-exp env) e1)
                    ((interp-x86-exp env) e2))
               1 0)]
      [`(< ,e1 ,e2)
           (if (< ((interp-x86-exp env) e1)
                    ((interp-x86-exp env) e2))
               1 0)]
      [`(<= ,e1 ,e2)
           (if (<= ((interp-x86-exp env) e1)
                    ((interp-x86-exp env) e2))
               1 0)]
      [`(> ,e1 ,e2)
           (if (> ((interp-x86-exp env) e1)
                    ((interp-x86-exp env) e2))
               1 0)]
      [`(>= ,e1 ,e2)
           (if (>= ((interp-x86-exp env) e1)
                    ((interp-x86-exp env) e2))
               1 0)]
          [else ((super interp-x86-exp env) ast)]
          )))

    (define (eflags-status env cc)
      (match cc
     ['e
      (define eflags ((interp-x86-exp env) '(reg __flag)))
      (arithmetic-shift (bitwise-and eflags #b1000000) -6)]
     ['l
          ;; Get the value of the lt flag which doesn't actually exist
          ;; the lt flag is simulated by overflow == sign for x86
      (define eflags ((interp-x86-exp env) '(reg __flag)))
      (define overflow (bitwise-and eflags #b100000000000))
      (define sign     (bitwise-and eflags #b000010000000))
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

    (define/override (interp-x86 env)
      (lambda (ast)
        (when (pair? ast)
          (vomit "R1/interp-x86" (car ast)))
        (match ast
          [`((set ,cc ,d) . ,ss)
           (define name (get-name d))
       (define val (eflags-status env cc))
       (verbose "set" cc val)
           ((interp-x86 (cons (cons name val) env)) ss)]
          ;; if's are present before patch-instructions
          [(or `((if ,cnd ,thn ,els) . ,ss)
               `((if ,cnd ,thn ,_ ,els ,_) . ,ss))
           (if (not (eq? 0 ((interp-x86-exp env) cnd)))
               ((interp-x86 env) (append thn ss))
               ((interp-x86 env) (append els ss)))]

          [`((label ,l) . ,ss)
           ((interp-x86 env) ss)]

          ;; cmpq performs a subq operation and examimines the state
          ;; of the result, this is done without overwriting the second
          ;; register. -andre
          ;; Notice that the syntax is very confusing
          ;; (cmpq ,s2 ,s1) (jl then) (jmp else) ...
          ;; (if (< s1 s2) then else)
          [`((cmpq ,s2 ,s1) . ,ss)
           (let* ([v1 ((interp-x86-exp env) s1)]
                  [v2 ((interp-x86-exp env) s2)]
                  [v3 (- v2 v1)]
                  [zero     (arithmetic-shift (b2i (eq? v3 0)) 6)]
                  [sign     (arithmetic-shift (b2i (< v3 0)) 7)]
                  ;; Our numbers do not overflow so this bit is always 0
                  [overflow (arithmetic-shift 0 11)]
                  [eflags (bitwise-ior overflow sign zero)])
             ((interp-x86 (cons (cons '__flag eflags) env)) ss))]
          [`((movzbq ,s ,d) . ,ss)
           (define x (get-name d))
           (define v ((interp-x86-exp env) s))
           ((interp-x86 (cons (cons x v) env)) ss)]
          [`((jmp ,label) . ,ss)
           ((interp-x86 env) (goto-label label (program)))]
          [`((jmp-if ,cc ,label) . ,ss)
       (cond [(eq? (eflags-status env cc) 1)
          ((interp-x86 env) (goto-label label (program)))]
         [else ((interp-x86 env) ss)])]
       [`(program ,xs (type ,ty) ,ss ...)
            (display-by-type ty ((interp-x86 env) `(program ,xs ,@ss)))]
       [`(program ,xs ,ss ...)
        (parameterize ([program ss])
         ((super interp-x86 '()) ast))]
       [else ((super interp-x86 env) ast)]
       )))

    (define/public (display-by-type ty val)
      (match ty
        ['Boolean (if val #t #f)]
        ['Integer val]
        [else (error (format "don't know how to display type ~a" ty))]))

    ));; class interp-R1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R2: Vectors

(define interp-R2
  (class interp-R1
    (super-new)
    (inherit get-name seq-C interp-x86-op)
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
       [free_ptr        (box uninitialized)]
       [fromspace_end   (box uninitialized)]
       [rootstack_begin (box uninitialized)]
           [global-label-table
            (make-immutable-hash
             `((free_ptr         . ,free_ptr)
               (fromspace_begin  . ,fromspace_begin)
               (fromspace_end    . ,fromspace_end)
               (rootstack_begin  . ,rootstack_begin)
               (rootstack_end    . ,rootstack_end)))])

    (define/public (memory-read)
      (lambda (addr)
        (let-values ([(start stop name vect) (fetch-page addr)])
          (let ([value (vector-ref vect (arithmetic-shift (- addr start) -3))])
            (when (equal? value uninitialized)
              (error 'interp-R2/memory-read
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
                (set-box! fromspace_end   (+ h-begin hs))
                (set-box! free_ptr        h-begin))))))

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
          (set-box! free_ptr        h-begin))))

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
           (vomit "R2/fetch page" addr min max name vect)
           (if (and (<= min addr) (< addr max))
               (values min max name vect)
               (search rest-memory))]
          [other (error 'fetch-page "unmatched ~a" m)])))

    (define/override (primitives)
      (set-union (super primitives)
         (set 'vector 'vector-ref 'vector-set!)))

    (define/override (interp-op op)
      (match op
        ['eq? (lambda (v1 v2)
                (cond [(or (and (fixnum? v1) (fixnum? v2))
                           (and (boolean? v1) (boolean? v2))
                           (and (vector? v1) (vector? v2)))
                       (eq? v1 v2)]))]
        ['vector vector]
        ['vector-ref vector-ref]
        ['vector-set! vector-set!]
        [else (super interp-op op)]))

    (define/override (interp-scheme env)
      (lambda (ast)
        (verbose "R2/interp-scheme" ast)
    (match ast
          [`(void) (void)]
      [`(global-value free_ptr)
       (unbox free_ptr)]
      [`(global-value fromspace_end)
       (unbox fromspace_end)]
          [`(allocate ,l ,ty) (build-vector l (lambda a uninitialized))]
          [`(collect ,size)
           (unless (exact-nonnegative-integer? ((interp-scheme env) size))
             (error 'interp-C "invalid argument to collect in ~a" ast))
           (void)]
          [`(program (type ,ty) ,e)
       ((initialize!) runtime-config:rootstack-size
        runtime-config:heap-size)
       ((interp-scheme '()) e)]
      ;; [`(initialize ,stack-size ,heap-size)
      ;;  ((initialize!) stack-size heap-size)
      ;;  (void)]
          [else ((super interp-scheme env) ast)]
          )))

    (define (mem-error message expr)
      (lambda (who fmt . args)
        (error who "~a in ~a raise error:\n~a"
               message expr
               (apply format (cons fmt args)))))

    (define (global-value-err ast)
      (lambda ()
        (error 'interp-R2 "global label is unknown in ~a" ast)))


    (define/public (fetch-global label)
      (let* ([err (global-value-err label)]
             [ref (hash-ref global-label-table label err)]
             [value (unbox ref)])
        (when (equal? value uninitialized)
          (debug "fetch" global-label-table)
          (error 'interp-R2/fetch-global
                 "global value, ~a, used before initialization"
                 label))
        value))

    (define/override (interp-C env)
      (lambda (ast)
        (vomit "R2/interp-C" ast)
        (match ast
          [`(void) (void)]
      [`(global-value free_ptr)
       (unbox free_ptr)]
      [`(global-value fromspace_end)
       (unbox fromspace_end)]
          ;; I should do better than make these noops - andre
          ;; [`(initialize ,s ,h)
          ;;  (unless (and (exact-nonnegative-integer? s)
          ;;               (exact-nonnegative-integer? h))
          ;;    (error "intialize must be called with literals"))
          ;;  ((initialize!) s h)
          ;;  env]
          ;; Determine if a collection is needed.
          ;; Which it isn't because vectors stored in the environment
          ;; is the representation of the heap in the C language,
          ;; but collection is a no-op so we should check to see if
          ;; everything is well formed anyhow.
          [`(collection-needed? ,size)
           (when (or (eq? (unbox free_ptr) uninitialized)
                     (eq? (unbox fromspace_end) uninitialized))
             (error 'interp-C "uninitialized state in ~a" ast))
           #t]
          ;; Collection isn't needed or possible in this representation
          [`(collect ,size)
           (unless (exact-nonnegative-integer? ((interp-C env) size))
             (error 'interp-C "invalid argument to collect in ~a" ast))
           env]
          [`(collect ,rs ,size)
           (unless (and (exact-nonnegative-integer? ((interp-C env) rs))
                        (exact-nonnegative-integer? ((interp-C env) size)))
             (error 'interp-C "invalid argument(s) to collect in ~a" ast))
           env]
          ;; allocate a vector of length l and type t that is initialized.
          [`(allocate ,l ,ty) (build-vector l (lambda a uninitialized))]
          ;; Analysis information making introduce rootstack easier
          [`(call-live-roots (,xs ...) ,ss ...)
       ;; roots can also be any's -Jeremy
           #;(for ([x (in-list xs)])
             (unless (vector? (lookup x env))
               (error 'interp-C
                      "call-live-roots stores non-root ~a in ~a" x ast)))
           ((seq-C env) ss)]
          [`(program ,xs (type ,ty) ,ss ...)
           ((initialize!) runtime-config:rootstack-size
            runtime-config:heap-size)
           ((super interp-C env) `(program ,xs ,@ss))]
          [otherwise ((super interp-C env) ast)])))

    (define/override (interp-x86-exp env)
      (lambda (ast)
        (vomit "interp-x86-exp" ast)
        (match ast
          [`(global-value ,label) (fetch-global label)]
          [`(deref ,r ,i) #:when (not (eq? r 'rbp))
           (define base ((interp-x86-exp env) `(reg ,r)))
           (define addr (+ base i))
           ((memory-read) addr)]
          [else ((super interp-x86-exp env) ast)])))

    (define/public (interp-x86-store env)
      (lambda (ast value)
        (vomit "interp-x86-store" ast value)
        (match ast
          [`(global-value ,label)
           (define loc (hash-ref global-label-table label (global-value-err ast)))
           (set-box! loc value)
           env]
          [`(deref ,r ,i) #:when (not (eq? r 'rbp))
           (define base ((interp-x86-exp env) `(reg ,r)))
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

    #;(interp-x86 : (env -> (R2-stmt -> env)))
    (define/override (interp-x86 env)
      (lambda (ast)
        (when (pair? ast)
          (vomit "R2/interp-x86" (car ast)))
    (match ast
          [`((callq malloc) . ,ss)
           (define num-bytes ((interp-x86-exp env) '(reg rdi)))
           ((interp-x86 `((rax . ,(allocate-page! 'malloc num-bytes)) . ,env))
        ss)]
          [`((callq alloc) . ,ss)
           (define num-bytes ((interp-x86-exp env) '(reg rdi)))
           ((interp-x86 `((rax . ,(allocate-page! 'alloc num-bytes)) . ,env))
        ss)]
          [`((callq collect) . ,ss)
           (define rootstack ((interp-x86-exp env) '(reg rdi)))
           (define bytes-requested ((interp-x86-exp env) '(reg rsi)))
           ((collect!) rootstack bytes-requested)
           ((interp-x86 env) ss)]
          [`((movq ,s ,d) . ,ss)
           (define value   ((interp-x86-exp env) s))
           (define new-env ((interp-x86-store env) d value))
           ((interp-x86 new-env) ss)]
          [`((,(? x86-binary-op? binop) ,s ,d) . ,ss)
           (define src ((interp-x86-exp env) s))
           (define dst ((interp-x86-exp env) d))
           (define op  (interp-x86-op binop))
           (define new-env ((interp-x86-store env) d (op src dst)))
           ((interp-x86 new-env) ss)]
          [`((,(? x86-unary-op? unary-op) ,d) . ,ss)
           (define dst ((interp-x86-exp env) d))
           (define op  (interp-x86-op unary-op))
           (define new-env ((interp-x86-store env) d (op dst)))
           ((interp-x86 new-env) ss)]
      ;; The below applies after register allocation -JGS
      [`(program (,stack-space ,root-space) (type ,ty) ,ss ...)
       #:when (and (integer? stack-space) (integer? root-space))
        ((initialize!) runtime-config:rootstack-size
         runtime-config:heap-size)
       (define env (cons (cons 'r15 (+ root-space (unbox rootstack_begin)))
                 '()))
       (parameterize ([program ss])
          (let ([env^ ((interp-x86 env) ss)])
        (display-by-type ty (lookup 'rax env^))))]
      ;; The below applies before register allocation
      [`(program ,xs (type ,ty) ,ss ...)
        ((initialize!) runtime-config:rootstack-size
         runtime-config:heap-size)
       (define env (cons (cons 'r15 (unbox rootstack_begin)) '()))
       (parameterize ([program ss])
          (let ([env^ ((interp-x86 env) ss)])
        (display-by-type ty (lookup 'rax env^))))]
          [else ((super interp-x86 env) ast)])))

    ));; interp-R2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R3: functions


(define interp-R3
  (class interp-R2
    (super-new)
    (inherit primitives seq-C display-by-type interp-op initialize!)
    (inherit-field result rootstack_begin free_ptr fromspace_end
           uninitialized)

    (define/public (non-apply-ast)
      (set-union (primitives)
         (set 'if 'let 'define 'program 'has-type 'void)))

    (define/override (interp-scheme env)
      (lambda (ast)
        (verbose "R3/interp-scheme" ast)
        (match ast
          [`(define (,f [,xs : ,ps] ...) : ,rt ,body)
           (cons f `(lambda ,xs ,body))]
          [`(program (type ,ty) ,ds ... ,body)
       ((interp-scheme '()) `(program ,@ds ,body))]
          [`(program ,ds ... ,body)
       ((initialize!) runtime-config:rootstack-size
        runtime-config:heap-size)
       (let ([env (map  (interp-scheme '()) ds)])
         ((interp-scheme env) body))]
          [`(,fun ,args ...) #:when (not (set-member? (non-apply-ast) fun))
       (define new-args (map (interp-scheme env) args))
           (define fun-val ((interp-scheme env) fun))
       (match fun-val
          [`(lambda (,xs ...) ,body)
           (define new-env (append (map cons xs new-args) env))
           ((interp-scheme new-env) body)]
          [else (error "interp-scheme, expected function, not" fun-val)])]
          [else ((super interp-scheme env) ast)]
      )))

    (define/public (interp-F env)
      (lambda (ast)
        (verbose "R3/interp-F" ast)
    (define result
        (match ast
      ;; For R4
          [`(define (,f [,xs : ,ps] ...) : ,rt ,body)
           (cons f `(lambda ,xs ,body))]
          [`(function-ref ,f)
           (lookup f env)]
          [`(app ,fun ,args ...)
        (define arg-vals (map (interp-F env) args))
        (define fun-val ((interp-F env) fun))
            (match fun-val
               [`(lambda (,xs ...) ,body)
                (define new-env (append (map cons xs arg-vals) env))
        ((interp-F new-env) body)]
               [else (error "interp-F, expected function, not" fun-val)])]
          [`(program (type ,ty) ,ds ... ,body)
        ((interp-F env) `(program ,@ds ,body))]
          [`(program ,ds ... ,body)
       ((initialize!) runtime-config:rootstack-size
        runtime-config:heap-size)
           (let ([top-level (map  (interp-F '()) ds)])
          ((interp-F top-level) body))]
      ;; For R3
      [`(global-value free_ptr)
       (unbox free_ptr)]
      [`(global-value fromspace_end)
       (unbox fromspace_end)]
          [`(allocate ,l ,ty) (build-vector l (lambda a uninitialized))]
          [`(collect ,size)
           (unless (exact-nonnegative-integer? size)
             (error 'interp-F "invalid argument to collect in ~a" ast))
           (void)]
          [`(void) (void)]
          ;; For R2
          [`(has-type ,e ,t) ((interp-F env) e)]
          [#t #t]
          [#f #f]
          [`(and ,e1 ,e2)
           (match ((interp-F env) e1)
             [#t (match ((interp-F env) e2)
                   [#t #t] [#f #f])]
             [#f #f])]
          [`(if ,cnd ,thn ,els)
           (if ((interp-F env) cnd)
               ((interp-F env) thn)
               ((interp-F env) els))]
      ;; For R1
      [(? symbol?)
       (lookup ast env)]
      [(? integer?) ast]
      [`(let ([,x ,e]) ,body)
       (let ([v ((interp-F env) e)])
         ((interp-F (cons (cons x v) env)) body))]
      [`(program ,e) ((interp-F '()) e)]
      [`(,op ,args ...) #:when (set-member? (primitives) op)
       (apply (interp-op op) (map (interp-F env) args))]
      ))
    (verbose "R3/interp-F" ast result)
    result
    ))

    (define/override (interp-C env)
      (lambda (ast)
    (verbose "R3/interp-C" ast)
    (match ast
       [`(define (,f [,xs : ,ps] ...) : ,rt ,locals ,ss ...)
        (cons f `(lambda ,xs ,@ss))]
       [`(function-ref ,f)
        (lookup f env)]
       [`(app ,f ,args ...)
        (define arg-vals (map (interp-C env) args))
        (define f-val ((interp-C env) f))
        (match f-val
           [`(lambda (,xs ...) ,ss ...)
        (define new-env (append (map cons xs arg-vals) env))
        (define result-env ((seq-C new-env) ss))
        (lookup result result-env)]
           [else (error "interp-C, expected a funnction, not" f-val)])]
           #;[`(program ,locals (type ,ty) (defines ,ds ...) ,ss ...)
            ((interp-C env) `(program ,locals (defines ,@ds) ,@ss))]
       [`(program ,locals (type ,ty) (defines ,ds ...) ,ss ...)
        ((initialize!) runtime-config:rootstack-size
         runtime-config:heap-size)
        (define new-env (map (interp-C '()) ds))
        (define result-env ((seq-C new-env) ss))
        (lookup result result-env)]
       [else ((super interp-C env) ast)])))

    (define (stack-arg-name n)
      (string->symbol (string-append "rsp_" (number->string n))))

    (define/public (builtin-funs)
      (set 'malloc 'alloc 'collect 'initialize 'read_int))

    (define/override (get-name ast)
      (match ast
         [`(stack-arg ,n) (stack-arg-name n)]
     [else (super get-name ast)]))

    (define (call-function f-val ss env)
      (match f-val
        [`(lambda ,n ,extra ,body-ss ...)
     (debug "interp-x86 call-function" f-val)
         ;; copy some register and stack locations over to new-env
         (define passing-regs
           (filter (lambda (p) p)
                   (for/list ([r arg-registers])
                     (assq r env))))
      (define passing-stack
        (for/list ([i (in-range
               0 (max 0 (- n (vector-length
                      arg-registers))))])
              (define name (stack-arg-name (* i 8)))
              (define val (lookup name env))
              (define index (+ 16 (* i 8)))
              (cons index val)))
      (define new-env
        (match extra
           [`(,stack-size ,root-size)
        #:when (and (integer? stack-size) (integer? root-size))
        (cons (cons 'r15 (+ root-size (unbox rootstack_begin)))
              (append passing-regs passing-stack env))]
         [else
          (append passing-regs passing-stack env)]))
      (define result-env
        (parameterize ([program body-ss])
              ((interp-x86 new-env) body-ss)))
      (define res (lookup 'rax result-env))
      ((interp-x86 (cons (cons 'rax res) env)) ss)]
     [else (error "interp-x86, expected a function, not" f-val)]))

    (define/override (interp-x86-exp env)
      (lambda (ast)
    (match ast
       [`(stack-arg ,n)
        (define x (stack-arg-name n))
        (lookup x env)]
       [`(function-ref ,f)
        (lookup f env)]
       [else ((super interp-x86-exp env) ast)])))

    (define/override (interp-x86 env)
      (lambda (ast)
        (when (pair? ast)
          (verbose "R3/interp-x86" (car ast)))
    (match ast
       [`(define (,f) ,n ,extra ,ss ...)
        (cons f `(lambda ,n ,extra ,@ss))]
       ;; Treat lea like mov -Jeremy
       [`((leaq ,s ,d) . ,ss)
        (define x (get-name d))
        (define v ((interp-x86-exp env) s))
        ((interp-x86 (cons (cons x v) env)) ss)]
       [`((indirect-callq ,f) . ,ss)
        (define f-val ((interp-x86-exp env) f))
        (call-function f-val ss env)]
       [`((callq ,f) . ,ss) #:when (not (set-member? (builtin-funs) f))
        (call-function (lookup f env) ss env)]
       ;; The below applies after register allocation -JGS
       [`(program (,stack-space ,root-space) (type ,ty)
              (defines ,ds ...) ,ss ...)
        #:when (and (integer? stack-space) (integer? root-space))
        ((initialize!) runtime-config:rootstack-size
         runtime-config:heap-size)
            (parameterize ([program ss])
           (define env (map (interp-x86 '()) ds))
           (define env^ (cons (cons 'r15 (+ root-space
                        (unbox rootstack_begin))) env))
           (define result-env ((interp-x86 env^) ss))
           (display-by-type ty (lookup 'rax result-env)))]
      ;; The below applies before register allocation
           [`(program ,extra (type ,ty) (defines ,ds ...) ,ss ...)
        ((initialize!) runtime-config:rootstack-size
         runtime-config:heap-size)
            (parameterize ([program ss])
           (define env (map (interp-x86 '()) ds))
           (define env^ (cons (cons 'r15 (unbox rootstack_begin)) env))
           (define result-env ((interp-x86 env^) ss))
           (display-by-type ty (lookup 'rax result-env)))]

       [else ((super interp-x86 env) ast)])))

    )) ;; end  interp-R3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R4: lambda

(define interp-R4
  (class interp-R3
    (super-new)
    (inherit initialize!)
    (inherit-field result)

    (define/override (non-apply-ast)
      (set-union (super non-apply-ast)
                 (set 'global-value 'allocate 'collect)))
    
    (define/override (interp-scheme env)
      (lambda (ast)
        (verbose "R4/interp-scheme" ast)
    (match ast
          [`(lambda: ([,xs : ,Ts] ...) : ,rT ,body)
           `(lambda ,xs ,body ,env)]
          [`(define (,f [,xs : ,ps] ...) : ,rt ,body)
           (mcons f `(lambda ,xs ,body))]
          [`(program (type ,ty) ,ds ... ,body)
        ((initialize!) runtime-config:rootstack-size
         runtime-config:heap-size)
           ((interp-scheme env) `(program ,@ds ,body))]
          [`(program ,ds ... ,body)
        ((initialize!) runtime-config:rootstack-size
         runtime-config:heap-size)
           (let ([top-level (map (interp-scheme '()) ds)])
             ;; Use set-cdr! on define lambda's for mutual recursion
             (for/list ([b top-level])
               (set-mcdr! b (match (mcdr b)
                              [`(lambda ,xs ,body)
                               `(lambda ,xs ,body ,top-level)])))
             ((interp-scheme top-level) body))]
      [`(,fun ,args ...) #:when (not (set-member? (non-apply-ast) fun))
       (define arg-vals (map (interp-scheme env) args))
       (define fun-val ((interp-scheme env) fun))
       (match fun-val
          [`(lambda (,xs ...) ,body ,lam-env)
           (define new-env (append (map cons xs arg-vals) lam-env))
           ((interp-scheme new-env) body)]
          [else (error "interp-scheme, expected function, not" fun-val)])]
          [else ((super interp-scheme env) ast)]
      )))

    (define/override (interp-F env)
      (lambda (ast)
        (verbose "R4/interp-F" ast)
        (match ast
          [`(lambda: ([,xs : ,Ts] ...) : ,rT ,body)
           `(lambda ,xs ,body ,env)]
          [`(define (,f [,xs : ,ps] ...) : ,rt ,body)
           (mcons f `(lambda ,xs ,body))]
          [`(program (type ,ty) ,ds ... ,body)
           ((interp-F env) `(program ,@ds ,body))]
          [`(program ,ds ... ,body)
       ((initialize!) runtime-config:rootstack-size
        runtime-config:heap-size)
           (let ([top-level (map (interp-F '()) ds)])
             ;; Use set-cdr! on define lambda's for mutual recursion
             (for/list ([b top-level])
               (set-mcdr! b (match (mcdr b)
                              [`(lambda ,xs ,body)
                               `(lambda ,xs ,body ,top-level)])))
             ((interp-F top-level) body))]
      [`(app ,fun ,args ...)
       (define arg-vals (map (interp-F env) args))
       (define fun-val ((interp-F env) fun))
       (match fun-val
          [`(lambda (,xs ...) ,body ,lam-env)
           (define new-env (append (map cons xs arg-vals) lam-env))
           ((interp-F new-env) body)]
          [else (error "interp-F, expected function, not" fun-val)])]
          [else ((super interp-F env) ast)]
      )))

    )) ;; end interp-R4


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R6: type Any and inject/project

(define interp-R6
  (class interp-R4
    (super-new)
    (inherit-field result)

    (define/override (primitives)
      (set-union (super primitives)
         (set 'boolean? 'integer? 'vector? 'procedure?)))

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
                   (and (vector? v1) (vector? v2)))
               (eq? v1 v2)])]))]
     [else (super interp-op op)]
     ))

    (define/public (tyeq? t1 t2)
 ;;     (display t1) (display " ") (display t2) (newline) (flush-output)
      (match `(,t1 ,t2)
        [`((Vectorof ,t1) (Vector ,t2s ...))
         (foldr (lambda (x y) (and x y)) #t (map (lambda (x) (tyeq? t1 x)) t2s))] ;; wtf racket, why cant i just pass and?
        [`((Vector ,t1s ...) (Vectorof ,t2))
         (foldr (lambda (x y) (and x y)) #t (map (lambda (x) (tyeq? t2 x)) t1s))]
        [else (equal? t1 t2)]))

    (define/override (interp-scheme env)
      (lambda (ast)
        (verbose "R6/interp-scheme" ast)
    (define recur (interp-scheme env))
    (match ast
          [`(inject ,(app recur v) ,t)
       `(tagged ,v ,t)]
      [`(project ,(app recur v) ,t2)
       (match v
          [`(tagged ,v1 ,t1)
           (cond [(tyeq? t1 t2) v1]
             [else (error "in project, type mismatch" t1 t2)])]
          [else (error "in project, expected injected value" v)])]
      [else
       ((super interp-scheme env) ast)]
      )))

    (define/override (interp-F env)
      (lambda (ast)
        (verbose "R6/interp-F" ast)
    (define recur (interp-F env))
        (match ast
          [`(inject ,(app recur v) ,t)
       `(tagged ,v ,t)]
      [`(project ,(app recur v) ,t2)
       (match v
          [`(tagged ,v1 ,t1)
           (cond [(tyeq? t1 t2) v1]
             [else (error "in project, type mismatch" t1 t2)])]
          [else (error "in project, expected injected value" v)])]
          [else ((super interp-F env) ast)]
      )))

    (define/override (interp-C env)
      (lambda (ast)
    (verbose "R6/interp-C" ast)
    (match ast
          [`(inject ,e ,t)
       `(tagged ,((interp-C env) e) ,t)]
      [`(project ,e ,t2)
       (define v ((interp-C env) e))
       (match v
          [`(tagged ,v1 ,t1)
           (cond [(tyeq? t1 t2)
              v1]
             [else
              (error "in project, type mismatch" t1 t2)])]
          [else
           (error "in project, expected injected value" v)])]
      [else
       ((super interp-C env) ast)]
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

    )) ;; interp-R6


(define get-tagged-type
  (lambda (e)
    (match e
      [`(tagged ,v ,ty) ty])))

(define valid-op?
  (lambda (op)
    (member op '(+ - and or not))))

(define interp-r7-op
  (lambda (op es)
    (match `(,op ,es)
      [`(+ ((tagged ,v1 Integer) (tagged ,v2 Integer)))
       `(tagged ,(fx+ v1 v2) Integer)]
      [`(- ((tagged ,v Integer)))
       `(tagged ,(fx- 0 v) Integer)]
      [`(and (,v1 ,v2))
       (match v1
         [`(tagged #f Boolean) v1]
         [else v2])]
      [`(or (,v1 ,v2))
       (match v1
         [`(tagged #f Boolean) v2]
         [else v1])]
      [`(not (,v1))
       (match v1
         [`(tagged #f Boolean) `(tagged #t Boolean)]
         [else `(tagged #f Boolean)])])))
         
(define (interp-r7 env)
  (lambda (ast)
    (vomit "interp-r7" ast env)
    (define recur (interp-r7 env))
    (match ast
      [(? symbol?) (lookup ast env)]
      [`(function-ref ,f) (lookup f env)]
      [`(function-ref ,f ,n) (lookup f env)] ;; This is to deal with the detail of our translation that it keeps the arity of functions in the funref 
      [(? integer?) `(tagged ,ast Integer)]
      [#t `(tagged #t Boolean)]
      [#f `(tagged #f Boolean)]
      [`(read) `(tagged ,(read-fixnum) Integer)]
      [`(lambda (,xs ...) ,body)
       `(tagged (lambda ,xs ,body ,env) (,@(map (lambda (x) 'Any) xs) -> Any))]
      [`(define (,f ,xs ...) ,body)
       (mcons f `(lambda ,xs ,body))]
      [`(program ,ds ... ,body)
       (let ([top-level (map (interp-r7 '()) ds)])
         ;; Use set-cdr! on define lambda's for mutual recursion
         (for/list ([b top-level])
           (set-mcdr! b (match (mcdr b)
                          [`(lambda ,xs ,body)
                           `(tagged (lambda ,xs ,body ,top-level) 
                                    (,@(map (lambda (x) 'Any) xs) -> Any))])))
         ((interp-r7 top-level) body))]
      [`(vector ,es ...)
       (let* ([elts (map recur es)]
              [tys (map get-tagged-type elts)])
         `(tagged ,(apply vector (map recur es)) (Vector ,@tys)))]
      [`(vector-set! ,(app recur e1^) ,(app recur n^) ,(app recur e2^))
       (match e1^ 
	 [`(tagged ,vec ,ty) 
	  (match n^
	    [`(tagged ,n ,ty)
	     (vector-set! vec n e2^)
	     `(tagged (void) Void)])])]
      [`(vector-ref ,(app recur e^) ,(app recur n^))
       (match e^ 
	 [`(tagged ,vec ,ty) 
	  (match n^
	    [`(tagged ,n ,ty)
	     (vector-ref vec n)])])]
      [`(let ([,x ,e]) ,body)
       (let ([v (recur e)])
         ((interp-r7 (cons (cons x v) env)) body))]
      [`(,op ,es ...) #:when (valid-op? op)
       (interp-r7-op op (map recur es))]
      [`(eq? ,l ,r)
       `(tagged ,(equal? (recur l) (recur r)) Boolean)]
      [`(if ,q ,t ,f)
       (match (recur q)
         [`(tagged #f Boolean)
          (recur f)]
         [else (recur t)])]
      [`(app ,f ,es ...) 
       (define new-args (map recur es))
       (let ([f-val (recur f)])
         (match f-val 
           [`(tagged (lambda (,xs ...) ,body ,lam-env) ,ty)
            (define new-env (append (map cons xs new-args) lam-env))
            ((interp-r7 new-env) body)]
           [else (error "interp-r7, expected function, not" f-val)]))]
      [`(,f ,es ...)
       (define new-args (map recur es))
       (let ([f-val (recur f)])
         (match f-val 
           [`(tagged (lambda (,xs ...) ,body ,lam-env) ,ty)
            (define new-env (append (map cons xs new-args) lam-env))
            ((interp-r7 new-env) body)]
           [else (error "interp-r7, expected function, not" f-val)]))])))
