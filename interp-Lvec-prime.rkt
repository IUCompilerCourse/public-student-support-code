#lang racket
(require "interp-Lvec.rkt")
(require "utilities.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-Lvec-prime interp-Lvec-prime-mixin interp-Lvec-prime-class)

(define (interp-Lvec-prime-mixin super-class)
  (class super-class
    (super-new)

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
	      (error 'interp-Lvec-class/memory-read
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
           ;(copious "Lvec/fetch page" addr min max name vect)
           ; vect is too large to print, makes things hard to read.
           ;(copious "Lvec/fetch page" addr min max name)
           (if (and (<= min addr) (< addr max))
               (values min max name vect)
               (search rest-memory))]
          [other (error 'fetch-page "unmatched ~a" m)])))

    (define/override (interp-exp env)
      (lambda (ast)
        (define recur (interp-exp env))
	(verbose "interp-exp" ast)
	(match ast
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
             (error 'interp-exp "invalid argument to collect in ~a" ast))
           (void)]
	  [else ((super interp-exp env) ast)]
	  )))

    (define/override (interp-program ast)
      (verbose "interp-program" ast)
      (match ast
        [(Program info e)
         ((initialize!) runtime-config:rootstack-size
                        runtime-config:heap-size)
         ((interp-exp '()) e)]
        ))
    ))

(define interp-Lvec-prime-class (interp-Lvec-prime-mixin interp-Lvec-class))
    
(define (interp-Lvec-prime p)
  (send (new interp-Lvec-prime-class) interp-program p))
