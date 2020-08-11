#lang racket

(require graph)

(require racket/pretty)
(require (for-syntax racket))
(require rackunit rackunit/text-ui)

(provide debug-level at-debug-level? debug verbose vomit
         map2 map3 b2i i2b
         racket-id->c-id 
         hash-union set-union*
         fix while 
         label-name lookup extend-env make-dispatcher assert
         fun-call? indirect-call?
         read-fixnum read-program 
	 compile compile-file check-passes-suite interp-tests compiler-tests
         compiler-tests-gui compiler-tests-suite
	 ;make-graph add-edge adjacent vertices 
         print-dot
         use-minimal-set-of-registers!
	 general-registers num-registers-for-alloc caller-save callee-save
	 arg-registers rootstack-reg register->color color->register
         registers align byte-reg->full-reg print-by-type strip-has-type
         make-lets dict-set-all dict-remove-all goto-label get-CFG 
         symbol-append any-tag
         tests-for all-tests)

;; debug state is a nonnegative integer.
;; The easiest way to increment it is passing the -d option
;; to run-tests.rkt
;; 0 none 
;; 1 trace passes in run-test
;; 2 debug macros
;; 3 verbose debugging
;; 4 (vomit) absolutely everything
;; The higher the setting the more information is reported.
;; If you want the same functionality as previous incarnation
;; of utilities then uncomment the line after this definition
;; and change the number there.
(define debug-level
  (make-parameter
   0
   (lambda (d)
     (unless (exact-nonnegative-integer? d) 
       (error 'debug-state "expected nonnegative-integer in ~a" d))
     d)))
;; (debug-level 2)

;; Check to see if debug state is at least some level
(define (at-debug-level? n)
  (unless (exact-nonnegative-integer? n)
    (error 'at-debug-level? "expected non-negative integer ~a" n))
  (>= (debug-level) n))

(define (test-verbosity)
  (cond [(>= (debug-level) 1) 'verbose]
        [else 'normal]))

(define all-tests
  (map (lambda (p) (car (string-split (path->string p) ".")))
       (filter (lambda (p) (string=? (cadr (string-split (path->string p) ".")) "rkt"))
               (directory-list (build-path (current-directory) "tests")))))

(define (tests-for r)
  (map (lambda (p)
         (cadr (string-split p "_")))
       (filter
        (lambda (p)
          (string=? r (car (string-split p "_"))))
        all-tests)))

;; print-label-and-values prints out the label followed the values
;; and the expression that generated those values
;; The label is formated with the file and line number of the
;; debubing expression for easier location.
(define-syntax (print-label-and-values stx)
  (syntax-case stx ()
    [(_ label value ...)
     (let* ([src (syntax-source stx)]
            [src (if (path? src)
                     (find-relative-path (current-directory) src)
                     src)]
            [lno (syntax-line stx)])
       #`(begin
           (printf "~a @ ~a:~a\n" label #,src #,lno)
        (begin
          (printf "~a:\n" 'value)
          (if (string? value)
              (display value)
              (pretty-display value))
          (newline))
        ...
        (newline)))]))

;; This series of macros are used for debuging purposes
;; and print out
(define-syntax-rule (define-debug-level name level)
    (...
     (define-syntax (name stx)
      (syntax-case stx ()
        [(_ label value ...)
         #`(when (at-debug-level? level)
             #,(syntax/loc stx
                 (print-label-and-values label value ...)))]))))

;; Print out debugging info in a somewhat organized manner
;; (debug "foo" (car '(1 2)) 'foo) should print
;; foo @ utilities.rkt:77
;; (car '(1 2)):
;; 1
;; 'foo:
;; foo
(define-debug-level trace 1)
(define-debug-level debug 2)
(define-debug-level verbose 3)
(define-debug-level vomit 4)

(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

(define fix (lambda (f) (lambda (x) ((f (fix f)) x))))

;; These functions convert between booleans and integers in
;; the regular C convention
(define (i2b i)
  (cond [(eq? i 0) #f]
        [else #t]))

(define (b2i b)
  (cond [b 1]
        [else 0]))

;; This function is like map but the function f returns
;; two values using the ``values'' form. Thus, the result
;; of map2 is two lists.
(define (map2 f ls)
  (cond [(null? ls)
         (values '() '())]
        [else
         (let-values ([(x1 x2) (f (car ls))]
                      [(ls1 ls2) (map2 f (cdr ls))])
           (values (cons x1 ls1) (cons x2 ls2)))]))

(define (map3 f ls)
  (cond [(null? ls)
         (values '() '() '())]
        [else
         (let-values ([(x1 x2 x3) (f (car ls))]
                      [(ls1 ls2 ls3) (map3 f (cdr ls))])
           (values (cons x1 ls1) (cons x2 ls2) (cons x3 ls3)))]))

;; set-union* takes a list of sets and unions them all together.
(define (set-union* ls)
  (foldl set-union (set) ls))

(define (hash-union . hs)
  (for*/hash ([h (in-list hs)]
              [(k v) (in-hash h)])
    (values k v)))

;; label-name prepends an underscore to a label (symbol or string)
;; if the current system is Mac OS and leaves it alone otherwise.
;; label-name : (U String Symbol) -> String
(define (label-name name)
  (unless (or (symbol? name) (string? name))
    (error 'label-name "expected string or symbol got ~s" name))
  (cond
    [(symbol? name) (label-name (symbol->string name))]
    [(eqv? (system-type 'os) 'macosx) (string-append "_" name)]
    [else name]))

(define (fun-call? s)
  (or (eq? s 'app) (eq? s 'tailcall)))

(define (indirect-call? s)
  (or (symbol=? s 'indirect-callq)
      (symbol=? s 'tail-jmp)))


;; The lookup function takes a key and an association list
;; and returns the corresponding value. It triggers an
;; error if the key is not present in the association list.
;;   
;; The association list may be constructed of either
;; immutable or mutable pairs.
;;
(define no-default (gensym))

(define (lookup x ls [default no-default])
  (let recur ([xs ls])
    (cond
      [(null? xs)
       (if (eq? default no-default)
           (error 'lookup "didn't find ~a in ~a" x ls)
           default)]
      [(pair? xs)
       (define fst (car xs))
       (cond
         [(and (pair?  fst) (equal? x (car  fst))) (cdr  fst)]
         [(and (mpair? fst) (equal? x (mcar fst))) (mcdr fst)]
         [(or (pair? fst) (mpair? fst)) (recur (cdr xs))]
         [else (error 'lookup "expected pair for ~a in ~a" fst ls)])]
      [else (error 'lookup "expected an association list, not ~a" ls)])))

(define (extend-env env x v)
  (cons (cons x v) env))

(define (read-fixnum)
  (define r (read))
  (cond [(fixnum? r) r]
	[else (error 'read "expected an integer")]))

;; Read an entire .rkt file wrapping the s-expressions in
;; a list whose head is 'program.
(define (read-program path)
  (unless (or (string? path) (path? path))
    (error 'read-program "expected a string in ~s" path))
  (unless (file-exists? path)
    (error 'read-program "file doesn't exist in ~s" path))
  (debug "utilities/read-program" path)
  (define input-prog
    (call-with-input-file path
      (lambda (f)
        `(program () ,@(for/list ([e (in-port read f)]) e)))))
  (debug "utilities/read-program" input-prog)
  input-prog)

(define (make-dispatcher mt)
  (lambda (e . rest)
    (match e
       [`(,tag ,args ...)
	(apply (hash-ref mt tag) (append rest args))]
       [else
	(error "no match in dispatcher for " e)]
       )))

;; The check-passes function takes a compiler name (a string), a
;; typechecker (see below), a description of the passes (see below),
;; and an initial interpreter to apply to the initial expression, and
;; returns a function that takes a test name and runs the passes and
;; the appropriate interpreters to test the correctness of all the
;; passes. This function assumes there is a "tests" subdirectory and a
;; file in that directory whose name is the test name followed by
;; ".rkt". Also, there should be a matching file with the ending ".in"
;; that provides the input for the Scheme program. If any program
;; should not pass typechecking, then there is a file with the name
;; number (whose contents are ignored) that ends in ".tyerr".
;;
;; The description of the passes is a list with one entry per pass.
;; An entry is a list with three things: a string giving the name of
;; the pass, the function that implements the pass (a translator from
;; AST to AST), and a function that implements the interpreter (a
;; function from AST to result value).
;;
;; The typechecker is a function of exactly one argument that EITHER
;; raises an error using the (error) function when it encounters a
;; type error, or returns #f when it encounters a type error. 

#;(define (strip-has-type e)
  e)

(define (strip-has-type e)
  (match e 
    [`(has-type ,e ,T)
     (strip-has-type e)]
    [`(,(app strip-has-type e*) ...)
      `(,@e*)]
    [else
     e]))

(define ((check-exception name test-name error-expected) fn)
  (with-handlers
    ([exn:fail?
      (lambda (exn)
        (cond [error-expected 'expected-error]
              [else
               (displayln (format "encountered exception while testing '~a`, case ~a" name test-name))
               (raise exn)]))])
    (let ([res (fn)])
      (when (and (not (string? res)) (not (pair? res)))
        (check-false error-expected (format "expected an error, not ~a" res)))
      res)))

(define ((check-passes-suite name typechecker passes initial-interp) test-name)
  (test-suite
   test-name
   (let* ([input-file-name (format "tests/~a.in" test-name)]
          [result-file-name (format "tests/~a.res" test-name)]
          [program-name (format "tests/~a.rkt" test-name)]
          [sexp (read-program program-name)]
          [error-expected (file-exists? (format "tests/~a.err" test-name))]
          [checker (check-exception name test-name error-expected)]
          [type-error-expected (file-exists? (format "tests/~a.tyerr" test-name))]
          [tsexp (checker (thunk (test-typecheck typechecker sexp)))])
     (test-case
       "typecheck"  
       (if type-error-expected 
           (check-false
            tsexp
            (format "expected type error in compiler '~a', case ~a, but no error raised by typechecker" name test-name))
           (check-not-false
            tsexp
            (format "expected no type error in compiler '~a', case ~a, but received error ~a" name test-name tsexp))))
     (trace "type checker output:" (strip-has-type tsexp))
     (unless type-error-expected
       (make-test-suite
        "passes"
        (let loop ([passes passes]
                   [p tsexp]
                   [result (cond [initial-interp
                                  (if (file-exists? input-file-name)
                                      (with-input-from-file input-file-name
                                        (lambda () (checker (thunk (initial-interp tsexp)))))
                                      (checker (thunk (initial-interp tsexp))))]
                                 [else 
                                  (if (file-exists? result-file-name)
                                      (call-with-input-file result-file-name
                                        (lambda (f) (string->number (read-line f))))
                                      42)])]
                   [tests '()])
          (trace "testing" test-name)
          (if (null? passes) (reverse tests)
              (match (car passes)
                [`(,pass-name ,pass ,interp)
                 (trace (string-append "running pass: " pass-name))
                 (let ([input p]
                       [new-p (checker (thunk (pass p)))])
                   (trace "pass output: " (strip-has-type new-p))
                   (if interp
                       (let ([new-result
                              (if (file-exists? input-file-name) 
                                  (with-input-from-file input-file-name
                                    (lambda () (checker (thunk (interp new-p)))))
                                  (checker (thunk (interp new-p))))])
                         (trace "running output " new-result)
                         (if result
                             (loop (cdr passes) new-p new-result
                                   (cons (test-suite (string-append "pass " pass-name)
                                                     (check-equal? new-result result 
                                                                   (format "differing results in compiler '~a' on test '~a' pass '~a', expected ~a, not ~a" name test-name pass-name result new-result)))
                                         tests))
                             (loop (cdr passes) new-p new-result tests)))
                       (loop (cdr passes) new-p result tests)))]))))))))                           
                                
(define (compile passes)
  (let ([prog-file-name (vector-ref (current-command-line-arguments) 0)])
    ((compile-file passes) prog-file-name)))

;; The compile-file function takes a typechecker and a description of
;; the compiler passes (see the comment for check-passes) and returns
;; a function that, given a program file name (a string ending in
;; ".rkt") that passes typechecking, applies all of the passes and
;; writes the output to a file whose name is the same as the proram
;; file name but with ".rkt" replaced with ".s". The function then
;; returns #t. If the program does not typecheck, the returned
;; function will return #f.
(define (compile-file typechecker passes)
  (lambda (prog-file-name)
    (define file-base (string-trim prog-file-name ".rkt"))
    (define out-file-name (string-append file-base ".s"))
    (trace "testing" prog-file-name)
    (call-with-output-file
      out-file-name
      #:exists 'replace
      (lambda (out-file)
        (define sexp (read-program prog-file-name))
        (define tsexp (test-typecheck typechecker sexp))
        (if tsexp
            (let ([x86 (let loop ([passes passes] [p tsexp])
                         (cond [(null? passes) p]
                               [else
                                (match (car passes)
                                  [`(,name ,pass ,interp)
                                   (let ([new-p ((check-exception name file-base #f) (thunk (pass p)))])
				     (trace (string-append "running pass: "
							   name)
					    (strip-has-type new-p))
                                     (loop (cdr passes) new-p)
                                     )])]))])
              (cond [(string? x86)
                     (write-string x86 out-file)
                     (newline out-file)
                     (flush-output out-file)
                     #t]
                    [else
                     (error "compiler did not produce x86 output")])
              )
            #f)
        ))))

;; The interp-tests function takes a compiler name (a string), a
;; typechecker (see the comment for check-passes) a description of the
;; passes (ditto) a test family name (a string), and a list of test
;; numbers, and runs the compiler passes and the interpreters to check
;; whether the passes correct.
;; 
;; This function assumes that the subdirectory "tests" has a bunch of
;; Scheme programs whose names all start with the family name,
;; followed by an underscore and then the test number, ending in
;; ".rkt". Also, for each Scheme program there is a file with the same
;; number except that it ends with ".in" that provides the input for
;; the Scheme program. If any program should not pass typechecking,
;; then there is a file with the name number (whose contents are
;; ignored) that ends in ".tyerr".

(define (interp-tests name typechecker passes initial-interp test-family
                      test-nums)
  (run-tests (interp-tests-suite name typechecker passes initial-interp
                                 test-family test-nums)
             (test-verbosity)))

(define (interp-tests-suite name typechecker passes initial-interp test-family
                            test-nums)
  (define checker-suite (check-passes-suite name typechecker passes
                                            initial-interp))
  (make-test-suite
   "interpreter tests"
   (for/list ([test-number (in-list test-nums)])
     (let ([test-name (format "~a_~a" test-family test-number)])
       (checker-suite test-name)))))


;; POLICY: we could use raw string comparison here, but it is nice to
;; be whitespace insensitive as long as we are creating valid S-exprs
;; as output.
(define (result-check res expected)
   (or
    (equal? res (string->number expected))
    (string=? res expected)
    (equal? (with-input-from-string res read)
            (with-input-from-string expected read))))

;; Use exponential backoff to poll/sleep until a timeout is reached.
;; Takes: a control function as produced by "process".
;; Returns: status: 'done-ok, 'done-error, 'timed-out
(define (wait-or-timeout control-fun maxsecs)
  (define (poll)    
    (match (control-fun 'status)
      ['running    #f]
      ['done-ok    'done-ok]
      ['done-error 'done-error]))
  (let loop ((slept 0) (delta 0.001)) ;; Start at 1ms    
    (define remain (- maxsecs slept))
    (debug (format "Polling subprocess, ~a elapsed" slept))
    (cond
      [(>= slept maxsecs) 'timed-out]
      [(< remain delta)
       (sleep remain)
       (or (poll) 'timed-out)]
      [else
       (define slept2 (+ slept delta))
       (sleep delta)
       (or (poll) (loop slept2 (* 2 delta)))])))


;; The compiler-tests function takes a compiler name (a string), a
;; typechecker (see the comment for check-passes) a description of the
;; passes (ditto), a test family name (a string), and a list of test
;; numbers (see the comment for interp-tests), and runs the compiler
;; to generate x86 (a ".s" file) and then runs gcc to generate machine
;; code, unless a type error is detected. It runs the machine code and
;; stores the result. If the test file has a corresponding .res file,
;; the result is compared against its contents; otherwise, the result
;; is compared against 42. If a type error is detected, it will check
;; if a .tyerr file exists, and report an error if not. It will do the
;; same if a .tyerr file exists but the typechecker does not report an
;; error.

(define (get-value-or-fail command output)
  (match (process command)
    [`(,in1 ,out ,_ ,inErr ,control-fun)
     (let* ([timeout 3.0]
	    [res (wait-or-timeout control-fun timeout)]
	    [result (cond [(symbol=? res 'timed-out) `(error timed-out ,timeout)]
			  [(symbol=? res 'done-error) `(error done-error ,(control-fun 'exit-code))]
			  [else `(result done ,(read-line in1))])])
       (close-input-port in1)
       (close-input-port inErr)
       (close-output-port out)
       result)]))

(define (compiler-tests-suite name typechecker passes test-family test-nums)
  (let ([compiler (compile-file typechecker passes)])
    (make-test-suite
     "compiler tests"
     (for/list ([test-number (in-list test-nums)])
       (let* ([test-name (format "~a_~a" test-family test-number)]
              [type-error-expected (file-exists? (format "tests/~a.tyerr" test-name))]
              [typechecks (compiler (format "tests/~a.rkt" test-name))])
         (test-suite
          test-name
          (test-case
           "compilation"
           (test-case
            "typecheck"
            (if type-error-expected
                (check-false typechecks "Expected expression to fail typechecking")
                (test-case "assembly"
                  (check-not-false typechecks "Expected expression to pass typechecking")
                  (let ([gcc-output (system (format "gcc -g -std=c99 runtime.o tests/~a.s -o tests/~a.out" test-name test-name))])
                    (check-not-false gcc-output "Failed during assembly")
                    (let ([input (if (file-exists? (format "tests/~a.in" test-name))
                                     (format " < tests/~a.in" test-name)
                                     "")]
                          [output (if (file-exists? (format "tests/~a.res" test-name))
                                      (call-with-input-file
                                        (format "tests/~a.res" test-name)
                                        (lambda (f) (read-line f)))
                                      "42")]
                          [error-expected (file-exists? (format "tests/~a.err" test-name))])
                      (let* ([command (format "./tests/~a.out ~a" test-name input)]
                             [result (get-value-or-fail command output)])
                        (test-case
                            "execution"
                          (check-not-false gcc-output "Unable to run program, gcc reported assembly failure")
                          (check-not-equal? (cadr result) 'timed-out (format "x86 execution timed out after ~a seconds" (caddr result)))
                          (cond [error-expected
                                 (check-equal? (cadr result) 'done-error (format "expected error, not: ~a" (caddr result)))
                                 (check-equal? (caddr result) 255 (format "expected error, not: ~a" (caddr result)))]
                                [else
                                 (check-not-eq? (cadr result) eof "x86 execution did not produce output")
                                 (check result-check (caddr result) output "Mismatched output from x86 execution")])))))))))))))))

(define (compiler-tests name typechecker passes test-family test-nums)
  (run-tests (compiler-tests-suite name typechecker passes test-family test-nums) (test-verbosity)))

(define (compiler-tests-gui name typechecker passes test-family test-nums)
  ((dynamic-require ''rackunit/gui 'test/gui) (compiler-tests-suite name typechecker passes test-family test-nums)))


;; Takes a function of 1 argument (or #f) and Racket expression, and
;; returns whether the expression is well-typed. If the first argument
;; is #f, that means we aren't providing a typechecker so we simply
;; return true. If not, we apply the typechecker to the expression. We
;; require that a typechecker will EITHER raise an error using the
;; (error) function when it encounters a type error, or that it
;; returns #f when it encounters a type error. This function then
;; returns whether a type error was encountered.
(define (test-typecheck tcer exp)
  (define (handler e)
    (vomit "test-typecheck" tcer exp e)
    (when (at-debug-level? 1)
	  (display (exn-message e))
	  (newline)(newline))
    #f)
  (if (eq? tcer #f)
      exp
      (let ([res (with-handlers ([exn:fail? handler])
                   (tcer exp))])
        (match res
          [#f #f]
          [`(program ,elts ...) res]
          [else exp]))))

(define assert
  (lambda (msg b)
    (if (not b)
	(begin
	  (display "ERROR: ")
	  (display msg)
	  (newline))
	(void))))

;; (case-> (symbol . -> . symbol) (string . -> . string))
(define (racket-id->c-id x)
  (define (->c-id-char c)
    (if (or (char<=? #\A c #\Z)
            (char<=? #\a c #\z)
            (char<=? #\0 c #\9))
        c
        #\_))
  (cond
    [(symbol? x) (string->symbol (racket-id->c-id (symbol->string x)))]
    [(string? x) (list->string (map ->c-id-char (string->list x)))]
    [else (error 'racket-id->c-id "expected string or symbol: ~v" x)]))

;; System V Application Binary Interface
;; AMD64 Architecture Processor Supplement
;; Edited by Jan Hubicˇka, Andreas Jaeger, Mark Mitchell
;; December 2, 2003

;; We reserve rax and r11 for patching instructions.
;; We reserve r15 for the rootstack pointer. 
(define rootstack-reg 'r15)
;; There are 11 other general registers
;; The ordering here indicates preference in the register allocator.
;; We put the caller-saved registers first.
(define general-registers (vector 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10
                                  'rbx  'r12 'r13 'r14))

(define arg-registers (void))
(define registers-for-alloc (void))

;; registers-for-alloc should always inlcude the arg-registers.
(define (use-minimal-set-of-registers! f)
  (if f
      (begin
        ;; need at least 2 arg-registers, see limit-functions -Jeremy
        ;(set! arg-registers (vector 'rcx 'rdx))
        (set! arg-registers (vector 'rcx 'rdx))
        ;(set! registers-for-alloc (vector 'rcx 'rdx)))
        (set! registers-for-alloc (vector 'rbx 'rcx 'rdx)))
      (begin
        (set! arg-registers (vector 'rcx 'rdx 'rdi 'rsi 'r8 'r9))
        (set! registers-for-alloc general-registers))))

(use-minimal-set-of-registers! #f)

;; We don't need to include the reserved registers
;; in the list of caller or callee save registers.
(define caller-save (set 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10))
(define callee-save (set 'rbx 'r12 'r13 'r14))

(define byte-register-table
  (make-immutable-hash
   `((ah . rax) (al . rax)
     (bh . rbx) (bl . rbx)
     (ch . rcx) (cl . rcx)
     (dh . rdx) (dl . rdx))))

(define (byte-reg->full-reg x)
  (let ([r? (hash-ref byte-register-table x #f)])
    (unless r?
      (error 'byte-reg->full-reg "invalid byte register ~a" x))
    r?))

;; The positive numbers here correspond to indices in the general-registers
;; and registers-for-alloc.
;; TODO: reorder to put caller-saved registers first, as in below. -Jeremy
#;(define reg-colors
  '((rax . -1) (r11 . -2) (r15 . -3) (rbp . -4) (__flag . -5)
    (rcx . 0) (rdx . 1) (rsi . 2) (rdi . 3) (r8 . 4) (r9 . 5) (r10 . 6)
    (rbx . 7) (r12 . 8) (r13 . 9) (r14 . 10)
    ))
#;(define reg-colors
  '((rax . -1) (r11 . -2) (r15 . -3) (rbp . -4) (__flag . -5)
    (rbx . 0) (rcx . 1) (rdx . 2) (rsi . 3) (rdi . 4)
    (r8 . 5) (r9 . 6) (r10 . 7) (r12 . 8) (r13 . 9)
    (r14 . 10)))

(define reg-colors
  '((rax . -1) (r11 . -2) (r15 . -3) (rbp . -4) (__flag . -5)))

(for ([r registers-for-alloc]
      [i (in-naturals)])
  (set! reg-colors (cons (cons r i) reg-colors)))

(define (register->color r)
  (cond [(assq r reg-colors) => (lambda (p) (cdr p))]
        [else -1])) ;; for registers not used in register allocator.
  
;;  (cdr (assq r reg-colors)))

(define (num-registers-for-alloc)
  (vector-length registers-for-alloc))

(define (color->register c)
  (vector-ref registers-for-alloc c))

(define registers (set-union (list->set (vector->list general-registers))
			     (set 'rax 'r11 'r15 'rsp 'rbp '__flag)))

(define (align n alignment)
  (cond [(eq? 0 (modulo n alignment))
	 n]
	[else
	 (+ n (- alignment (modulo n alignment)))]))


; Produces a string containing x86 instructions that print whatever is
; currently in %rax. Will clobber the contents of (potentially)
; r12-r15, so this should only be used at the end of a program (and
; before moving 0 to rax).  Note that the "[depth 12]" part here is an
; optional argument, which is used internally.  Call this function
; with a single argument, like (print-by-type '(Vector Integer
; Boolean)).  If you try to print nested vectors that are more than 4
; levels deep, the 5th vector will be printed as #(...).
(define (print-by-type ty [depth 12])
  (define (mov-and-print depth) 
    (lambda (ty index)
      (format "\tmovq\t~a(%r~a), %rax\n~a" (* 8 (+ 1 index)) depth (print-by-type ty (+ 1 depth)))))
  (match ty
    ['Any 
     (format "\tmovq\t%rax, %rdi\n\tcallq\t~a\n" (label-name "print_any"))]
    ['Void (format "\tcallq\t~a\n" (label-name "print_void"))]
    ['Integer 
     (format "\tmovq\t%rax, %rdi\n\tcallq\t~a\n" (label-name "print_int"))]
    ['Boolean 
     (format "\tmovq\t%rax, %rdi\n\tcallq\t~a\n" (label-name "print_bool"))]
    [`(Vector ,tys ...)
     (if (> depth 15)
         (format "\tmovq\t%rax, %rdi\n\tcallq\t~a\n" (label-name "print_ellipsis"))
         (string-join (map (mov-and-print depth) tys (range (length tys)))
                      (format "\tcallq\t~a\n" (label-name "print_space"))
                      #:before-first (format "\tmovq\t%rax, %r~a\n\tcallq\t~a\n" depth (label-name "print_vecbegin"))
                      #:after-last (format "\tcallq\t~a\n" (label-name "print_vecend"))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph Printing

(define (print-dot graph file-name)
  (if (at-debug-level? 1)
      (call-with-output-file file-name #:exists 'replace
	(lambda (out-file)
	  (write-string "strict graph {" out-file) (newline out-file)
	  
	  (for ([v (in-vertices graph)])
	       (write-string (format "~a;\n" v) out-file))
	  
	  (for ([v (in-vertices graph)])
	       (for ([u (in-neighbors graph v)])
		    (write-string (format "~a -- ~a;\n" u v) out-file)))
	  
	  (write-string "}" out-file)
	  (newline out-file)))
      '()))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscelaneous helper functions

(define (make-lets bs e)
  (cond [(null? bs) e]
        [(eq? (caar bs) '_)
         `(seq ,(cdr (car bs))
               ,(make-lets (cdr bs) e))]
        [else
         `(let ([,(car (car bs)) ,(cdr (car bs))])
            ,(make-lets (cdr bs) e))]))

(define (dict-remove-all dict keys)
  (for/fold ([d dict]) ([k keys])
    (dict-remove d k)))

(define (dict-set-all dict key-vals)
  (for/fold ([d dict]) ([(k v) (in-dict key-vals)])
    (dict-set d k v)))

;; This parameter (dynamically scoped thingy) is used for goto.
(define get-CFG (make-parameter '()))

(define (goto-label label)
  (dict-ref (get-CFG) label))

(define (symbol-append s1 s2)
  (string->symbol (string-append (symbol->string s1) (symbol->string s2))))

(define (any-tag ty)
  (match ty
    ['Integer 1]		;; 001
    ['Boolean 4]		;; 100
    ['Void 5]                   ;; 101
    [`(Vector ,ts ...) 2]	;; 010
    [`(Vectorof ,t) 2]
    [`(,ts ... -> ,rt) 3]	;; 011
    [else (error "in any-tag, unrecognized type" ty)]
    ))

