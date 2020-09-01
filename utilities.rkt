;;
;;   WARNING!!!!
;;
;;   The original version of this file lives in the course-compiler repository.
;;   There is a copy of this file in the public-student-support-code
;;   repository.
;;
;;   DO NOT EDIT the version of this file in public-student-support-code
;;   because any changes you make will be OBLITERATED the next time
;;   someone edits the version in the course-compiler repository
;;   and then copies it over to public-student-support-code.

#lang racket
(require racket/struct)
(require graph)

;; Version 0.2
;; ---------------------
#|

This file is updated periodically based on the internal, reference
copy of the P423/523 compiler.

Changelog:

 0.1: initial release, used 2016-2017.

 0.2: update during Fall'17 semester.
        We removed these functions: 
         * interp-test-suite
         * compiler-test-suite
        We added:
         * parallel test running
         * various other changes

 0.3: updated during Fall'18 semester.
        We removed multithreading
        We added rackunit integration
|#

(require racket/pretty racket/match)
(require (for-syntax racket))
					;(require racket/async-channel)
(require rackunit rackunit/text-ui rackunit/gui)

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
         print-dot
         use-minimal-set-of-registers!
	 general-registers num-registers-for-alloc caller-save callee-save
	 arg-registers rootstack-reg register->color color->register
         registers align byte-reg->full-reg print-by-type strip-has-type
         make-lets dict-set-all dict-remove-all goto-label get-CFG 
         symbol-append any-tag parse-program
         
         (contract-out [struct Prim ((op symbol?) (arg* exp-list?))])
         (contract-out [struct Var ((name symbol?))])
         (contract-out [struct Int ((value fixnum?))])
         (contract-out [struct Let ((var symbol?) (rhs exp?) (body exp?))])
         (struct-out Program)
         (struct-out ProgramDefs)
         (contract-out [struct Bool ((value boolean?))])
         (contract-out [struct If ((cnd exp?) (thn exp?) (els exp?))])
         (contract-out [struct HasType ((expr exp?) (type type?))])
         (struct-out Void)
         (contract-out [struct Apply ((fun exp?) (arg* exp-list?))])
         (contract-out [struct Def ((name symbol?) (param* param-list?) (rty type?) (info any?)
                                    (body any?))])
         (contract-out [struct Lambda ((param* param-list?) (rty type?) (body exp?))])
         (contract-out [struct FunRef ((name symbol?))])
         (contract-out [struct FunRefArity ((name symbol?) (arity fixnum?))])
         (struct-out Inject)
         (struct-out Project)
         (struct-out ValueOf)
         (struct-out TagOf)
           
         (contract-out [struct Assign ((lhs lhs?) (rhs exp?))])
         (contract-out [struct Seq ((fst stmt?) (snd tail?))])
         (contract-out [struct Return ((arg exp?))])
         (contract-out [struct IfStmt ((cnd cmp?) (thn goto?) (els goto?))])
         (contract-out [struct Goto ((label symbol?))])
         (struct-out Collect)
         (struct-out CollectionNeeded?)
         (struct-out GlobalValue)
         (struct-out Allocate)
         (struct-out AllocateProxy)
         (contract-out [struct Call ((fun exp?) (arg* exp-list?))])
         (contract-out [struct TailCall ((fun exp?) (arg* exp-list?))])
         (struct-out CFG)
         
         (contract-out [struct Imm ((value fixnum?))])
         (contract-out [struct Reg ((name symbol?))])
         (contract-out [struct Deref ((reg symbol?) (offset fixnum?))])
         (contract-out [struct Instr ((name symbol?) (arg* arg-list?))])
         (contract-out [struct Callq ((target symbol?))])
         (contract-out [struct IndirectCallq ((target arg?))])
         (struct-out Retq)
         (contract-out [struct Jmp ((target symbol?))])
         (contract-out [struct TailJmp ((target arg?))])
         (contract-out [struct Block ((info any?) (instr* instr-list?))])
         (struct-out StackArg)

         (contract-out [struct JmpIf ((cnd symbol?) (target symbol?))])
         (contract-out [struct ByteReg ((name symbol?))])
         )

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
          (pretty-print value)
          #;(if (string? value)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract Syntax Tree struct definitions

(define (make-recur port mode)
  (case mode
    [(#t) write]
    [(#f) display]
    [else (lambda (p port) (print p port mode))]
    ))

(define (newline-and-indent port col)
  (let ([lead (if col (make-string col #\space) "")])
    (newline port)
    (write-string lead port)
    ))

(struct Var (name) #:transparent
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(Var x)
        (write x port)]))])

(struct Int (value) #:transparent
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(Int n)
        (write n port)]))])

(struct Prim (op arg*)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(Prim op arg*)
          (write-string "(" port)
          (write-string (symbol->string op) port)
          (for ([arg arg*])
            (write-string " " port)
            (recur arg port))
          (write-string ")" port)
          ])))])

(struct Let (var rhs body)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(Let x rhs body)
          (let-values ([(line col pos) (port-next-location port)])
            (write-string "(let ([" port)
            (write-string (symbol->string x) port)
            (write-string " " port)
            (recur rhs port)
            (write-string "])" port)
            (newline-and-indent port col)
            (write-string "   " port) ;; indent body
            (recur body port)
            (write-string ")" port)
            (newline-and-indent port col)
            )])))])

(struct CFG (block*) #:transparent
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(CFG G)
          (for/list ([(label tail) (in-dict G)])
            (write label port)
            (write-string ":" port)
            (newline port)
            (write-string "    " port)
            (recur tail port)
            (newline port))])))])

(define (print-info info port mode)
  (let ([recur (make-recur port mode)])
    (for ([(label data) (in-dict info)])
      (match label
        ['locals
         (write-string "locals:" port)
         (newline port)
         (cond [(dict? data)
                (write-string "    " port)
                (for ([(var type) (in-dict data)])
                  (write-string (symbol->string var) port)
                  (write-string " : " port)
                  (recur type port)
                  (write-string ", " port)
                  )
                (newline port)]
               [else
                (recur data port)
                (newline port)])]
        [else
         (write-string (symbol->string label) port)
         (write-string ":" port)
         (newline port)
         (recur data port)
         (newline port)
         ]))))

(struct Program (info body) #:transparent
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(Program info body)
          (write-string "program:" port)
          (newline port)
          (print-info info port mode)
          (cond [(list? body)
                 (for ([def body])
                   (recur def port)
                   (newline port))]
                [else
                 (recur body port)])])))])

(struct ProgramDefs (info def* body) #:transparent
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(ProgramDefs info def* body)
          (write-string "functions:" port)
          (newline port)
          (for ([def def*]) (recur def port))
          (newline port)
          (write-string "program:" port)
          (newline port)
          (recur body port)
          ])))])
  
(struct Bool (value) #:transparent
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(Bool b)
        (write b port)]))])

(struct If (cnd thn els)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(If cnd thn els)
          (let-values ([(line col pos) (port-next-location port)])
            (write-string "(if" port)
            (write-string " " port)
            (recur cnd port)
            (newline-and-indent port col)
            (write-string "   " port) ;; indent 
            (recur thn port)
            (newline-and-indent port col)
            (write-string "   " port) ;; indent 
            (recur els port)
            (newline-and-indent port col)
            (write-string ")" port)
            )])))])

(struct IfStmt (cnd thn els)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(IfStmt cnd thn els)
          (let-values ([(line col pos) (port-next-location port)])
            (write-string "if " port)
            (recur cnd port)
            (write-string " then" port)
            (newline-and-indent port col)
            (write-string "   " port) ;; indent 
            (recur thn port)
            (newline-and-indent port col)
            (write-string "else" port)
            (newline-and-indent port col)            
            (write-string "   " port) ;; indent 
            (recur els port)
            )])))])

(struct Void () #:transparent
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(Void)
        (write-string "(void)" port)]))])

(struct Apply (fun arg*)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(Apply fun arg*)
          (write-string "(" port)
          (recur fun port)
          (for ([arg arg*])
            (write-string " " port)
            (recur arg port))
          (write-string ")" port)
          ])))])

(define (write-type ty port)
  (match ty
    [`(Vector ,tys ...)
     (write-string "(Vector" port)
     (for ([ty tys])
       (write-string " " port)
       (write-type ty port))
     (write-string ")" port)]
    [`(,t1 -> ,t2)
     (write-string "(" port)
     (write-type t1 port)
     (write-string " -> " port)
     (write-type t2 port)
     (write-string ")" port)]
    [(? symbol?)
     (write-string (symbol->string ty) port)]
    [else
     (write-string "_" port)
     ]))
  
(define (write-params param* port)
  (for ([param param*])
    (match param
      [`(,x : ,t)
       (write-string " " port)
       (write-string "[" port)
       (write-string (symbol->string x) port)
       (write-string " : " port)
       (write-type t port)
       (write-string "]" port)])))

(struct Def (name param* rty info body)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(Def name ps rty info body)
          (let-values ([(line col pos) (port-next-location port)])
            (write-string "(define (" port)
            (write-string (symbol->string name) port)
            (write-string " " port)
            (write-params ps port)
            (write-string ") " port)
            (write-string ":" port)
            (write-string " " port)
            (write-type rty port)
            (newline-and-indent port col)
            (write-string "   " port)
            (cond [(list? body)
                   (for ([block body])
                     (cond [(pair? block)
                            (write-string (symbol->string (car block)) port)
                            (write-string ":" port)
                            (newline-and-indent port col)
                            (write-string "      " port)
                            (recur (cdr block) port)
                            (newline-and-indent port col)
                            (write-string "   " port)]
                           [else
                            (recur block port)
                            (newline-and-indent port col)
                            (write-string "   " port)]))]
                  [else
                   (recur body port)])
            (newline-and-indent port col)            
            (write-string ")" port)
            )
          ])))])

(struct Lambda (param* rty body)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(Lambda ps rty body)
          (let-values ([(line col pos) (port-next-location port)])
            (write-string "(lambda: " port)
            (write-params ps port)
            (write-string " " port)
            (write-string ":" port)
            (write-string " " port)
            (write-type rty port)
            (newline-and-indent port col)
            (write-string "   " port)
            (recur body port)
            (write-string ")" port)
            (newline-and-indent port col)
            )
          ])))])
  
(struct Inject (value type) #:transparent)
(struct ValueOf (value type) #:transparent)
(struct TagOf (value) #:transparent)
(struct Project (value type)  #:transparent)
  
(struct FunRef (name)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(FunRef f)
        (write f port)]))])
  
(struct FunRefArity (name arity)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(FunRefArity f n)
        (write f port)
        (write-string "{" port)
        (write n port)
        (write-string "}" port)
        ]))])
  
(define (Assign-print assign port mode)
  (let ([recur (make-recur port mode)])
    (match assign
      [(Assign lhs rhs)
       (let-values ([(line col pos) (port-next-location port)])
         (recur lhs port)
         (write-string " " port)
         (write-string "=" port)
         (write-string " " port)
         (recur rhs port)
         (write-string ";" port)
         )         
       ])))
(struct Assign (lhs rhs) #:transparent
  #:methods gen:custom-write
  [(define write-proc Assign-print)])

(struct Seq (fst snd) #:transparent
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(Seq fst snd)
          (let-values ([(line col pos) (port-next-location port)])
            (recur fst port)
            (newline-and-indent port col)
            (recur snd port)
            )])))])
  
(struct Return (arg)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(Return e)
          (let-values ([(line col pos) (port-next-location port)])
            (write-string "return" port)
            (write-string " " port)
            (recur e port)
            (write-string ";" port)
            )
          ])))])

(struct Goto (label)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(Goto label)
        (let-values ([(line col pos) (port-next-location port)])
          (write-string "goto" port)
          (write-string " " port)
          (write label port)
          (write-string ";" port)
          )
        ]))])

(struct HasType (expr type)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(HasType expr type)
          (recur expr port)
          ])))])

(struct GlobalValue (name)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(GlobalValue name)
        (write-string (symbol->string name) port)
        ]))])
  
(struct Collect (size)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(Collect size)
        (let-values ([(line col pos) (port-next-location port)])
          (write-string "(collect " port)
          (write size port)
          (write-string ");" port)
          )
        ]))])
  
(struct CollectionNeeded? (size) #:transparent)

(struct Allocate (amount type) 
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(Allocate amount type)
        (let-values ([(line col pos) (port-next-location port)])
          (write-string "(allocate " port)
          (write amount port)
          (write-string " " port)
          (write-type type port)
          (write-string ")" port)
          )
        ]))])

(struct AllocateProxy (type) #:transparent)

(struct Call (fun arg*)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(Call fun arg*)
          (write-string "(" port)
          (recur fun port)
          (for ([arg arg*])
            (write-string " " port)
            (recur arg port))
          (write-string ")" port)
          ])))])

(struct TailCall (fun arg*)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(TailCall fun arg*)
          (write-string "(" port)
          (recur fun port)
          (for ([arg arg*])
            (write-string " " port)
            (recur arg port))
          (write-string ")" port)
          ])))])

(struct Imm (value) #:transparent
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(Imm n)
        (write-string "$" port)
        (write n port)]))])

(struct Reg (name)
  #:methods gen:custom-write
  [(define (write-proc reg port mode)
     (match reg
       [(Reg r)
        (write-string "%" port)
        (write r port)]))])

(struct Deref (reg offset)
  #:methods gen:custom-write
  [(define (write-proc dr port mode)
     (match dr
       [(Deref reg offset)
        (write offset port)
        (write-string "(" port)
        (write-string "%" port)
        (write reg port)
        (write-string ")" port)
        ]))])

(struct Instr (name arg*)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(Instr name arg*)
          (let-values ([(line col pos) (port-next-location port)])
            (write name port)
            (let ([i 0])
              (for ([arg arg*])
                (cond [(not (eq? i 0))
                       (write-string "," port)])
                (write-string " " port)
                (recur arg port)
                (set! i (add1 i))
                ))
            (newline-and-indent port col)
            )])))])

(struct Callq (target)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(Callq target)
        (let-values ([(line col pos) (port-next-location port)])
          (write-string "callq" port)
          (write-string " " port)
          (write target port)
          (newline-and-indent port col))
        ]))])

(struct Retq ()
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(Retq)
        (let-values ([(line col pos) (port-next-location port)])
          (write-string "retq" port)
          (newline-and-indent port col))
        ]))])

(struct IndirectCallq (target)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(IndirectCallq target)
          (let-values ([(line col pos) (port-next-location port)])
            (write-string "callq" port)
            (write-string " " port)
            (write-string "*" port)
            (recur target port)
            (newline-and-indent port col))])))])

(struct Jmp (target)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(Jmp target)
        (let-values ([(line col pos) (port-next-location port)])
          (write-string "jmp" port)
          (write-string " " port)
          (write target port)
          (newline-and-indent port col))
        ]))])
  
(struct TailJmp (target)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(TailJmp target)
        (let-values ([(line col pos) (port-next-location port)])
          (write-string "tailjmp" port)
          (write-string " " port)
          (write target port)
          (newline-and-indent port col))
        ]))])

(struct Block (info instr*) #:transparent
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (let ([recur (make-recur port mode)])
       (match ast
         [(Block info instr*)
          (for ([instr instr*])
            (recur instr port))
          ])))])

(struct StackArg (number) #:transparent) ;; no longer needed? -Jeremy

(struct ByteReg (name)
  #:methods gen:custom-write
  [(define (write-proc reg port mode)
     (match reg
       [(ByteReg r)
        (write-string "%" port)
        (write r port)]))])

(struct JmpIf (cnd target)
  #:methods gen:custom-write
  [(define (write-proc ast port mode)
     (match ast
       [(JmpIf cnd target)
        (let-values ([(line col pos) (port-next-location port)])
          (write-string "j" port)
          (write cnd port)
          (write-string " " port)
          (write target port)
          (newline-and-indent port col))
        ]))])
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicates for contracts on structs

(define (any? e) #t)

(define (param-list? ps)
  (or (null? ps)
      (and (param? (car ps))
           (param-list? (cdr ps)))))

(define (param? p)
  (match p
    [`(,x : ,t) (and (symbol? x) (type? t))]
    [else #f]))

(define (exp-list? es)
  (or (null? es)
      (and (exp? (car es))
           (exp-list? (cdr es)))))

(define (exp? e)
  (match e
    [(Var x) #t]
    [(Int n) #t]
    [(Bool b) #t]
    [(Void) #t]
    [(Let x rhs body) #t]
    [(Lambda ps rt body) #t]
    [(Prim op es) #t]
    [(Apply e es) #t]
    [(GlobalValue n) #t]
    [(Allocate n t) #t]
    [(If cnd thn els) #t]
    [(HasType e t) #t]
    [(Collect s) #t] ;; update figure in book? see expose-alloc-exp in vectors.rkt
    [(Apply e e*) #t]
    [(FunRef f) #t]
    [(Call f e*) #t]
    [else #f]))

(define (type? t)
  (match t
    [`(Vector ,ts ...) #t]
    ['Integer #t]
    ['Boolean #t]
    ['Void #t]
    [`(,ts ... -> ,t) #t]
    ['() #t] ;; for when a type is not specified
    ['_ #t]  ;; also for when a type is not specified
    [else #f]))

(define (lhs? v)
  (match v
    [(Var x) #t]
    [(Reg r) #t]
    [else #f]))

(define (stmt? s)
  (match s
    [(Assign x e) #t]
    [(Collect n) #t]
    [else #f]))

(define (tail? t)
  (match t
    [(Return e) #t]
    [(Seq s t) #t]
    [(Goto l) #t]
    [(IfStmt cnd els thn) #t]
    [(TailCall f arg*) #t]
    [else #f]))

(define (goto? s)
  (match s
    [(Goto l) #t]
    [else #f]))

(define (cmp? e)
  (match e
    [(Prim cmp (list arg1 arg2)) #t] ;; should also check cmp -Jeremy
    [else #f]))

(define (arg? arg)
  (match arg
    [(Imm n) #t]
    [(Var x) #t]
    [(Reg r) #t]
    [(Deref r n) #t]
    [(ByteReg r) #t]
    [(? symbol?) #t] ;; for condition code in set instruction
    [(GlobalValue name) #t]
    [(FunRef f) #t]
    [else #f]))

(define (arg-list? es)
  (or (null? es)
      (and (arg? (car es))
           (arg-list? (cdr es)))))

(define (instr? ins)
  (match ins
    [(Instr n arg*) #t]
    [(Callq t) #t]
    [(IndirectCallq a) #t]
    [(Jmp t) #t]
    [(TailJmp t) #t]
    [(JmpIf c t) #t]
    [else #f]
    ))

(define (instr-list? es)
  (or (null? es)
      (and (instr? (car es))
           (instr-list? (cdr es)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing S-expressions into Abstract Syntax Trees (and back)


(define src-primitives
  '(read + - eq? < <= > >= and or not vector vector-ref vector-set!))

(define (parse-exp e)
  (match e
    [(? symbol?) (Var e)]
    [(? fixnum?) (Int e)]
    [(? boolean?) (Bool e)]
    [`(void) (Void)]
    [`(let ([,x ,rhs]) ,body) (Let x (parse-exp rhs) (parse-exp body))]
    [`(if ,cnd ,thn ,els) (If (parse-exp cnd) (parse-exp thn) (parse-exp els))]
    [`(lambda: ,ps : ,rt ,body)
     (Lambda ps rt (parse-exp body))]
    [`(,op ,es ...)
     #:when (set-member? src-primitives op)
     (Prim op (for/list ([e es]) (parse-exp e)))]
    [`(,e ,es ...)
     (Apply (parse-exp e) (for/list ([e es]) (parse-exp e)))]
    ))

(define (parse-def d)
  (match d
    [`(define (,f ,ps ...) : ,rty ,body)
     (Def f ps rty '() (parse-exp body))]
    ))

(define (parse-program p)
  (match p
    [`(program ,info ,body)
     (Program info (parse-exp body))]
    [`(program ,info ,def* ... ,body)
     (ProgramDefs info
                  (for/list ([d def*]) (parse-def d))
                  (parse-exp body))]
    ))

(define (unparse-exp e)
  (match e
    [(Var x) x]
    [(Int n) n]
    [(Bool b) b]
    [(Void) '(void)]
    [(Let x rhs body)
     `(let ([,x ,(unparse-exp rhs)]) ,(unparse-exp body))]
    [(Lambda ps rt body)
     `(lambda: ,ps ,rt ,(unparse-exp body))]
    [(Prim op es)
     `(,op ,@(map unparse-exp es))]
    [(Apply e es)
     `(,(unparse-exp e) ,@(map unparse-exp es))]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (define parsed-prog (parse-program input-prog))
  (debug "utilities/read-program" parsed-prog)
  parsed-prog)

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

(define (strip-has-type e)
  e)

#;(define (strip-has-type e)
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
        (trace "compile-file: output of type check" tsexp)
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

;; (define (interp-tests name typechecker passes initial-interp test-family test-nums)
;;   (define checker (check-passes name typechecker passes initial-interp))
;;   (for ([test-number (in-list test-nums)])
;;     (let ([test-name (format "~a_~a" test-family test-number)])
;;       (debug "utilities/interp-test" test-name)
;;       (checker test-name))))

(define (interp-tests name typechecker passes initial-interp test-family test-nums)
  (run-tests (interp-tests-suite name typechecker passes initial-interp test-family test-nums) (test-verbosity)))

(define (interp-tests-suite name typechecker passes initial-interp test-family test-nums)
  (define checker-suite (check-passes-suite name typechecker passes initial-interp))
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
    (if (string? res)
	(string=? res expected)
	(string=? (number->string res) expected))
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
          (if type-error-expected
              (test-case "typecheck" (check-false typechecks "Expected expression to fail typechecking"))
	      (if (not typechecks) (fail "Expected expression to typecheck")
		  (test-case "code generation"
			     (let ([gcc-output (system (format "gcc -g -std=c99 runtime.o tests/~a.s -o tests/~a.out" test-name test-name))])
			       (if (not gcc-output) (fail "Failed during assembly")
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
				       (check-not-false gcc-output "Unable to run program, gcc reported assembly failure")
				       (check-not-equal? (cadr result) 'timed-out (format "x86 execution timed out after ~a seconds" (caddr result)))
				       (cond [error-expected
					      (check-equal? (cadr result) 'done-error (format "expected error, not: ~a" (caddr result)))
					      (check-equal? (caddr result) 255 (format "expected error, not: ~a" (caddr result)))]
					     [else
					      (check-not-eq? (cadr result) eof "x86 execution did not produce output")
					      (check result-check (caddr result) output "Mismatched output from x86 execution")]))))))))))))))

(define (compiler-tests name typechecker passes test-family test-nums)
  (run-tests (compiler-tests-suite name typechecker passes test-family test-nums) (test-verbosity)))

(define (compiler-tests-gui name typechecker passes test-family test-nums)
  (test/gui (compiler-tests-suite name typechecker passes test-family test-nums)))


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
          ;[(Program info body) res]
          [else res]
          ;[else exp]
          ))))

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
         (Seq (cdr (car bs))
              (make-lets (cdr bs) e))]
        [else
         (Let (car (car bs)) (cdr (car bs))
              (make-lets (cdr bs) e))]))

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

