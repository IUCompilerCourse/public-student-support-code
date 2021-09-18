#lang racket
(require graph)
(require "utilities.rkt")

(provide print-dot print-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph Printing

(define (print-dot graph file-name)
  (if (at-debug-level? 1)
      (call-with-output-file file-name #:exists 'replace
	(lambda (out-file)
	  (write-string "strict graph {" out-file) (newline out-file)
	  
	  (for ([v (in-vertices graph)])
	       (write-string (format "~a;\n" v) out-file))
	  
	  (for ([u (in-vertices graph)])
	       (for ([v (in-neighbors graph u)])
		    (write-string (format "~a -- ~a;\n" u v) out-file)))
	  
	  (write-string "}" out-file)
	  (newline out-file)))
      '()))

(define (print-graph graph)
  (for ([u (in-vertices graph)])
    (for ([v (in-neighbors graph u)])
      (write-string (format "~a -> ~a;\n" u v))
      )))

