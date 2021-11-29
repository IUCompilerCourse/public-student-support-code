#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "interp-Lwhile.rkt")
(provide interp-Lvecof interp-Lvecof-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp-Lvecof-class
  (class interp-Lwhile-class
    (super-new)

    (define/override (interp-op op)
      (verbose "Lvecof/interp-op" op)
      (match op
        ['make-vector make-vector]
        ['vectorof-length vector-length]
        ['any-vectorof-length (lambda (v)
                              (match v [(Tagged v^ tg) (vector-length v^)]))]
        ['vectorof-ref vector-ref]
        ['vectorof-set! vector-set!]
        ['* fx*]
        [else (super interp-op op)]))
    ))

(define (interp-Lvecof p)
  (send (new interp-Lvecof-class) interp-program p))
