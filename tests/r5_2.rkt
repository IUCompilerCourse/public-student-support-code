(define (doubleid [x : Integer]) : Integer
  ((lambda: ([x : Integer]) : Integer x) x))
(doubleid 42)
