(let ([x (vector 42 42 42)])
  (if (read) ((lambda (x) (+ (- (vector-ref x 2)) 84)) x)
    (if (and (eq? #t #f) (eq? (vector-ref x 0) (vector-ref x 1)))
	(vector-ref x 0) (void))))
