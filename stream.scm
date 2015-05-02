(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons x y)
     (cons x (delay y)))))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define stream-null? null?)

(define (stream-seq n)
  (stream-cons n (stream-seq (+ n 1))))

(define (stream-ref s n)
  (if (stream-null? s) '()
    (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1)))))

(define (stream-map fn s)
  (if (stream-null? s) '()
    (stream-cons (fn (stream-car s))
                 (stream-map fn (stream-cdr s)))))

(define (stream-foreach fn s)
  (if (stream-null? s) '()
    (begin
      (fn (stream-car s))
      (stream-foreach fn (stream-cdr s)))))

(define (stream-filter fn s)
  (if (stream-null? s) '()
    (if (fn (stream-car s))
      (stream-cons (stream-car s) (stream-filter fn (stream-cdr s)))
      (stream-filter fn (stream-cdr s)))))

(define (stream-take s n)
  (if (or (stream-null? s) (= n 0)) '()
    (cons (stream-car s)
          (stream-take (stream-cdr s) (- n 1)))))

(define (stream-fold fn init s)
  (let iter ((result init)
             (s s))
    (if (stream-null? s) result
      (let ((i (stream-car s)))
        (stream-cons result (iter (+ result i) (stream-cdr s)))))))

(define (fibgen a b)
  (stream-cons a (fibgen b (+ a b))))

(define fibs (fibgen 1 1))

(define (sieve s)
  (stream-cons (stream-car s)
               (sieve (stream-filter
                        (lambda (x)
                          (not (= (modulo x (stream-car s)) 0)))
                        (stream-cdr s)))))

(define primes (sieve (stream-seq 2)))

(define (pi-summand n)
  (stream-cons (/ 1.0 n)
               (stream-map - (pi-summand (+ n 2)))))

(define pi-stream
  (stream-map (lambda (x) (* 4 x))
              (stream-fold + 0 (pi-summand 1))))

