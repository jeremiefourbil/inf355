#lang racket

(define fail #f)

(define (reset)
  (set! fail (lambda () (error "no more alternatives"))))

(reset)

(define-syntax amb
  (syntax-rules ()
    ((_) (fail))
    ((_ x) x)
    ((_ x y )
     (let/cc k
       (let ((old-fail fail))
         (set! fail (lambda ()
                      (set! fail old-fail)
 
                      (k (begin y)))))
       x))
    ((_ x y ...)
      (amb x (amb y ...)))))

(define (procedure n)
  (printf "Producing ~s~n" n)
  n)

(define (mult)
  (let ((x (amb 1 2 3 4 5 6 7 8 9 10))
        (y (amb 1 2 3 4 5 6 7 8 9 10)))
    (if (= (* x y) 30) (list x y) (amb))))

(reset)

(define (bag-of f)
  (let ((results '()))
    (if (amb #t #f)
    (begin
      (set! results (cons (f) results))
      (fail))
    (reverse results))))

  