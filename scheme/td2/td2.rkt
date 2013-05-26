#lang racket

(define test
  (lambda (cond res)
    (if (equal? cond res)
        (void)
        (error "Bad test result"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implémentation de l’opérateur angélique ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Récupération de l’ensemble des solutions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bag-of f)
  (let ((results '()))
    (if (amb #t #f)
        (begin
          (set! results (cons (f) results))
          (fail))
        (reverse results))))

;; tests

(test (bag-of mult) '((3 10) (5 6) (6 5) (10 3)))

(reset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problème des huits reines ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check column row placed)
  (for-each (lambda (coord)
              (let ((c (car coord))
                    (r (cdr coord)))
                (if (equal? c column) (amb) (void))
                (if (equal? r row) (amb) (void))
                (if (or (equal? (+ r c) (+ row column)) (equal? (- r c) (- row column))) (amb) (void))))
              placed))

(define (queens)
  (let loop ((placed '())
             (column 1))
    (if (equal? (length placed) 8)
        placed 
        (letrec ((row (amb 1 2 3 4 5 6 7 8))
                 (coord (cons column row)))
          (check column row placed)
          (loop (cons coord placed) (+ column 1))))))

;; tests
(test (length (bag-of queens)) 92)

;;;;;;;;;;;;;;;;
;; Coroutines ;;
;;;;;;;;;;;;;;;;