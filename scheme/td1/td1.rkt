#lang racket

;;;; Mise en place

(define (test cond result)
  (if (equal? cond result)
      (void)
      (error "Bad test result")))

;;;; Échauffement

(define (next-odd n)
  (if (equal? 0 (modulo n 2))
      (+ n 1)
      (+ n 2)))

(define (prime? n)
  (cond 
    ((<= n 1) #f)
    ((<= n 3) #t)
    ((equal? 0 (modulo n 2)) #f)
    (else (let loop ((i 3)
                     (s (sqrt n)))
            (cond
              ((> i s) #t)
              ((equal? 0 (modulo n i)) #f)
              (else (loop (+ i 2) s)))))
    ))

(define (map-interval f min max)
  (let loop ((min min))
    (if (> min max)
        '()
        (cons (f min) (loop (+ min 1))))
    ))

(define iota range)

;;;; Tests Échauffement

(test (+ 1 2) 3)
(test (next-odd 1) 3)
(test (next-odd 2) 3)
(test (prime? -5) #f)
(test (prime? 0) #f)
(test (prime? 1) #f)
(test (prime? 2) #t)
(test (prime? 3) #t)
(test (prime? 19) #t)
(test (prime? 21) #f)
(test (map-interval (lambda (x) (+ 2 x)) 10 13) '(12 13 14 15))
(test (iota 5) '(0 1 2 3 4))
(test (iota 0) '())
(test (iota -1) '())

;;;; Manipulation de données

(define (revsymb symb)
  (string->symbol (list->string (reverse (string->list (symbol->string symb))))))

(define (trans l)
  (map (lambda (x) (revsymb x)) l))

(define (display-all l)
  (for-each (lambda (x) (display x) (newline)) l))

(define (filter test l)
  (if (empty? l)
      '()
      (let ((filtered (filter test (cdr l))))
        (if (test (car l)) 
            (cons (car l) filtered) 
            filtered))))

(define (slash operator l)
  (if (> 2 (length l))
      l ;We should define the return of the binairy operator with less than 2 elements (or throw an error)
      (let loop ((res (operator (car l) (car (cdr l))))
                 (list (cdr l)))
        (if (empty? (cdr list)) ;There's no element left
            res
            (loop (operator res (car (cdr list))) (cdr list))))
      ))

;;;; Test Manipulation de données

(test (revsymb 'foobar) 'raboof)
(test (trans '(foo bar)) '(oof rab))
(test (filter (lambda (x) (> x 3)) '(1 10 2 20)) '(10 20))
(test (slash * '(10 20 30)) 6000)
(test (slash string-append '("foo" "bar")) "foobar")
(test (slash + '(1 2 3)) 6)
(test (slash - '(10 2 3)) 5)
(test (slash expt '(2 3 4)) 4096)
(test (slash * (filter prime? (iota 100))) 2305567963945518424753102147331756070)

;;;; Transformation de code

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or a b ...)
     (let ((result a))
       (if result
           result
           (or b ...))))
    ))

(define-syntax and 
  (syntax-rules ()
    ((and) #t)
    ((and a) a)
    ((and a b ...)
     (if a
         (and b ...)
         #f))
    ))

(define-syntax while
  (syntax-rules ()
    ((while cond body ...) 
     (let loop ()
       (when cond
         (begin body ... (loop)))
       ))))

;;;; Test mise en jambe

(test (let ((i 0) (c 0)) (while (< i 5) (set! c (+ c i)) (set! i (+ i 1))) c) 10)
(test
 (let ((x 1))
   (+ (or (begin (set! x (+ 1 x)) x)) x) 4)
 4)
(test (and 1 2 3) 3)

(define-syntax define-trace
  (syntax-rules ()
    ((define-trace (f . args) body ...)
     (define (f . args)
       (begin (display f) (newline) (begin0 body ... (flush-output) (newline)))
       ; I think that it's not possible to print another time after after returning the value of body
       )
     )))

;; With this tracing, we can have the trace at the end of the procedure, but the procedure won't return the value we expected
(define-syntax define-trace2
  (syntax-rules ()
    ((define-trace2 (f . args) body ...)
     (define (f . args)
       (let ((result body ...))
         (display f) (newline)
         (display result)
         (newline) (display f))
       )
     )))


;; Programmation par contrat
;; pas encore fonctionnel

(define-syntax (contract stx)
  (syntax-case stx ()
    ((_ body ...)
     (let ((preSym (datum->syntax stx 'pre))
           (postSym (datum->syntax stx 'post))
           (invSym (datum->syntax stx 'inv)))
       #`(let ((postconds '()))
           (define (check expr)
             (lambda () 
               (when (not expr)
                 (error "error..."))))
           (define-syntax (#,preSym c)
             (syntax-case c ()
               ((_ cond)
                #`((check cond)))))
           (define-syntax (#,postSym c)
             (syntax-case c ()
               ((_ cond)
                #`(set! postconds (cons (lambda () ((check cond))) postconds)))))
           (define-syntax (#,invSym c)
             (syntax-case c ()
               ((_ cond)
                #`(begin
                    ((check cond))
                    (set! postconds (cons (lambda () ((check cond))) postconds))))))
           (begin0 
             (begin body ...)
             (for-each (lambda (c) (c)) (reverse postconds))))))))

;; quelques tests
(contract (post (printf "post~n")) (pre (printf "pre~n")) (inv (printf "inv~n"))(+ 2 3))
;(contract (pre #f) 1)
;(contract (pre #f) (post #f) 1)


