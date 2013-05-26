#lang racket

;;;;;;;;;;;;;;;;;;;
;; mise en place ;;
;;;;;;;;;;;;;;;;;;;

(define test
  (lambda (cond res)
    (if (equal? cond res)
        (void)
        (error "Bad test result"))))

;;;;;;;;;;;;;;;;;;
;; échauffement ;;
;;;;;;;;;;;;;;;;;;

(define next-odd
  (lambda (n)
    (if (equal? 0 (modulo n 2))
        (+ n 1)
        (+ n 2))))

(define prime?
  (lambda (n)
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
      )))

(define map-interval
  (lambda (f min max)
    (let loop ((min min))
      (if (> min max)
          '()
          (cons (f min) (loop (+ min 1))))
      )))

(define iota range)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests : échauffement ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MANIPULATION DE DONNEES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define revsymb
  (lambda (symb)
    (string->symbol (list->string (reverse (string->list (symbol->string symb)))))))

(define trans
  (lambda (x) (map revsymb x)))

(define display-all
  (lambda (l)
    (for-each (lambda (x) (display x) (newline)) l)))

(define filter
  (lambda (test l)
    (if (empty? l)
        '()
        (let ((filtered (filter test (cdr l))))
          (if (test (car l)) 
              (cons (car l) filtered) 
              filtered)))))

(define slash
  (lambda (operator l)
    (if (> 2 (length l))
        l
        (let loop ((res (operator (car l) (car (cdr l))))
                   (list (cdr l)))
          (if (empty? (cdr list))
              res
              (loop (operator res (car (cdr list))) (cdr list))))
        )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests : manipulation de données ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (revsymb 'foobar) 'raboof)
(test (trans '(foo bar)) '(oof rab))
(test (filter (lambda (x) (> x 3)) '(1 10 2 20)) '(10 20))
(test (slash * '(10 20 30)) 6000)
(test (slash string-append '("foo" "bar")) "foobar")
(test (slash + '(1 2 3)) 6)
(test (slash - '(10 2 3)) 5)
(test (slash expt '(2 3 4)) 4096)
(test (slash * (filter prime? (iota 100))) 2305567963945518424753102147331756070)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRANSFORMATION DE CODE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;
;; mise en jambe ;;
;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests : mise en jambe ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (let ((i 0) (c 0)) (while (< i 5) (set! c (+ c i)) (set! i (+ i 1))) c) 10)
(test
 (let ((x 1))
   (+ (or (begin (set! x (+ 1 x)) x)) x) 4)
 4)
(test (and 1 2 3) 3)

;;;;;;;;;;;
;; trace ;;
;;;;;;;;;;;

(define-syntax define-trace
  (syntax-rules ()
    ((define-trace (f . args) body ...)
     (define (f . args)
       (begin
         (display f)
         (newline)
         (begin0
           body ... 
           (flush-output) 
           (newline)))
       )
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programmation par contrat ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; pas encore fonctionnel
; probleme d'évalutation

(define-syntax (contract stx)
  (syntax-case stx ()
    ((_ body ...)
     (let ((preSym (datum->syntax stx 'pre))
           (postSym (datum->syntax stx 'post))
           (invSym (datum->syntax stx 'inv)))
       #`(let ((postconds '()))
           (define (check expr)
             (lambda ()
               (when (not (eval expr))
                 (error "error..."))))
           (define-syntax (#,preSym c)
             (syntax-case c ()
               ((_ cond)
                #`((check #'cond)))))
           (define-syntax (#,postSym c)
             (syntax-case c ()
               ((_ cond)
                #`(set! postconds (cons (lambda () (check #'cond)) postconds)))))
           (define-syntax (#,invSym c)
             (syntax-case c ()
               ((_ cond)
                #`(begin
                    ((check #'cond))
                    (set! postconds (cons (lambda () (check #'cond)) postconds))))))
           (begin0 
             (begin body ...)
             (for-each (lambda (c) ((c))) (reverse postconds))))))))

;; quelques tests
(contract (post (printf "post~n")) (pre (printf "pre~n")) (inv (printf "inv~n"))(+ 2 3))
(contract (pre (printf "pre~n")) (post (printf "post~n")) 1)

;; pour une liste de champs je definis les getters et setters
;; on commence en mettant les champs null par défault
(define-syntax (define-data stx)
  (syntax-case stx ()
    ((_ name field1 ...)
     (let ((constructor (datum->syntax stx (string->symbol (string-append "<" (symbol->string (syntax->datum #'name)) ">")))))
       #`(define #,constructor
           (lambda () "foo"))))))

(define-data joueur age)
(<joueur>)

;; fonction qui doit être mappée à l'ensemble des champs
(define mapper
  (lambda (field)
    (let ((get (datum->syntax field (string->symbol (string-append (symbol->string (syntax->datum #'field)) ">>"))))
          (set (datum->syntax field (string->symbol (string-append ">>" (symbol->string (syntax->datum #'field))))))
          (value null))
      #`(begin
          (define-syntax #,get
            (syntax-rules () (( _ ) #,value)))
          (define-syntax #,set
            (syntax-rules () (( _ v ) (set! #,value v))))))))
;(mapper toto)