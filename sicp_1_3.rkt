#lang planet neil/sicp

(define (average x y)
  (/ (+ x y) 2))

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Ex. 1.29

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)

(define (inc n) (+ n 1))

(define (even? n)
  (= (remainder n 2) 0))

; this is totally wrong mathematically
(define (integral-mary f a b n)
  (define (h a b n)
    (/ (- b a) n))
  ; with a b n = 0, 1, 0.1, h = 10
  (define k (- 0 1))
  (define (k-iter k)
    (inc k))
  (define (coefficient k)
    (cond ((= 0 k) 1)
          ((even? k) 2)
          (else 4)))
  (define (y a b n) 
    (if (= (k-iter k) 0)
        (f a)
        (f (+ a (* (k-iter k) (h b a n))))))
  (* (/ (h b a n) 3) (sum f (* (coefficient (k-iter k)) (y a b n)) inc b)))

(integral-mary cube 0 1 0.1)




; Exercise 1.30

;(define (sum-iter term a next b)
;  (define (iter a result)
;    (if 


; 1.3.3

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

         
; Exercise 1.36

(define (fixed-point-ex f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)
(fixed-point-ex (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point-ex (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)


; Exercise 1.37a

;(define (cont-frac n d k)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2) tolerance)))
;  (define (frac-iter n d k)
;    (define (guess k))
;    (/ n (+ d guess)))
;  (define (try guess)
;    (let ((next (frac-iter n d (+ 1 k)))))
;    (newline)
;    (display next)
;    (if (close-enough? guess next)
;        next
;        (try next)))
;  (try n))
  
;(cont-frac (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           k)