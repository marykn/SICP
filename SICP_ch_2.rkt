#lang planet neil/sicp

(define x 5)

(* x x)

(define (make-rat n d) (cons n d))

(define one-half (make-rat 1 2))

; Ex. 2.1

; make-rat should normalize the sign so that if the rational number is positive, both the numerator and denom are positive,
; and if the rational number is negative, only the numer is negative.

; (define (neg-make-rat n d) (

; (define neg-one-half (make-rat (-1) 2))


; Ex. 2.2

(define (average a b)
  (/ (+ a b) 2))

(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (start-segment p)
  (car p))

(define (end-segment p)
  (cdr p))

 (define (make-segment-bad x1 y1 x2 y2)
  (define start (make-point x1 y1))
  (define end (make-point x2 y2))
  (cons start end))

(define (make-segment start end)
  (cons start end))

(define (midpoint-segment s)
  (make-point
   (average (x-point (start-segment s)) (x-point (end-segment s)))
   (average (y-point (start-segment s)) (y-point (end-segment s)))))

(define try-segment (make-segment-bad 1 2 4 5))

(define pointa (make-point 1 2))
(define pointb (make-point 4 5))
(define segmenta (make-segment pointa pointb))
  
(print-point (midpoint-segment segmenta))
(print-point (midpoint-segment try-segment))



; Ex. 2.3

(define (abs x)
  (if (>= x 0)
      x
      (* x -1)))

(abs 0)

(define (print-segment segment)
  (newline)
  (print-point (start-segment segment))
  (print-point (end-segment segment)))

(define (print-rectangle rectangle)
  ; side1
  (print-segment (car (car rectangle)))
  ; side2
  (print-segment (get-width-segment rectangle))
  ; side3
  (print-segment (get-length-segment rectangle))
  ; side4
  (print-segment (cdr (cdr rectangle)))
  (newline))
 

; Assumes rectangle sides are parallel to x and y axes and that points are entered clockwise starting with lower left

(define (segments-make-rectangle side1 side2 side3 side4)
  (define width-length1 (cons side1 side2))
  (define width-length2 (cons side3 side4))
  (cons width-length1 width-length2))


; Returns a segment - side3
(define (get-length-segment rectangle)
  (car (cdr rectangle)))

; Returns a segment - side2
(define (get-width-segment rectangle)
  (cdr (car rectangle)))

; Returns a value
(define (get-distance segment)
 (define x-diff (abs (- (x-point (start-segment segment)) (x-point (end-segment segment)))))
 (define y-diff (abs (- (y-point (start-segment segment)) (y-point (end-segment segment)))))
 (if (> x-diff 0)
     x-diff
     y-diff
     ))

; Returns a pair of values that are the length and width distances
(define (get-dimensions rectangle)
  (cons (get-distance (get-length-segment rectangle))
  (get-distance (get-width-segment rectangle))))

; Returns the value of the length
(define (get-length rectangle)
  (car (get-dimensions rectangle)))

; Returns the value of the width
(define (get-width rectangle)
  (cdr (get-dimensions rectangle)))

(define (segments-perimeter rectangle)
  (* 2 (+ (get-length rectangle) (get-width rectangle))))

(define (segments-area rectangle)
  (* (get-length rectangle) (get-width rectangle)))

(define point1 (make-point 1 1))
(define point2 (make-point 1 5))
(define point3 (make-point 6 5))
(define point4 (make-point 6 1))
(define segment1 (make-segment point1 point2))
(define segment2 (make-segment point2 point3))
(define segment3 (make-segment point3 point4))
(define segment4 (make-segment point4 point1))

(define rectangle1 (segments-make-rectangle segment1 segment2 segment3 segment4))

(print-rectangle rectangle1)

(segments-perimeter rectangle1)
(segments-area rectangle1)









(define (points-make-rectangle point1 point2 point3 point4)
  (define half-rect1 (cons (make-segment point1 point2) (make-segment point2 point3)))
  (define half-rect2 (cons (make-segment point3 point4) (make-segment point4 point1)))
  ; Next iteration: come up with a way to make sure x points and y points match so you know you're calculating segments correctly
  (cons half-rect1 half-rect2))

(define rectangle2 (points-make-rectangle point1 point2 point3 point4))

(segments-perimeter rectangle1)
(segments-area rectangle1)
  
(print-rectangle rectangle2)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))







; Exercise 2.1.7
; Define a procedure last-pair that returns the list that contains only the last element of a given non-empty list.

(define (last-pair list)
  (cond ((null? list) "List is empty")
        ((= (length list) 1) list)
        ((last-pair (cdr list)))))

(define basic-list (list 1 2 3 4))
(define short-list (list 3))
(define empty-list (list))

(length short-list)
(length basic-list)
(length empty-list)


(cdr short-list)
(last-pair basic-list)
(cdr basic-list)
(last-pair short-list)
(last-pair empty-list)

basic-list

;(define (append list1 list2)
;  (if (null? list1)
;      list2
;      (cons (car list1) (append (cdr list1) list2))))

(append basic-list short-list)

(define (reverse-list list)
  (if (null? list) 
      nil
      (cons (reverse-list (cdr list)) (car list))))

(car short-list)
(reverse-list basic-list)
(reverse-list empty-list)