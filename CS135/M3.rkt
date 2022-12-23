;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname M3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Module3 - Design Recipe
;; Exercises

;; ex. 1
;; (abs n) produces the absolute value of n
;; Examples:
(check-expect (abs -3) 3)
(check-expect (abs 0) 0)
;; abs: Num -> Num

;; Tests:
(check-expect (abs 2.5) 2.5)
(check-expect (abs -1/2) 1/2)


;; ex. 2
;; (sqrt-shift x c) produce the square root of (x - c)
;; Examples:
(check-expect (sqrt-shift 7 3) 2)
(check-expect (sqrt-shift 125 4) 11)

;; sqrt-shift: Num Num -> Num
;; Requires: x >= c
(define (sqrt-shift x c)
  (sqrt (- x c)))