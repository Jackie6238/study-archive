;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname M4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Module4 - Simple Data
;; Exercises

;; ex. 1 omitted
;; ex. 2 omitted
;; ex. 3 omitted


;; ex. 4
;; boundaries: Num -> (anyof 1 -1 0)
(define (boundaries x)
  (cond [(and (< 80 x) (<= x 100)) 1]
        [(and (< 0 x) (<= x 80)) -1]
        [else 0]))

;; Tests:
(check-expect (boundaries -2) 0)
(check-expect (boundaries 0) 0)
(check-expect (boundaries 40) -1)
(check-expect (boundaries 80) -1)
(check-expect (boundaries 90.5) 1)
(check-expect (boundaries 100) 1)
(check-expect (boundaries 1000) 0)


;; ex. 5
(define (g i)
  (cond [(and (even? i) (<= 10 i 40)) "baz"]
        [(and (odd? i) (<= -20 i 20)) "qux"]
        [(or (< i -100) (> i 200)) "xyzzy"]
        [else "corge"]))


;; ex. 6 omitted
;; ex. 7 omitted
;; ex. 8 omitted
;; ex. 9 omitted

;; ex. 10

;; (flatten-me x) Say which interval x is in

;; flatten-me: Nat -> Nat
(define (flatten-me x)
  (cond [(< x 25) 1]
        [(< x 50) 2]
        [(< x 75) 3]
        [else 4]))

;; ex. 11
;; (tax-payable income) produces the taxes to be paid on income
;; Examples:
(check-expect (tax-payable 100) 10)
(check-expect (tax-payable 50000) 5500)

;; tax-payable: Num -> Num
;; Requires: income >= 0
(define (tax-payable income)
  (cond [(<= income  45000) (* income 0.1)]
        [(<= income 90000) (+ 4500 (* (- income 45000) 0.2))]
        [else (+ 4500 9000 (* (- income 90000) 0.3))]))

;; Tests:
(check-expect (tax-payable 45000) 4500)
(check-expect (tax-payable 90000) 13500)
(check-expect (tax-payable 100000) 16500)

;; ex. 12 omitted