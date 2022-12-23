;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname M9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Module9 - Patterns
;; Exercises

;; ex. 1
(define (fib n)
  (cond [(< n 2) n]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

;; (extend-fib n lst) returns a list containing n more Fibonacci values,
;; including the given values in lst

;; extend-fib: Nat (listof Nat) -> (listof Nat)
;;   Requires: (length lst) >= 2
(define (extend-fib n lst)
  (cond [(zero? n) lst]
        [else (extend-fib (sub1 n) (cons (fib n) lst))]))


;; ex. 2




;; ex. 3




