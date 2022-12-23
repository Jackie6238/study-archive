;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname M7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Module7 - Nats
;; Exercises


;; ex. 1
;; (sum-to n) produces the sum of all Nat between o and n
;; Example:
(check-expect (sum-to 4) 10)

;; sum-to: Nat -> Nat
(define (sum-to n)
  (cond [(zero? n) 0]
        [else (+ n (sum-to (sub1 n)))]))


;; ex. 2
;; (sum-between n b) produces the sum of all Nat between b and n
;; Examples:
(check-expect (sum-between 5 3) 12)

;; sum-between: Nat Nat -> Nat
;;   Requires: n >= b
(define (sum-between n b)
  (cond [(< n b) 0]
        [else (+ n (sum-between (sub1 n) b))]))


;; ex. 3
;; (countdown-by top step) that returns a listof Nat so the first is top,
;; the next is step less, and so on, until the next one would be zero or less
;; Examples:
(check-expect (countdown-by 12 3) (cons 12 (cons 9 (cons 6 (cons 3 empty)))))
(check-expect (countdown-by 11 3) (cons 11 (cons 8 (cons 5 (cons 2 empty)))))

;; countdown-by: Nat Nat -> (listof Nat)
(define (countdown-by top step)
  (cond [(<= top step) (cons top empty)]
        [else (cons top (countdown-by (- top step) step))]))


;; ex. 4
;; (n-th-item lst n) Produce the n-th item in lst, where (first lst) is the 0th 
;; Examples:
(check-expect (n-th-item (cons 3 (cons 7 (cons 31 (cons 63 empty)))) 0) 3)
(check-expect (n-th-item (cons 3 (cons 7 (cons 31 (cons 63 empty)))) 3) 63)

;; n-th-item: (listof Any) Nat -> Any
;;   Requires: n < (length lst)
(define (n-th-item lst n)
  (cond [(zero? n) (first lst)]
        [else (n-th-item (rest lst) (sub1 n))]))