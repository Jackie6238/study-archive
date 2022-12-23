;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname m15) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ex 2
;; squash-range (listof Num) -> (listof Num)
;;   Requires: all numbers in lon are between 0 and 225, inclusive
(define (squash-range lon)
  (map (lambda (n) (/ n 255)) lon))
(check-expect (squash-range '(0 204 255)) '(0 0.8 1))


;; ex 3
;; greet-each: (listof Str) -> (listof Str)
(define (greet-each los)
  (map (lambda (s) (string-append "Hi " s "!")) los))

(check-expect (greet-each '("Ali" "Carlos" "Sai")) '("Hi Ali!" "Hi Carlos!" "Hi Sai!"))

;; ex 4
;; neg-odd: (listof Int) -> (listof Int)
(check-expect (neg-odd '(2 5 8 11 14 17)) '(2 -5 8 -11 14 -17))

(define (neg-odd lon)
  (map (lambda (n) (cond [(odd? n) (- n)]
                         [else n])) lon))


;; ex 5
(check-expect (count-odd '(1 2 3)) 2)

;; count-odd: (listof Nat) -> Nat
(define (count-odd lon)
  (foldr (lambda (n rror)
           (cond [(odd? n) (add1 rror)]
                 [else rror]))
         0 lon))

;; ex 6
(check-expect (prod '(2 2 3 5)) 60)

;; prod: (listof Num) -> Num
(define (prod lon)
  (foldr * 1 lon))

;; ex 7
;; using length
(check-expect (total-length-v1 '((1 2 3) (4 5) (1 1 1))) 8)

;; total-length-v1: (listof (listof Any)) -> Nat
(define (total-length-v1 lst)
  (foldr (lambda (x rror) (+ (length x) rror)) 0 lst))

;; using lambda
(check-expect (total-length-v2 '((1 2 3) (4 5) (1 1 1))) 8)

(define (total-length-v2 lst)
  (foldr (lambda (x rror) (+ (foldr (lambda (y rror) (add1 rror)) 0 x) rror)) 0 lst))

;; ex 8
;; using length
(check-expect (average-v1 '(2 4 9)) 5)
(check-expect (average-v1 '(4 5 6 6)) 5.25)

;; average-v1: (listof Num) -> Num
(define (average-v1 lon)
  (foldr (lambda (x rror) (+ (/ x (length lon)) rror)) 0 lon))

;; using lambda
(check-expect (average-v2 '(2 4 9)) 5)
(check-expect (average-v2 '(4 5 6 6)) 5.25)

(define (average-v2 lon)
  (foldr (lambda (x rror) (+ (/ x (foldr (lambda (y rror) (add1 rror)) 0 lon)) rror)) 0 lon))
;; (foldr (lambda (y rror) (add1 rror)) 0 lon) = (length lon)


;; ex 9
(check-expect (times-square '(1 25 5 4 1 17)) 100)
;; Since (times-square '(1 25 5 4 1 17)) ⇒ ( * 1 25 4 1) => 100

;; times-square: (listof Nat) -> Nat
(define (times-square lon)
  (foldr * 1 (filter (lambda (n) (integer? (sqrt n))) lon)))


;; ex 10
(check-expect (double-each '(1 2 3 4)) '(2 4 6 8))
;; double: Num -> Num
(define (double n) ( * n 2))
;; double-each: (listof Num) -> (listof Num)
(define (double-each lst)
  (foldr (lambda (n rror) (cons (double n) rror)) empty lst))

;; ex 11
(check-expect (keep-evens '(1 2 3 4 5 6)) '(2 4 6))
;; keep-evens: (listof Int) -> (listof Int)
(define (keep-evens lst)
  (foldr (lambda (n rror) (cond [(even? n) (cons n rror)]
                                [else rror])) empty lst))

;; ex 12
;; Use foldr, filter, and even?.
(check-expect (sum-evens-v1 '(1 2 4 6)) 12)
(define (sum-evens-v1 loi)
  (foldr + 0 (filter even? loi)))

;; Use foldr and filter, but use a lambda expression instead of even?.
(check-expect (sum-evens-v2 '(1 2 4 6)) 12)
(define (sum-evens-v2 loi)
  (foldr + 0 (filter (lambda (i) (zero? (remainder i 2))) loi)))

;; Use foldr, lambda, and even? but not filter.
(check-expect (sum-evens-v3 '(1 2 4 6)) 12)
(define (sum-evens-v3 loi)
  (foldr (lambda (i rror) (cond [(even? i) (+ i rror)]
                                [else rror])) 0 loi))


;; ex 13
(check-expect (multiply-each (list 2 3 5) 4) (list 8 12 20))
(define (multiply-each lst n)
  (map (lambda (x) (* x n)) lst))

;; ex 14
(check-expect (add-total (list 2 3 5 10)) (list 22 23 25 30))
(define (add-total lst)
  (map (lambda (x) (+ x (foldr (lambda (y rror) (+ y rror)) 0 lst)))
       lst))


;; ex 15
(check-expect (discard-bad '(12 5 20 2 10 22) 10 20) '(12 20 10))
(define (discard-bad lst lo hi)
  (filter (lambda (x) (or (<= lo x hi) (<= hi x lo))) lst))


;; ex 16

(check-expect (squash-bad 10 20 '(12 5 20 2 10 22)) '(12 10 20 10 10 20))
(define (squash-bad lo hi lst)
  (map (lambda (n) (cond [(< n lo) lo]
                         [(> n hi) hi]
                         [else n]))lst))



;; ex 17
(check-expect (above-average '(1 2 1)) '(2))
(define (above-average lon)
  (filter (lambda (n) (> n (average-v1 lon))) lon))


;; ex 19
(check-expect (triangles 4) (list 0 1 3 6))
(define (triangles n)
  (build-list n (lambda (n) (/ (* n (+ n 1)) 2))))











