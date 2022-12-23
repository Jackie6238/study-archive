;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname m14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ex 1
;; (keep-multiples3 lon) keeps all multiples of 3
;; Examples:
(check-expect (keep-multiples3 empty) empty)
(check-expect (keep-multiples3 (list 1 3 6 9)) (list 3 6 9))

;; keep-multiples3: (listof Num) -> (listof Num)
(define (keep-multiples3 lon)
  (local [;; (multiple3? n) determines if n is a multiple of 3
          ;; multiple3?: Num -> Bool
          (define (multiple3? n) (zero? (remainder n 3)))]
    
    (filter multiple3? lon)))


;; ex 2
;; (keep-multiples23 lon) keeps all multiples of 2 and 3
;; Examples:
(check-expect (keep-multiples23 empty) empty)
(check-expect (keep-multiples23 (list 1 2 3 6 9)) (list 2 3 6 9))

;; keep-multiples23: (listof Num) -> (listof Num)
(define (keep-multiples23 lon)
  (local [;; (multiple3? n) determines if n is a multiple of 2 or 3
          ;; multiple3?: Num -> Bool
          (define (multiple2/3? n) (or (zero? (remainder n 3))
                                       (even? n)))]
    
    (filter multiple2/3? lon)))


;; ex 3
;; (keep-inrange lon) keeps values between 10 and 30, inclusive
;; Examples:
(check-expect (keep-inrange empty) empty)
(check-expect (keep-inrange (list -5 10.1 12 7 30 3 19 6.5 42)) (list 10.1 12 30 19))

;; keep-inrange: (listof Num) -> (listof Num)
(define (keep-inrange lon)
  (local [;; (inrange? n) determines if n is between 10 and 30
          ;; inrange?: Num -> Bool
          (define (inrange? n) (<= 10 n 30))]

    (filter inrange? lon)))


;; ex 4
;; (keep-short lst) Keep all the values in lst of length at most 6.
;; Examples:
(check-expect (keep-short empty) empty)
(check-expect (keep-short (list "Strive" "not" "to" "be" "a" "success"
                                "but" "rather" "to" "be" "of" "value"))
              (list "Strive" "not" "to" "be" "a" "but" "rather" "to" "be" "of" "value"))

;; keep-short: (listof Str) -> (listof Str)
(define (keep-short lst)
  (local [(define (l<=6? s) (<= (string-length s) 6))]

    (filter l<=6? lst)))

;; ex5
;; (sum-odds-or-evens lst) returns the sum of the evens if there are more evens than odds,
;; otherwise, it returns the sum of the odds
;; Examples:
(check-expect (sum-odds-or-evens empty) 0)
(check-expect (sum-odds-or-evens (list 1 2 3 4 5)) 9)
(check-expect (sum-odds-or-evens (list 1 2 4)) 6)

;; sum-odds-or-evens: (listof Int) -> Int
(define (sum-odds-or-evens lst)
  (local [(define odds (filter odd? lst))
          (define evens (filter even? lst))
          (define (sum-lst lst)
            (cond [(empty? lst) 0]
                  [else (+ (first lst) (sum-lst (rest lst)))]))]
    
    (cond [(> (length odds) (length evens))
           (sum-lst odds)]
          [else (sum-lst evens)])))

;; ex 6
;; (make-divisible? n) produces a predicate function. The predicate function consumes a Int,
;; returns true if its argument is divisible by n, and false otherwise.
;; Examples:
(check-expect (filter (make-divisible? 2) (list 0 1 2 3 4 5 6 7 8 9))
              (list 0 2 4 6 8))
(check-expect (filter (make-divisible? 3) (list 0 1 2 3 4 5 6 7 8 9))
              (list 0 3 6 9))
(check-expect (filter (make-divisible? 4) (list 0 1 2 3 4 5 6 7 8 9))
              (list 0 4 8))

;; make-divisible?: Int -> (Int -> Bool)
(define (make-divisible? n)
  (local [(define (divisible? m) (zero? (remainder m n)))]

    divisible?))




;; ex 8
;; (isort pred? lst) sorts the elements of lst so that adjacent elements satisfy pred?
;; Examples:
(check-expect (isort < (list 3 4 2 5 1)) (list 1 2 3 4 5))
(check-expect (isort > (list 3 4 2 5 1)) (list 5 4 3 2 1))
(check-expect (isort string<? (list "can" "ban" "fan")) (list "ban" "can" "fan"))

;; isort: (X X -> Bool) (listof X) -> (listof X)
(define (isort pred? lst)
  (local [(define (insert n lst)
            (cond [(empty? lst) (list n)]
                  [(pred? n (first lst)) (cons n lst)]
                  [else (cons (first lst) (insert n (rest lst)))]))]

  (cond [(empty? lst) lst]
        [else (insert (first lst) (isort pred? (rest lst)))])))


;; ex 9
;; (create-checker f answers)
;; Examples:

;; create-checker: (Str -> Num) (listof Num) -> (Str -> Bool)
;; (f Str) -> Num in answers -> true


;; ex 10
(define-struct gnode (key children))
;; A GT (Generalized Tree) is a (make-gnode Nat (listof GT))

;; (tested-gt-sum pred? t) produces the sum of all keys in the GT for which
;; <pred?> produces true
;; Examples:
(check-expect (tested-gt-sum even? (make-gnode 2 (list (make-gnode 3 empty)))) 2)

;; tested-gt-sum: (Nat -> Bool) GT -> Nat
(define (tested-gt-sum pred? t)
  (local [(define (tested-gt-sum/lst lst)
            (cond [(empty? lst) 0]
                  [else (+ (tested-gt-sum pred? (first lst))
                           (tested-gt-sum/lst (rest lst)))]))]
  
  (cond [(pred? (gnode-key t)) (+ (gnode-key t) (tested-gt-sum/lst (gnode-children t)))]
        [else (tested-gt-sum/lst (gnode-children t))])))
