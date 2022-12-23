;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname M8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Module8 - Lists2
;; Exercises


;; ex. 1 omitted
;; ex. 2 omitted
;; ex. 3 omitted
;; ex. 4 omitted
;; ex. 5
;; (add-al assoc alst) adds assoc to alst. If alst already contains assoc's
;; key, replace the value. Otherwise, add assoc at the end of alst.
;; Examples:
(check-expect (add-al (list 8 "Asha") empty) (list (list 8 "Asha")))
(check-expect ; alst does not contain this key.
 (add-al (list 7 "Bo") (list (list 8 "Asha") (list 2 "Joseph") (list 5 "Sami")))
 (list (list 8 "Asha") (list 2 "Joseph") (list 5 "Sami") (list 7 "Bo")))

;; add-al: (list Nat Str) AL -> AL
(define (add-al assoc alst)
  (cond [(empty? alst) (cons assoc empty)]
        [(= (key (first alst)) (key assoc))
         (cons assoc (rest alst))]
        [else (cons (first alst) (add-al assoc (rest alst)))]))

(define (key al) (first al))
(define (val al) (second al))


;; ex. 6
;; (remove-al key alst) removes the association of key from alst.
;; Examples:
(check-expect (remove-al 8 (list (list 8 "Asha"))) empty)

(define (remove-al key alst)
  (cond [(empty? alst) empty]
        [(= (first (first alst)) key) (rest alst)]
        [else (remove-al key (rest alst))]))


;; ex. 7 omitted
;; ex. 8
;; (expand-each lst1 lst2) expands each item in lst1 by creating a new list containing that item
;; and all the items in lst2
;; Examples:
(check-expect (expand-each (list 12 13 'x) (list 42 "zorkmids" 'Q))
              (list (list 12 42 "zorkmids" 'Q)
                    (list 13 42 "zorkmids" 'Q)
                    (list 'x 42 "zorkmids" 'Q)))

;; expand-each: (listof Any) (listof Any) -> (listof (listof Any))
(define (expand-each lst1 lst2)
  (cond [(empty? lst1) empty]
        [else (cons (cons (first lst1) lst2)
                    (expand-each (rest lst1) lst2))]))


;; ex. 9
;; (vector-add v1 v2) adds v1 and v2 together
;; Examples:
(check-expect (vector-add (list 3 5) (list 7 11)) (list 10 16))
(check-expect (vector-add (list 3 5 1 3) (list 2 2 9 3)) (list 5 7 10 6))

;; vector-add: (listof Nat) (listof Nat) -> (listof Nat)
;;   Requires: (length v1) = (length v2)
(define (vector-add v1 v2)
  (cond [(empty? v1) empty]
        [else (cons (+ (first v1) (first v2))
                    (vector-add (rest v1) (rest v2)))]))


;; ex. 10
(define gnames (list "Joseph" "Burt" "Douglas" "James" "David"))
(define snames (list "Hagey" "Matthews" "Wright" "Downey" "Johnston"))

;; (join-names g s) Make a list of full names from g (given names) and s (surnames)
;; Examples:
(check-expect (join-names gnames snames)
              (list "Joseph Hagey" "Burt Matthews" "Douglas Wright"
                    "James Downey" "David Johnston"))

;; join-names: (listof Str) (listof Str) -> (listof Str)
;;   Requires: (length g) = (length s)
(define (join-names g s)
  (cond [(empty? g) empty]
        [else (cons (string-append (first g) " " (first s))
                    (join-names (rest g) (rest s)))]))


;; ex. 11 omitted
;; ex. 12
(define (list=? lst1 lst2)
  (or (and (empty? lst1) (empty? lst2))
      (and (and (not (empty? lst1)) (not (empty? lst2)))
           (= (first lst1) (first lst2))
           (list=? (rest lst1) (rest lst2)))))


;; ex. 13
;; an Atom is (anyof Num Str Bool Sym)
;; (atom=? a1 a2) determines if a1 and a2 are equal
;; Examples:
(check-expect (atom=? 2 'a) false)
(check-expect (atom=? 2 2) true)

;; atom=?: Atom Atom -> Bool
(define (atom=? a1 a2)
  (cond [(and (number? a1) (number? a2)) (= a1 a2)]
        [(and (string? a1) (string? a2)) (string=? a1 a2)]
        [(and (boolean? a1) (boolean? a2)) (boolean=? a1 a2)]
        [(and (symbol? a1) (symbol? a2)) (symbol=? a1 a2)]
        [else false]))


;; (atom=?/list loa1 loa2) determines if loa1 and loa2 are equal
(define (atom=?/list loa1 loa2)
  (cond [(and (empty? loa1) (empty? loa2)) true]
        [(or (empty? loa1) (empty? loa2)) false]
        [else (and (atom=? (first loa1) (first loa2))
                   (atom=?/list (rest loa1) (rest loa2)))]))

;; ex. 14 omitted