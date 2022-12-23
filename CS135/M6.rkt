;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname M6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Module6 - Lists
;; Exercises


;; ex. 1
(define slst (cons "milk" (cons "eggs" (cons "bread" (cons "PB" empty)))))

;; produce the string "bread"
(first (rest (rest slst)))

;; produce the list (cons "bread" (cons "PB" empty))
(rest (rest slst))

;; produce the empty list (starting with slst)
(rest (rest (rest (rest slst))))


;; ex. 2
;; same-consec?: (listof Str) -> Bool
(define (same-consec-cond? loc)
  (cond [(or (empty? loc) (empty? (rest loc))) false]
        [(string=? (first loc) (first (rest loc))) true]
        [else false]))


;; ex. 3
;; (remove-second lst) produces lst with its second item removed
;; Examples:
(check-expect (remove-second (cons 'Mercury (cons 'Venus empty)))
              (cons 'Mercury empty))
(check-expect (remove-second (cons 2 (cons 4 (cons 6 (cons 0 (cons 1 empty))))))
              (cons 2 (cons 6 (cons 0 (cons 1 empty)))))

(define (remove-second lst)
  (cons (first lst) (rest (rest lst))))


;; ex. 4
;; (sum loi) produces the sum of all integers in lst
;; Examples:
(check-expect (sum (cons 6 (cons 7 (cons 42 empty)))) 55)

;; sum: (listof Int) -> Int
(define (sum loi)
  (cond [(empty? loi) 0]
        [else (+ (first loi) (sum (rest loi)))]))


;; ex. 5
;; (keep-evens loi) produces loi with all odd integers removed
(check-expect (keep-evens (cons 4 (cons 5 (cons 8 (cons 10 (cons 11 empty))))))
              (cons 4 (cons 8 (cons 10 empty))))
(check-expect (keep-evens (cons 5 empty)) empty)
(check-expect (keep-evens (cons 4 empty)) (cons 4 empty))

;; keep-evens: (listof Int) -> (listof Int)
(define (keep-evens loi)
  (cond [(empty? loi) empty]
        [(even? (first loi)) (cons (first loi)
                                   (keep-evens (rest loi)))]
        [else (keep-evens (rest loi))]))


;; ex. 6
;; (longest-word words) produces the length of the longest word in the list of words.
;; Examples:
(check-expect (longest-word (cons "and" empty)) 3)
(check-expect (longest-word (cons "and" (cons "then" empty))) 4)

;; longest-word: (ne-listof Str) -> Nat
(define (longest-word words)
  (cond [(empty? words) 0]
        [else (max (string-length (first words))
                   (longest-word (rest words)))]))


;; ex. 7
;; (e->* s) Replace each "e" in s with " * "
;; Examples:
(check-expect (e->* "beekeeper") "b**k**p*r")

;; e->*: Str -> Str
(define (e->* s)
  (list->string (e->*/lst (string->list s))))


;; (e->*/lst loc) replaces each #\e in loc with #\*
;; Examples:
(check-expect (e->*/lst (cons #\h (cons #\e (cons #\y (cons #\! empty)))))
              (cons #\h (cons #\* (cons #\y (cons #\! empty)))))

;; e->*/lst: (listof Char) -> (listof Char) 
(define (e->*/lst loc)
  (cond [(empty? loc) empty]
        [(char=? #\e (first loc)) (cons #\* (e->*/lst (rest loc)))]
        [else (cons (first loc) (e->*/lst (rest loc)))]))


;; ex. 8
;; (add-first lon) adds the first number to all the numbers in lon
;; Examples:
(check-expect (add-first (cons 7 (cons 3 (cons 5 empty))))
              (cons 14 (cons 10 (cons 12 empty))))

;; add-first: (ne-listof Num) -> (ne-listof Num)
(define (add-first lon)
  (add-item (first lon) lon))


;; (add-item item lon) adds item to each number in lon
;; Examples:
(check-expect (add-item 7 (cons 7 (cons 3 (cons 5 empty))))
              (cons 14 (cons 10 (cons 12 empty))))

;; add-item: Num (ne-listof Num) -> (ne-listof Num)
(define (add-item item lon)
        (cond [(empty? lon) empty]
              [else (cons (+ item (first lon))
                          (add-item item (rest lon)))]))


;; ex. 9
;; (drop-first los) produces los with all copies of first item removed
;; Examples:
(check-expect (drop-first (cons 'A (cons 'V (cons 'B (cons 'A (cons 'D empty))))))
              (cons 'V (cons 'B (cons 'D empty))))

;; drop-first: (listof Sym) -> (listof Sym)
(define (drop-first los)
  (drop-first/list (first los) (rest los)))


;; (drop-first/list item los) does the same thing as drop-first, using simple recursion
;; Examples:
(check-expect (drop-first/list 'A (cons 'V (cons 'B (cons 'A (cons 'D empty)))))
              (cons 'V (cons 'B (cons 'D empty))))

;; drop-first/list: Sym (listof Sym) -> (listof Sym)
(define (drop-first/list item los)
  (cond [(empty? los) empty]
        [(symbol=? item (first los))
         (drop-first/list item (rest los))]
        [else (cons (first los)
                    (drop-first/list item (rest los)))]))