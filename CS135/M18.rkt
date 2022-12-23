;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname M18) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; A Node is a Sym

;; A Graph is one of:
;; * empty
;; * (cons (list v (list w _ 1 ... w _ n)) g)
;; where g is a Graph
;;   v, w _ 1, ... w _ n are Nodes
;;   v is the in-neighbour to w _ 1 ... w _ n in the Graph
;;   v does not appear as an in-neighbour in g


(define g
  '((A (C D E))
    (B (E J))
    (C ())
    (D (F J))
    (E (K))
    (F (K H))
    (H ())
    (J (H))
    (K ())))



;; ex 1
;; (count-out-neighbours g) produces the number of out-neighbours of each Node in <g>
(check-expect (count-out-neighbours g) (list 3 2 0 2 1 2 0 1 0))

;; count-out-neighbours: Graph -> (listof Nat)
(define (count-out-neighbours g)
  (map (lambda (x) (length (second x))) g))


;; ex 2
;; (count-in-neighbours g) counts how many in-neighbours each node has
(check-expect (count-in-neighbours g) (list 0 0 1 1 2 1 2 2 2))

;; count-in-neighbours: Graph -> (listof Nat)
(define (count-in-neighbours g)
  (map (lambda (x)
         (length (filter (lambda (y)
                           (member? (first x) (second y))) ; all out-neighbours having (first x)
                         g))) g))


;; ex 3
;; (k-path-length start k)


;; ex 4
;; (make-diamond-graph n) produces a Graph with <n> diamonds
(check-expect (make-diamond-graph 2) '((D1 (D1a D1b))
                                       (D1a (D2))
                                       (D1b (D2))
                                       (D2 (D2a D2b))
                                       (D2a (D3))
                                       (D2b (D3))))
;; make-diamond-graph: Nat -> Graph
(define (make-diamond-graph n)
  (cond [(zero? n) empty]
        [else (append (make-diamond-graph (sub1 n))
                      (list (list (mk-node n "") (list (mk-node n "a") (mk-node n "b"))))
                      (list (list (mk-node n "a") (list (mk-node (add1 n) ""))))
                      (list (list (mk-node n "b") (list (mk-node (add1 n) "")))))]))
                           

;; (mk-node n suffix) produces a node with name 'D<n><suffix>
;; mk-node: Nat Str -> Sym
(define (mk-node n suffix)
  (string->symbol (string-append "D" (number->string n) suffix)))


;; ex 5
(define simple-graph '((a (i j k))
                       (j ())
                       (k (a j))
                       (i (j))))
(define complement-graph '((a ())
                           (j (i a k))
                           (k (i))
                           (i (a k))))


;; (graph-complement g) produces the complement of <g>
(check-expect (graph-complement simple-graph simple-graph) complement-graph)


;; graph-complement: Graph -> Graph
(define (graph-complement g g1)
  (cond [(empty? g) empty]
        [else (local [(define node (first (first g)))
                      (define nbrs (second (first g)))]
                      
         (cons (list node (out-neighbours node nbrs g1))
                    (graph-complement (rest g) g1)))]))


;; (out-neighbours node g) produces the new out-neighbours of <node>
(check-expect (out-neighbours 'k'(a j) simple-graph) '(i))
(check-expect (out-neighbours 'a '(i j k) simple-graph) '())

;; out-neighbours: Sym (listof Sym) Graph -> (listof Sym)
(define (out-neighbours node nbrs g)
  (cond [(empty? g) empty]
        [(and (not (symbol=? node (first (first g))))
              (not (member? (first (first g)) nbrs)))
         (cons (first (first g))
               (out-neighbours node nbrs (rest g)))]
        [else (out-neighbours node nbrs (rest g))]))


;; if some node appears in other nodes' out-neighbours,
;; and that node is not a member of out-neighbours of that node
;; then those nodes are the new out-neighbours of that node


























