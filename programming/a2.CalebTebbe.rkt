; A2: Caleb Tebbe 02-12-2014
; "I have not given, received, or used any unauthorized assistance." - Caleb Tebbe

;#lang racket
#lang eopl
; Exercises 1.15, 1.16, 1.17, 1.18, 1.19, 1.20, 1.21,1.22 1.23, 1.24, 1.25, and 1.26 in EOPL, p. 26-28.
; Exercises 1.27, 1.28, 1.29 and 1.30 in EOPL, p. 27-28.

; 1.15
(define duple
    (lambda (n str)
    (if (zero? n)
        '()
        (cons str (duple (- n 1) str))))) ; print str once and pass off to recursive call

;(duple 2 3)
;(duple 4 '(ha ha))
;(duple 0 '(blah))

; 1.16
(define invert
    (lambda (lst)
    (if(null? lst)
    '()
    (cons (list (car (cdr (car lst))) (car (car lst))) ; reverse first lst
        (invert(cdr lst)))))) ; recurse on rest of lst
;(invert '((a 1) (a 2) (1 b) (2 b)))

; 1.17
(define down
    (lambda (lst)
    (map (lambda(x) (list x)) lst))) ; put each element in lst in a list to add another set of ()
;(down '(1 2 3))
;(down '(a (more (complicated)) object))

; 1.18
(define swapper
  (lambda (s1 s2 slist)
    (cond ((null? slist) '())
          ((list? (car slist)) (cons (swapper s1 s2 (car slist)) (swapper s1 s2 (cdr slist))))
          ((equal? s1 (car slist)) (cons s2 (swapper s1 s2 (cdr slist))))
          ((equal? s2 (car slist)) (cons s1 (swapper s1 s2 (cdr slist))))
          (else (cons (car slist) (swapper s1 s2 (cdr slist)))))))
;(swapper 'a 'd '(a b c d))
;(swapper 'a 'd '(a d () c d))
;(swapper 'x 'y '((x) y (z (x))))

; 1.19
(define list-set
  (lambda (lst n x)
    (cond ((null? lst) '()) ; null list
          ((= n 0) (cons x (cdr lst))) ; at the element to replace
          (else (cons (car lst) ; cons current element with results on rest of list
                      (list-set (cdr lst) (- n 1) x))))))
;(list-set '(a b c d) 2 '(1 2))
;(list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)

; 1.20
(define count-occurrences
  (lambda (s slist)
    (cond ((null? slist) 0)
          ((list? (car slist)) ; first element is a list so add its result with rest of the list
           (+ (count-occurrences s (car slist))
              (count-occurrences s (cdr slist))))
          ((eq? s (car slist)) ; found s, add 1 to result on rest of list
           (+ 1 (count-occurrences s (cdr slist))))
          (else (count-occurrences s (cdr slist)))))) ; no results on the first element recurse on rest of list
;(count-occurrences 'x '((f x) y (((x z) x))))
;(count-occurrences 'x '((f x) y (((x z) () x))))
;(count-occurrences 'w '((f x) y (((x z) x))))

; 1.21
(define product
  (lambda (sos1 sos2)
    (if (null? sos1)
      '()
      ; must return a new list so append
      (append (map (lambda (x) (list (car sos1) x)) sos2) ; first element of sos1 with all of sos2
              (product (cdr sos1) sos2))))) ; recurse on rest of sos1
;(product '(a b c) '(x y))

; 1.22
(define filter-in
  (lambda (pred lst)
    (cond ((null? lst) '())
    ((pred (car lst)) (cons (car lst) (filter-in pred (cdr lst))))
    (else (filter-in pred (cdr lst))))))
; (filter-in number? '(a 2 (1 3) b 7))
; (filter-in symbol? '(a (b c) 17 foo))

; 1.23
(define list-index-helper
  (lambda (pred lst cnt)
    (cond ((null? lst) #f)
          ((pred (car lst)) cnt);)))
          (else (list-index-helper pred (cdr lst) (+ cnt 1))))))
(define list-index
  (lambda (pred lst)
    (list-index-helper pred lst 0)))
;(list-index number? '(a 2 (1 3) b 7))
;(list-index symbol? '(a (b c) 17 foo))
;(list-index symbol? '(1 2 (b c) 3))

; 1.24
(define every?
  (lambda (pred lst)
    (cond ((null? lst) #t)
          ((not (pred (car lst))) #f)
          (else (every? pred (cdr lst))))))
;(every? number? '(a b c 3 e))
;(every? number? '(1 2 3 4 5))
;(every? number? '(1 2 3 4 r))

; 1.25
(define exists?
  (lambda (pred lst)
    (cond ((null? lst) #f)
          ((pred (car lst)) #t)
          (else (exists? pred (cdr lst))))))
;(exists? number? '(a b c d 3 e))
;(exists? number? '(a b c d e))
;(exists? number? '(1 2 3 4 r))

; 1.26
(define up
  (lambda (lst)
    (cond ((null? lst) '())
          ((list? (car lst)) (append (car lst) (up (cdr lst))))
          (else (cons (car lst) (up (cdr lst)))))))
;(up '((1 2) (3 4)))
;(up '((x (y)) z))

; 1.27
(define flatten
  (lambda (slist)
    (cond ((null? slist) '())
          ((null? (car slist)) (flatten (cdr slist))) ; nothing to flatten so exclude it
          ((list? (car slist)) (append (flatten (car slist)) (flatten (cdr slist)))) ; append results of car list to cdr list
          (else (cons (car slist) (flatten (cdr slist))))))) ; car must be an individual char
;(flatten '(a b c))
;(flatten '((a) () (b ()) () (c)))
;(flatten '((a b) c (((d)) e)))
;(flatten '(a b (() (c))))

; 1.28
(define merge
  (lambda (loi1 loi2)
    (cond ((null? loi1) loi2)
          ((null? loi2) loi1)
          ((< (car loi1) (car loi2)) (cons (car loi1) (merge (cdr loi1) loi2)))
          (else (cons (car loi2) (merge loi1 (cdr loi2)))))))
;(merge '(1 4) '(1 2 8))
;(merge '(35 62 81 90 91) '(3 83 85 90))

; 1.29
(define sort
  (lambda (lst)
    (cond ((null? lst) '())
          ((null? (cdr lst)) lst)
          (else (merge (cons (car lst) '())
                       (sort (cdr lst)))))))
;(sort '(8 2 5 2 3))
;(sort '(90 80 70 60 50 40 30 20 10 1))

; 1.30
(define sort/predicate
  (lambda (pred lst)
    ; use these to figure out which elements are pred/not pred of a given element
    (define is-pred (lambda (predicate toCompare) (lambda (x) (predicate x toCompare))))
    (define is-not-pred (lambda (predicate toCompare) (lambda (x) (not (pred x toCompare)))))
    (cond ((null? lst) '())
          (else (append (sort/predicate pred (filter-in (is-pred pred (car lst)) (cdr lst)))
                        (list (car lst)) ; has to be a list or append complains
                        (sort/predicate pred (filter-in (is-not-pred pred (car lst)) (cdr lst))))))))
;(sort/predicate < '(8 2 5 2 3))
;(sort/predicate > '(8 2 5 2 3))
