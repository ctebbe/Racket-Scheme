#lang racket
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
;(define swapper
;  (lambda (s1 s2 slist)))

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
      (append (map (lambda (x) (list (car sos1) x)) sos2) ; first element of sos1 with all of sos2
              (product (cdr sos1) sos2))))) ; recurse on rest of sos1
;(product '(a b c) '(x y))
; 1.22
