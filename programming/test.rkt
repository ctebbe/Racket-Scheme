#lang racket
; Exercises 1.15, 1.16, 1.17, 1.18, 1.19, 1.20, 1.21,1.22 1.23, 1.24, 1.25, and 1.26 in EOPL, p. 26-28.
; Exercises 1.27, 1.28, 1.29 and 1.30 in EOPL, p. 27-28.

; 1.15
(define duple
    (lambda(n literal)
    (if (zero? n)
        '()
        (cons literal (duple (- n 1) literal)))))

(duple 2 3)


; 1.16
(define invert
    (lambda (lst)
    (if(null? lst)
    '()
    (cons (list(cadar lst)(caar lst))
        (invert(cdr lst))))))
(invert '((a 1) (a 2) (1 b) (2 b)))



; 1.17
(define down
    (lambda(lst)
    (map (lambda(x)(list x)) lst)))
(down '(1 2 3))
; 1.18
; 1.19
(define list-set
    (lambda(lst n x)
    cond ((null? lst) '())
        ((= n 0) (cons x lst)
        (else (cons (car lst)
                    (list-set (cdr lst)(- n 1) x))))))
(list-set '(a b c d) 2 '(1 2))
; 1.20
; 1.21
; 1.22
