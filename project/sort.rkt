#lang racket
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
