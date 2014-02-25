#lang eopl

; 1
(define empty-env
  (lambda () '() ))
(define extend-env
  (lambda (sym val env)
    (cons (list sym val) env)))

; 2
(define empty-env?
  (lambda (env)
    (cond ((null? env) #t)
          (else (#f)))))

; 3

; 4

; 5
; empty-stack, push, pop, top, and empty-stack?
(define empty-stack? (lambda (s) (s 'empty-stack?)))
(define value (lambda (s) (s 'value)))
(define pop (lambda (s) (s 'pop)))
(define top (lambda (s) (s 'top)))

(define empty-stack
  (lambda ()
    (lambda (op)
      (if (equal? op 'empty-stack?) #t
          (eopl:error 'empty-stack "op not defined on empty stack" op)))))
;(define push
;  (lambda (val stk)
;    (if (stk 'empty-stack?)
;        (lambda (op)
;          (cond
;            ((equal? op 'empty-stack?) #t)
;            ((equal? op 'value) val)
;            ((equal? op 'pop) empty-stack)
;            ((equal? op 'top) empty-stack)))
;        (lambda (op) ; non-empty stack
;          (cond
;            ((equal? op 'empty-stack?) #f)
;            ((equal? op 'value) (stk 'value))
            

; 6
(define-datatype bintree bintree?
  (leaf-node (num integer?))
  (interior-node (key symbol?)
                 (left bintree?)
                 (right bintree?)))

(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
      (leaf-node (value) (list 'leaf-node value))
      (interior-node (key left right)
                     (list 'interior-node key
                           (bintree-to-list left)
                           (bintree-to-list right))))))
; (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))

; 7
(define leaf-sum
  (lambda (tree)
    (cases bintree tree
      (leaf-node (value) value)
      (interior-node (key left right)
                     (+ (leaf-sum left)
                        (leaf-sum right))))))
; converts a tree into a list of (key leaf-sum) pairs
(define bintree-to-listsum
  (lambda (tree)
    (cases bintree tree
      (leaf-node (value) '())
      (interior-node (key left right)                   
                     (append (list (list key (leaf-sum tree)))
                             (bintree-to-listsum left)
                             (bintree-to-listsum right))))))

; (bintree-to-listsum tree-z)

; takes a listsum representation and finds the max 
; by comparing curr-max pair with head of the lst
; and moves the winner on
(define max-pair-finder-helper
  (lambda (lst max)
    (cond ((null? lst) max)
          ((< (cadr max) (cadar lst))
           (max-pair-finder-helper (cdr lst) (car lst)))
          (else (max-pair-finder-helper (cdr lst) max)))))

(define max-pair-finder
  (lambda (listsums)
    (max-pair-finder-helper listsums (car listsums))))
; (max-pair-finder '((z 14) (y 8) (x 7)))

(define max-interior
  (lambda (tree)
    (car (max-pair-finder (bintree-to-listsum tree)))))


; test trees
(define tree-x (interior-node 'x (leaf-node 3) (leaf-node 4)))
(define tree-y (interior-node 'y (leaf-node 1) tree-x))
(define tree-z (interior-node 'z (leaf-node 6) tree-y))

