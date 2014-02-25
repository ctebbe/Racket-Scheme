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



; 5
; empty-stack, push, pop, top, and empty-stack?
(define empty-stack? (lambda (s) (s 'isempty?)))
(define value (lambda (s) (s 'value)))

(define empty-stack
  (lambda ()
    (lambda (op)
      (if (equal? op 'empty-stack?) #t
          (eopl:error 'empty-stack "op not defined on empty stack" op)))))

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

