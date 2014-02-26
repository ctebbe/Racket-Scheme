#lang eopl
; Caleb Tebbe
; “I have not given, received, or used any unauthorized assistance.” - Caleb Tebbe

; 1
;empty-env
(define empty-env
  (lambda () '() ))

; extend-env(var val env)
(define extend-env
  (lambda (var val env)
    (cons (list var val) env)))

; 2
; empty-env?
(define empty-env?
  (lambda (env)
    (cond ((null? env) #t)
          (else (#f)))))

; 3
; has-binding?(env var)
(define has-binding?
  (lambda (env var)
    (cond ((null? env) #f)
          ;((empty-env? env) #f) keeps failing here for some reason
          ((equal? var (caar env)) #t)
          (else (has-binding? (cdr env) var)))))

; 4
; extend-env*(vars vals env)
(define extend-env*
  (lambda (vars vals env)
    (cond ((null? vars) env) ; since we can assume vars.size == vals.size only check one
          (else (extend-env* (cdr vars) (cdr vals) 
                             (extend-env (car vars) (car vals) env))))))


; test envs
(define test-env (extend-env 'x 1 (extend-env 'y 2 (empty-env))))
(define ext-env (extend-env* '(a b c d) '(3 4 5 6) (extend-env 'x 1 (extend-env 'y 2 (empty-env)))))

; 5
; empty-stack, push, pop, top, and empty-stack?

; wrapper functions for the stack
(define empty-stack? (lambda (stk) (stk 'empty-stack?)))
(define pop (lambda (stk) (stk 'pop)))
(define top (lambda (stk) (stk 'top)))

(define empty-stack
  (lambda ()
    (lambda (op)
      (if (equal? op 'empty-stack?) #t
          (eopl:error 'empty-stack "op not defined on empty stack" op)))))

(define push
  (lambda (item stk)
    (lambda (op)
      (cond
        ((equal? op 'empty-stack?) #f)
        ((equal? op 'pop) stk)
        ((equal? op 'top) item)))))

; test stacks
; ((empty-stack) 'empty-stack?)
; ((empty-stack) 'pop) <-- throws error
(define stk-x (push 'x empty-stack))
(define stk-y (push 'y stk-x))
(define stk-z (push 'z stk-y))

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

(define max-interior
  (lambda (tree)
    (car (max-pair-finder (bintree-to-listsum tree)))))

; test trees
(define tree-x (interior-node 'x (leaf-node 3) (leaf-node 4)))
(define tree-y (interior-node 'y (leaf-node -1) tree-x))
(define tree-z (interior-node 'z (leaf-node 1) tree-y))
; (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
; (bintree-to-listsum tree-z)
; (max-pair-finder '((z 14) (y 8) (x 7)))
; (max-interior tree-y)
