(module top (lib "eopl.ss" "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).

  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "lang.scm")             ; for scan&parse
  (require "interp.scm")           ; for value-of-program
  (require "tests.scm")            ; for test-list
  
  (provide run run-all)
  
  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;
  
  ;; run : String -> ExpVal

  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))
  
  ;; run-all : () -> Unspecified

  ;; runs all the tests in test-list, comparing the results with
  ;; equal-answer?  

  (define run-all
    (lambda ()
      (run-tests! run equal-answer? test-list)))
  
  (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans (sloppy->expval correct-ans))))
  
  (define sloppy->expval 
    (lambda (sloppy-val)
      (cond
        ((number? sloppy-val) (num-val sloppy-val))
        ((boolean? sloppy-val) (bool-val sloppy-val))
        (else
         (eopl:error 'sloppy->expval 
                     "Can't convert sloppy value to expval: ~s"
                     sloppy-val)))))
    
  ;; run-one : Sym -> ExpVal
  ;; (run-one sym) runs the test whose name is sym
  (define run-one
    (lambda (test-name)
      (let ((the-test (assoc test-name test-list)))
        (cond
          ((assoc test-name test-list)
           => (lambda (test)
                (run (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))
  
  
  (define t0 "print -(x,v)")
  (define t01 "print +(x,v)")
  (define t02 "print *(2,3)")
  
  (define t2
    "{print +(1,0); print +(1,1)}")
  
  
  (define t3
    "{print -(x,v); print +(v,x)}")
  
  (define t4
    "if zero?(0) then print 1 else print 2")
  
  (define t5
    "while zero?(0) print 1")
  
  (define t6
    "v = 7")
  (define t7
    "var x,y; {x = 4; x = 5; print x}")
  (define t8
    "var x, y; {x = +(2,3); y = *(2,3)}")
  
  (define t9
    "var apple; {apple = +(8,5); print apple}")
  
  (define ex1
    "var x,y; {x = 3; y = 4; print +(x,y)}")
  
  (define ex2
    "var x,y,z; {x = 3; y = 4; z = 0;
                 while not(zero?(x))
                    {z = +(z,y); x = -(x,1)};
                 print z}")
  
  (define ex3
    "var x; {x = 3;
             print x;
             var x; {x = 4; print x};
             print x}")
  
  (define ex4
    "var f,x;{f = proc(x) proc(y) *(x,y);
              x = 3;
              print ((f 4) x)}")
  
  (define t10fact
    "var fact;{fact = proc(x)
                      if zero?(x)then 1
                      else *(x,(fact -(x, 1)));
               print (fact 4)}")
  ;(run-all)
  )