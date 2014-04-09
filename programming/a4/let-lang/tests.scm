(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-diff "-(44,33)" 11)
      (simple-arith-add "+(40,10)" 50)
      (simple-arith-mult "*(4,5)" 20)
      (simple-arith-div "/(10,5)" 2)
      (simple-arith-div-2 "/(11,5)" 2) ; integer division
      
      ;; minus
      (simple-minus "minus(5)" -5)
      (nested-minus "minus(-(minus(5),9))" 14)
  
      ;; nested arithmetic
      (nested-arith-left-diff "-(-(44,33),22)" -11)
      (nested-arith-right-diff "-(55, -(22,11))" 44)
      
      (nested-arith-left-add "+(+(5,5),10)" 20)
      (nested-arith-right-add "+(2, +(22,11))" 35)
      
      (nested-arith-left-mult "*(*(5,5),10)" 250)
      (nested-arith-right-mult "*(2, *(2,5))" 20)
      
      (nested-arith-left-div "/(/(20,5),2)" 2)
      (nested-arith-right-div "/(6, /(10,5))" 3)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      (if-true-equal "if equal?(5, 5) then 3 else 4" 3)
      (if-false-equal "if equal?(5, 4) then 3 else 4" 4)
      
      (if-true-greater "if greater?(5, 4) then 3 else 4" 3)
      (if-false-greater "if greater?(4, 5) then 3 else 4" 4)
      (if-true-greater "if greater?(5, 5) then 3 else 4" 4)
      
      (if-true-less "if less?(4, 5) then 3 else 4" 3)
      (if-false-less "if less?(5, 4) then 3 else 4" 4)
      (if-true-less "if less?(5, 5) then 3 else 4" 4)
      
      ;; null/emptylist tests
      (if-null "if null?(emptylist) then 3 else 4" 3)
      (if-not-null "if null?(1) then 3 else 4" 4)
      
      ;; cons/car/cdr
      (car-test "car(cons(2,3))" 2)
      (cdr-test "cdr(cons(2,3))" 3)
      (nested-cons-test-1 "car(cdr(cons(2,cons(3,4))))" 3)
      (nested-cons-test-2 "car(cons(2,cons(3,4)))" 2)
      (nested-cons-test-3 "car(cdr(cdr(cons(2,cons(3,cons(4, 5))))))" 4)
      
      ;; cond
      (cond-test-1 "cond equal?(3,3) ==> 4 end" 4)
      ;(cond-test-2 "cond equal?(2,1)==>5 greater?(3,2)==>3 end" 3)

      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

      ))
  )