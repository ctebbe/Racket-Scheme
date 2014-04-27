(module lang (lib "eopl.ss" "eopl")                
  
  ;; language for IMPLICIT-REFS

  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (statement) a-program)

      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      (expression
        ("+" "(" expression "," expression ")")
        add-exp)
      (expression
        ("*" "(" expression "," expression ")")
        mult-exp)
      
      (expression
       ("not" "(" expression ")")
       not-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

;      (expression
;       ("if" expression "then" expression "else" expression)
;       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      (expression
       ("proc" "(" (separated-list identifier ",") ")" expression)
       proc-exp)

      (expression
        ("letrec"
          (arbno identifier "(" identifier ")" "=" expression)
           "in" expression)
        letrec-exp)
      
      (expression
        ("begin" expression (arbno ";" expression) "end")
        begin-exp)

      ;; new for implicit-refs

      (expression
        ("set" identifier "=" expression)
        assign-exp)
      
      (statement
       ("print" expression)
       print-stat)
      
      (statement
       ("{" (separated-list statement ";") "}")
       block-stat)
            
      (statement
       ("if" expression "then" statement "else" statement)
       if-stat)
      
      (statement
       ("while" expression statement)
       while-stat)
      
      (statement
       (identifier "=" expression)
       set-stat)
      
      (statement
       ("var" (separated-list identifier ",") ";" statement)
       declare-stat)
      
      (expression
       ("(" expression expression ")")
       call-exp)

      ))
  

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
