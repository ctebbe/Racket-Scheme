(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the IMPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-program (stat) ; everything from the top-level is a statement
          (execute-stat stat (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 118, 119
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) 
        ;              = (deref (apply-env \r \x{}))}
        (var-exp (var) (deref (apply-env env var)))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
        (add-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (+ num1 num2)))))
        (mult-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (* num1 num2)))))
        
        (not-exp (exp)
                 (let ((val1 (value-of exp env)))
                   (let ((val2 (expval->bool val1)))
                     (if val2
                         (bool-val #f)
                         (bool-val #t)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ; edited commented out
;        (if-exp (exp1 exp2 exp3)
;          (let ((val1 (value-of exp1 env)))
;            (if (expval->bool val1)
;              (value-of exp2 env)
;              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((v1 (value-of exp1 env)))
            (value-of body
              (extend-env var (newref v1) env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

;        (call-exp (rator rand)
;          (let ((proc (expval->proc (value-of rator env)))
;                (arg (value-of rand env)))
;            (apply-procedure proc arg)))
        
        (call-exp (rator rands)
                  (let ((proc (expval->proc (value-of rator env)))
                        (args (map (lambda(x)
                                     (value-of x env)) rands)))
                    (apply-procedure proc args)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (assign-exp (var exp1)
          (begin
            (setref!
              (apply-env env var)
              (value-of exp1 env))
            (num-val 27)))

        )))

  (define execute-stat
    (lambda (stat env)
      (cases statement stat
        (print-stat (exp)
                    (print-exp (value-of exp env)))
        (block-stat (stats)
                    (execute-stats stats env))
        (if-stat (test-exp t-stat f-stat)
                 ((let ((val (value-of test-exp env)))
                    (if (expval->bool val)
                        (execute-stat t-stat)
                        (execute-stat f-stat)))))
        (while-stat (exp stat)
                    (execute-while-stat exp stat env))
        (set-stat (var exp)
                  (setref!
                   (apply-env env var)
                   (value-of exp env)))
        (declare-stat (vars stat)
                      (execute-stat stat (extend-uninit-env* vars env))))))
  
  (define execute-stats
    (lambda (stats env)
      (when (not (null? stats))
        (begin
          (execute-stat (car stats) env)
          (execute-stats (cdr stats) env)))))
  
  (define execute-while-stat
    (lambda (exp stat env)
      (let ((val (value-of exp env)))
            (when (expval->bool val)
                (begin
                  (execute-stat stat env)
                  (execute-while-stat exp stat env))))))
  
  (define print-exp
    (lambda (exp)
      (cases expval exp
        (num-val (v)
                 (eopl:printf "~a\n" v)) ; turn value into a string to easily print
        (bool-val (v)
                  (eopl:printf "~a\n" v))
        (proc-val (v)
                  (eopl:printf "~a\n" v))
        (ref-val (v)
                 (eopl:error "print-exp cannot print reference...\n")))))


  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 119

  ;; uninstrumented version
  ;;  (define apply-procedure
  ;;    (lambda (proc1 val)
  ;;      (cases proc proc1
  ;;        (procedure (var body saved-env)
  ;;          (value-of body
  ;;            (extend-env var (newref val) saved-env))))))
  
  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
          (let ((r (newref arg)))
            (let ((new-env (extend-env var r saved-env)))
              (when (instrument-let)
                (begin
                  (eopl:printf
                    "entering body of proc ~s with env =~%"
                    var)
                  (pretty-print (env->list new-env)) 
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))  
  
    (define extend-uninit-env*
    (lambda (vars env)
      (if (null? vars) env
          (extend-uninit-env* (cdr vars)
                             (extend-env (car vars)
                                         (newref (num-val 0))
                                         env)))))

  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (list
            (car p)
            (expval->printable (cadr p))))
        l)))

  )
  


  
