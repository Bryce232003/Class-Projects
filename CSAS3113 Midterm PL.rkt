#lang eopl
;;;# Mini Functional Language Interpreter
;;;This is an interpreter for a small language implemented in Racket using the EoPL (`#lang eopl`) language tools.

 
(require rackunit "../eopl-extras.rkt")
 
;;Q1 Answered

;;Q2 Answered

;;Q3 Answered


;;Q1
;; List definition
; List ::=()
;      ::= (Exp-val . List)

;; Inference Rules
;; For Empty & Non-Empty Lists

; (value-of (list-exp '())) = (empty-list)
; (value-of exp ρ σ) = (val1, σ1)
; (value-of (list-exp exp exp .... exp) ρ σ1) = (val2, σ2)
; _____________________________________________________________________
;   (value-of (list-exp (cons-exp exp exp))) = ((pair-val val1 val2) σ2)

;; For Cons
; (where L1 is a list)
;        (value-of exp1 ρ σ) ^ (value-of exp2 ρ σ) ∈ List
;        (value-of exp1 ρ σ) ^ L1 ∈ List
; _______________________________________________________________________
; (value-of (cons exp1 (cons exp2 ... L1)) ρ σ) = ((list2listval(list exp1 exp2 L1)) ρ σ1)    

;; For Car
;     L = (listval2list(list exp-val1 exp-val2 exp-val3 ... ()))
; ____________________________________________________________
;         (value-of (car L)ρ σ) = exp-val1

;; For Cdr
;     L = (listval2list(list exp-val1 exp-val2 exp-val3 ... ())) 
; ____________________________________________________________
;         (value-of (cdr L) ρ σ) = (list2listval(list exp-val2 exp-val3 ... ())) 

;; For null?
;    (listval2list(list exp-val1 exp-val2 exp-val3 ... ())) = L
; ____________________________________________________________
;  (value-of (null? L) ρ σ) = [(bool-val #t)     if L = emptylist-val        
;                             [(bool-val #f)     else L = list-val (not empty)


;;Q2
; Inference rule for let*
; (value-of exp1 ρ σ) = (val1, σ1)
; (value-of exp2 ρ σ) = (val2, σ2)
;_________________________________________________________________________
; (value-of (let* ((var1 exp1) (var2 exp2) .... (varn expn))body)ρ σ = ((pair-vals varn expn), σn+1)

 
 
;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
 
(define the-lexical-spec
  '((whitespace (whitespace) skip)
    
    (comment ("%" (arbno (not #\newline))) skip)
    
    (identifier
     (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    
    (number (digit (arbno digit)) number)
    
    (number ("-" digit (arbno digit)) number)
    ))
 
(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)
 
    (expression ("true") true-exp)
 
    (expression ("false") false-exp)
 
    (expression (identifier) var-exp)
    
    (expression("-" "(" expression "," expression ")")diff-exp)
    
    (expression ("zero?" "(" expression ")") zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression) if-exp)
    
    (expression 
     ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    
    (expression
     ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression)
               "in" expression) letrec-exp)
    ;;Q2 Grammar
    (expression
     ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
    
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    
    (expression ("(" expression (arbno expression) ")") call-exp)
 
    ;;Q1 Grammar 
    (expression ("cons" "(" expression "," expression ")") cons-exp)
 
    ;;Q1 Grammar 
    (expression ("car" "(" expression ")") car-exp)
 
    ;;Q1 Grammar 
    (expression ("cdr" "(" expression ")") cdr-exp)
 
    ;;Q1 Grammar 
    (expression ("null?" "(" expression ")") null?-exp)
 
    ;;Q1 Grammar 
    (expression ("list" "(" (arbno expression) ")") list-exp)
 
    ;;Q1 Grammar 
    (expression ("emptylist") emptylist-exp)

    ))
 
;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
 
(sllgen:make-define-datatypes the-lexical-spec the-grammar)
 
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
 
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
 
(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
 
;;;;;    ENVIRONMENT (NO MIDTERM CHANGES)
 
(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?)))
 
(define (apply-env env search-sym)
  (cases environment env
    (empty-env ()
               (eopl:error 'apply-env "No binding for ~s" search-sym))
    (extend-env (var val saved-env)
                (if (eqv? search-sym var)
                    val
                    (apply-env saved-env search-sym)))))


 
;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;
 
;;; an expressed value is either a number, a boolean, a procval, or a list.
 
(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?))
  
  ;Q1 List Datatype
  (list-val
   (first expval?)
   (rest expval?))
  
  ;Q1 List Datatype
  (emptylist-val))
 
;;;;;;;;;;;;;;;;;;;;;; extractors:
 
;; expval2num : ExpVal -> Int
;; expval --> Int throws error
;; Purpose: Extract number from given expval
(define (expval2num v)
  (cases expval v
    (num-val (num) num)
    (else (expval-extractor-error 'num-val v))))
 
;; expval --> Bool throws error
;; Purpose: Extract Boolean from given expval
(define (expval2bool v)
  (cases expval v
    (bool-val (bool) bool)
    (else (expval-extractor-error 'bool-val v))))
 
;; expval --> proc throws error
;; Purpose: Extract proc from given expval
(define (expval2proc v)
  (cases expval v
    (proc-val (proc) proc)
    (else (expval-extractor-error 'proc-val v))))

;;Q1
;; expval --> List (of expvals)
;; Returns a list of expvals 
(define (expval2list v)
  (cases expval v
    (num-val (num) (list (num-val num)))
    (bool-val (bool) (list (bool-val bool)))
    (proc-val (proc) (list (proc-val proc)))
    (list-val (first rest) (list first rest))
    (emptylist-val () (list (emptylist-val)))))
                                   

 
;; symbol expval --> throws error
(define (expval-extractor-error variant value)
  (eopl:error 'expval-extractors "Looking for a ~s, given ~s"
              variant value))
 
 
;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;
 
;; Any --> Boolean
;; Purpose: Determine if given value is a vector with a single environment
(define (voenv? penv)
  (and (vector? penv)
       (= (vector-length penv) 1)
       (environment? (vector-ref penv 0))))
 
(define-datatype proc proc?
  (procedure
   (var (list-of symbol?))
   (body expression?)
   (envv voenv?)))


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;
 
;; value-of-program : Program -> ExpVal
(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
               (value-of exp1 (empty-env)))))

;Q1 & Q2
;; value-of : expression environment -> expval
(define (value-of exp env)
  (cases expression exp
    
    (const-exp (num) (num-val num))
 
    (true-exp () (bool-val #t))
 
    (false-exp () (bool-val #f))
    
    (var-exp (var) (apply-env env var))
    
    (diff-exp (exp1 exp2)
              (let ((num1 (expval2num (value-of exp1 env)))
                    (num2 (expval2num (value-of exp2 env))))
                (num-val (- num1 num2))))
    
    (zero?-exp (exp1)
               (let ((val1 (expval2num (value-of exp1 env))))
                 (if (zero? val1)
                     (bool-val #t)
                     (bool-val #f))))
    
    (if-exp (exp1 exp2 exp3)
            (let ((val1 (value-of exp1 env)))
              (if (expval2bool val1)
                  (value-of exp2 env)
                  (value-of exp3 env))))
    
    (let-exp (vars exps body)       
             (let [(vals (map (lambda (e) (value-of e env)) exps))]
               (value-of body
                         (foldr (lambda (var val acc)
                                  (extend-env var val acc))
                                env
                                vars
                                vals))))
    
    ;Q2 -- let* expression
    (let*-exp (vars exps body)
              ; holds the representation for the env
              (let rep ((env env)
                        (ids vars)
                        (vals exps))
                (if (null? ids)
                    (value-of body env)
                    (rep (extend-env (car ids) (value-of (car vals) env) env)
                         (cdr ids)
                         (cdr vals)))))
    
    (proc-exp (params body)
              (proc-val (procedure params body (vector env ))))
    
    (call-exp (rator rands)
              (let [(proc (expval2proc (value-of rator env)))
                    (args (map (lambda (rand) (value-of rand env)) rands))]
                (apply-procedure proc args)))
 
    (letrec-exp (names params bodies letrec-body)
                (value-of letrec-body (mk-letrec-env names params bodies env)))

    ;;Q1 -- Empty List Exp
    (emptylist-exp ()
                   (emptylist-val))
    
    ;;Q1 -- List Exp
    (list-exp (exps)
              (let ((vals (map (lambda (e) (value-of e env)) exps)))
                (foldr (lambda (v acc) (list-val v acc)) (emptylist-val) vals)))
 
    ;;Q1 -- Cons Exp
    (cons-exp (exp1 exp2)
              (list-val (value-of exp1 env)
                        (value-of exp2 env)))
  
    ;;Q1 -- Car Exp
    (car-exp (exp)
             (car(expval2list (value-of exp env))))
        
    ;;Q1 -- Cdr Exp
    (cdr-exp (exp)
             (cdr (expval2list (value-of exp env))))
      
    ;;Q1 -- Null Exp
    (null?-exp (exp)
               (cases expval (value-of exp env)
                 (emptylist-val () (bool-val #t))
                 (else (bool-val #f))))
    ))



; ----------------------------------------------- Q3 ---------------------------------------------------
;; enviorment parameters expression -> env (env contains all the free vars in the exp)
;; find the free variables in the expression
(define (free-var env params exp)
  ; holds the representation for the env
  (let rep ((accum (empty-env))
            (bound-vars params)
            (exp exp))
    (cases expression exp
    
      (const-exp (num) accum)

      (true-exp () accum)
    
      (false-exp () accum)

      (var-exp (var) (if (memv var bound-vars)
                         accum
                         (extend-env var (apply-env env var) accum)))

      (diff-exp (exp1 exp2)
                (rep (rep accum bound-vars exp1) bound-vars exp2))

      (zero?-exp (exp1) (free-var env bound-vars exp1))

      (if-exp (exp1 exp2 exp3)
              (rep
               (rep
                (rep accum bound-vars exp1)
                bound-vars exp2)
               bound-vars exp3))

      (let-exp (vars exps body)
               (let ((new-env (foldl 
                               (lambda (e a) (rep a bound-vars e))
                               accum
                               exps)))
                 (rep new-env (append vars bound-vars) body)))


      
      (let*-exp (vars exps body)
                ; helper function to processess bindings
                (define (let*-bind-processor accum bound-vars vars exps)
                  (if (null? vars)
                      accum
                      (let ((curr-var (car vars))
                            (curr-exp (car exps)))
                        ;helper for getting free vars inside processess bindings
                        (let ((curr-exp-freevars (rep accum bound-vars curr-exp)))
                          (let*-bind-processor curr-exp-freevars (cons curr-var bound-vars) (cdr vars) (cdr exps))))))
                ; helper to process the let*-bindings
                (let ((env-bindings (let*-bind-processor accum bound-vars vars exps)))
                  (rep env-bindings (append vars bound-vars) body)))

    
      (cons-exp (exp1 exp2)
                (rep (rep accum bound-vars exp1) bound-vars exp2))
              
    
      (car-exp (exp)  (rep accum bound-vars exp))
    
      (cdr-exp (exp) (rep accum bound-vars exp))
    
      (null?-exp (exp) (rep accum bound-vars exp))
    
      (list-exp (exps)
                ;Another Env needed to process a case more than once
                (let rep2 ((remaining-exps exps)
                           (current-env accum))
                  (if (null? remaining-exps)
                      current-env
                      (rep2 (cdr remaining-exps)(rep current-env bound-vars (car remaining-exps))))))
              

                  
      (emptylist-exp () (empty-env))

      (proc-exp (params body)
                (rep accum (append params bound-vars) body))

      (call-exp (rator rands)
                ;Another Env needed to process a case more than once
                (let rep2 ((accum (rep accum bound-vars rator))
                           (oprands rands))
                  (if (null? oprands)
                      accum
                      (rep2(rep accum bound-vars (car oprands))
                           (cdr oprands)))))


   
      (letrec-exp (names params bodies letrec-body)
                  ;need every bounded var since they can be used anywhere in the letrec
                  (let ((every-bounded-var (append names bound-vars)))
                    ;free vars in bodies
                    (let ((free-bodies 
                           (foldl 
                            (lambda (e a) 
                              (rep a every-bounded-var e))
                            accum
                            bodies)))
                      (rep free-bodies  every-bounded-var letrec-body))))

   
      )))
    
 
;; (listof symbol) (listof (listof symbol)) (listof expression) environment --> environment
;; Purpose: Add the proc-vals for the given procedures in the given environment
(define (mk-letrec-env names params bodies env)
  (let* [(temp-proc-vals (map (lambda (p b)
                                (proc-val (procedure p b (vector (empty-env)))))
                              params
                              bodies))
         (new-env (foldl (lambda (name proc env)
                           (extend-env name
                                       proc
                                       env))
                         env
                         names
                         temp-proc-vals))]
    (begin
      (for-each (lambda (p)
                  (cases proc p
                    (procedure (p b ve)
                               (vector-set! ve 0 new-env))))
                (map (lambda (p) (expval2proc p))
                     temp-proc-vals))
      new-env)))

 
;; apply-procedure : proc (listof expval) -> expval
;; Purpose: Apply the given procedure to the given values
(define (apply-procedure f vals)
  (cases proc f
    (procedure (params body envv)
               (let [(saved-env (vector-ref envv 0))]
                 (value-of body
                           (foldr (lambda (binding acc)
                                    (extend-env (car binding)
                                                (cadr binding)
                                                acc))
                                  saved-env
                                  (map (lambda (p v) (list p v))
                                       params
                                       vals)))))))
 
;;;;;;   EVALUATION WRAPPERS
 
;; string -> a-program
;; Purpose: Parse the given extended LC-program
(define (parse p) (scan&parse p))
 
;; string -> ExpVal
;; Purpose: Evaluate the given extended LC-program
(define (eval string)
  (value-of-program (parse string)))
 
;;;;; EXAMPLES OF EVALUATION ;;;;;;;;;;;;

; Below are example enviroments used for the sake of testing free variables.
; For free-var to be called & tested the variable has to be declared.
; The num-vals / bool-vals are dummy values that hold no significance.
; The start of those tests are at line 632

(define initial-env 
  (let* ((env1 (extend-env 'x (num-val 2) (empty-env)))
         (env2 (extend-env 'z (num-val 20) env1)))
    env2))

(define initial-env2 
  (let* ((env1 (extend-env 'x (num-val 2) (empty-env))))
    env1))

(define initial-env3 
  (let* ((env1 (extend-env 'x (num-val 2) (empty-env)))
         (env2 (extend-env 'y (num-val 10) env1))
         (env3 (extend-env 'z (num-val 30) env2)))
    env3))

(define initial-env4
  (let* ((env1 (extend-env 'x (num-val 4) (empty-env)))
         (env2 (extend-env 'y (num-val 5) env1)))
    env2))

(define initial-env5
  (let* ((env1 (extend-env 'y (bool-val #t) (empty-env))))
    env1))

(define initial-env6 
  (let* ((env1 (extend-env 'x (num-val 2) (empty-env)))
         (env2 (extend-env 'y (num-val 10) env1))
         (env3 (extend-env 'z (num-val 30) env2))
         (env4 (extend-env 'w (bool-val #t) env3)))
    env4))


(define initial-env7 ; For given test 
  (let* ((env1 (extend-env 'x (num-val 2) (empty-env)))
         (env2 (extend-env 'y (num-val 10) env1))
         (env3 (extend-env 'z (num-val 30) env2))
         (env4 (extend-env 'i (bool-val #t) env3))
         (env5 (extend-env 'j (num-val 3) env4))
         (env6 (extend-env 'a (num-val 43) env5))
         (env7 (extend-env 'b (num-val 0) env6))
         (env8 (extend-env 'h (num-val 74) env7))
         (env9 (extend-env 'g (num-val 28) env8))
         (env10 (extend-env 'f (num-val 100) env9)))
    env10))


(define initial-env8 
  (let* ((env1 (extend-env 'a (num-val 20) (empty-env)))
         (env2 (extend-env 'b (num-val 1) env1))
         (env3 (extend-env 'c (num-val 2) env2))
         (env4 (extend-env 'd (num-val 33) env3))
         (env5 (extend-env 'j (num-val 67) env4)))
    
    env5))
        
; ---------------------------------------------------------------------------------------

;Beginnning of tests

; Tests for Q1 & Q2 & Q3 (Line 562 - 728)
; Tests for Q1 --> Line 562-621
; Tests for Q2 --> Line 625-632
; Tests for Q3 --> Line 635-721

(check-equal? (eval "( proc (g) (g 30) proc (y) -(y, 1))")
              (num-val 29))
 
(check-equal? (eval "if zero?(1) then 1 else 2")
              (num-val 2))
 
(check-equal? (eval "-(15, 10)")
              (num-val 5))
 
(check-equal? (eval "let x = 10 in if zero?(-(x, x)) then x else 2")
              (num-val 10))
 
(check-equal? (eval "let decr = proc (a) -(a, 1) in (decr 30)")
              (num-val 29))
 
(check-equal? (eval "( proc (g) (g 30) proc (y) -(y, 1))")
              (num-val 29))
 
(check-equal? (eval "let x = 200
                     in let f = proc (z) -(z, x) 
                        in let x = 100 
                           in let g = proc (z) -(z, x) 
                              in -((f 1), (g 1))")
              (num-val -100))
 
(check-equal? (eval "let sum = proc (x) proc (y) -(x, -(0, y)) in ((sum 3) 4)")
              (num-val 7))
 
(check-equal? (eval "let sum = proc (x) proc (y) -(x, -(0, y))
                     in letrec sigma (n) = if zero?(n)
                                           then 0
                                           else ((sum n) (sigma -(n, 1)))
                        in (sigma 5)")
              (num-val 15))
 
(check-equal? (eval "letrec even(n) = if zero?(n)
                                      then zero?(n)
                                      else if zero?(-(n, 1))
                                           then zero?(n)
                                           else (even -(n, 2))
                     in (even 501)")
              (bool-val #f))
 
 

(check-equal? (eval "let x = 4 in cons(x, cons( cons(-(x,1), emptylist), emptylist))")
              (list-val (num-val 4) (list-val (list-val (num-val 3) (emptylist-val))(emptylist-val))))


 

(check-equal? (eval "let x = 4 in list (x -(x,1) -(x,3))")
              (list-val (num-val 4) (list-val (num-val 3) (list-val (num-val 1) (emptylist-val)))))
 

(check-equal? (eval "list(1 2 3)")
              (list-val (num-val 1) (list-val (num-val 2) (list-val (num-val 3)(emptylist-val)))))


(check-equal? (eval "let x = 30 in let* x = -(x,1) y = -(x,2) in -(x,y)")
              (num-val 2))


(check-equal? (eval "let* x = 5 in -(x, 2)")
              (num-val 3))

(check-equal? (eval "let* x = 1 y = 1 in -(y,x)") (num-val 0))

(check-equal? (eval "let* x = 1 y = -(x,2) in -(y,x)") (num-val -2))

(check-equal? (eval "let* x = 1 y = -(x,1) z = -(y, -(40, x))  in -(z, -(x, y))") (num-val -40))


(check-equal? (free-var (empty-env) '() (const-exp 5)) (empty-env))

(check-equal? (free-var initial-env '(x) (var-exp 'x)) (empty-env)) 

(check-equal? (free-var initial-env2  '(x z) (diff-exp (var-exp 'x) (var-exp 'z))) (empty-env))

(check-equal? (free-var initial-env '(x)(zero?-exp (var-exp 'x))) (empty-env)) ; Case where all are bound (parameters)

(check-equal? (free-var initial-env '()(zero?-exp (var-exp 'x))) (extend-env 'x (num-val 2) (empty-env))) ; Case where all are free (not parameters)

(check-equal? (free-var initial-env3 '(x y z) (if-exp (var-exp 'x) (var-exp 'y) (var-exp 'z))) (empty-env)) ; Case where all are bound (parameters)

(check-equal? (free-var initial-env3 '() (if-exp (var-exp 'x) (var-exp 'y) (var-exp 'z))) (extend-env 'z (num-val 30) ; Case where all are free (not parameters)
                                                                                                      (extend-env 'y (num-val 10)
                                                                                                                  (extend-env 'x (num-val 2)
                                                                                                                              (empty-env)))))
(check-equal? (free-var initial-env4 '(x) (proc-exp '(x) (var-exp 'y))) (extend-env 'y (num-val 5) (empty-env)))
(check-equal? (free-var initial-env4 '(x y) (proc-exp '(x) (var-exp 'y)))  (empty-env))
(check-equal? (free-var initial-env4 '(y) (proc-exp '(x) (var-exp 'y)))  (empty-env))

(check-equal? (free-var initial-env2 '() (call-exp (var-exp'x) (list (const-exp 10)))) (extend-env 'x (num-val 2) (empty-env)))

(check-equal? (free-var initial-env4 '() (cons-exp (var-exp 'x) (var-exp 'y))) (extend-env 'y (num-val 5) (extend-env 'x (num-val 4) (empty-env))))
(check-equal? (free-var initial-env4 '(x) (cons-exp (var-exp 'x) (var-exp 'y))) (extend-env 'y (num-val 5)(empty-env)))
(check-equal? (free-var initial-env4 '(y) (cons-exp (var-exp 'x) (var-exp 'y))) (extend-env 'x (num-val 4) (empty-env)))
(check-equal? (free-var initial-env4 '(x y) (cons-exp (var-exp 'x) (var-exp 'y))) (empty-env))

(check-equal? (free-var initial-env '() (car-exp (var-exp 'x))) (extend-env 'x (num-val 2) (empty-env)))
(check-equal? (free-var initial-env '(x) (car-exp (var-exp 'x))) (empty-env))
(check-equal? (free-var initial-env '(y) (car-exp (var-exp 'x))) (extend-env 'x (num-val 2) (empty-env)))

(check-equal? (free-var initial-env5 '()(cdr-exp (var-exp 'y))) (extend-env 'y (bool-val #t) (empty-env)))
(check-equal? (free-var initial-env5 '(y)(cdr-exp (var-exp 'y))) (empty-env))
(check-equal? (free-var initial-env5 '(x)(cdr-exp (var-exp 'y))) (extend-env 'y (bool-val #t) (empty-env)))

(check-equal? (free-var initial-env '() (null?-exp (var-exp 'x))) (extend-env 'x (num-val 2) (empty-env)))
(check-equal? (free-var initial-env '(x) (null?-exp (var-exp 'x))) (empty-env))


(check-equal? (free-var initial-env4 '() (list-exp (list (var-exp 'x) (var-exp 'y)))) (extend-env 'y (num-val 5) (extend-env 'x (num-val 4) (empty-env))))

(check-equal? (free-var initial-env '() (emptylist-exp)) (empty-env))
(check-equal? (free-var initial-env2 '() (emptylist-exp)) (empty-env))
(check-equal? (free-var initial-env3 '() (emptylist-exp)) (empty-env))
(check-equal? (free-var initial-env4 '() (emptylist-exp)) (empty-env))
(check-equal? (free-var initial-env5 '() (emptylist-exp)) (empty-env))


(check-equal? (free-var initial-env3 '() (let-exp '(x y) (list(const-exp 5) (const-exp 6)) (var-exp 'z))) (extend-env 'z (num-val 30) (empty-env)))

(check-equal? (free-var initial-env3 '() (list-exp (list (var-exp 'x) (var-exp 'y)))) (extend-env 'y (num-val 10) (extend-env 'x (num-val 2) (empty-env))))

(check-equal? (free-var initial-env6 '() (let-exp '(x y z) (list (const-exp 5) (const-exp 6) (const-exp 7)) (var-exp 'w)))(extend-env 'w (bool-val #t) (empty-env)))


; Given test --> Produces free var with rescpect to f so produces the empty-env
(check-equal? (free-var initial-env7 '()   (let-exp '(f)
                                                    (list (proc-exp '(x y z)
                                                                    (let-exp '(g)
                                                                             (list (proc-exp '(a b)
                                                                                             (let-exp '(h)
                                                                                                      (list (proc-exp '(i j)
                                                                                                                      (diff-exp (var-exp 'x) 
                                                                                                                                (diff-exp (var-exp 'i) 
                                                                                                                                          (diff-exp (var-exp 'j) 
                                                                                                                                                    (var-exp 'a))))))
                                                                                                      (call-exp (var-exp 'h) 
                                                                                                                (list (var-exp 'z) 
                                                                                                                      (diff-exp (var-exp 'b) 
                                                                                                                                (var-exp 'a)))))))
                                                                             (call-exp (var-exp 'g) (list (const-exp 10) (const-exp 20))))))
                                                    (call-exp (var-exp 'f) (list (const-exp 1) (const-exp 2) (const-exp 3)))))
              (empty-env))


(check-equal?(free-var initial-env '()(let*-exp '(x y)(list (const-exp 5)(var-exp 'x))(var-exp 'y)))(empty-env))

(check-equal? (free-var initial-env3 '() (let*-exp '(x y) (list (var-exp 'z) (var-exp 'x)) (var-exp 'y))) (extend-env 'z (num-val 30) (empty-env)))       

(check-equal? (free-var initial-env '() (letrec-exp '(f g h) 
                                                    '((x) (y) (z)) 
                                                    (list (var-exp 'g) (var-exp 'h) (var-exp 'f)) 
                                                    (call-exp (var-exp 'f) (list (const-exp 10)))))
              (empty-env))

(check-equal?(free-var initial-env8 '() (letrec-exp '(f g)'((x) (y))(list (var-exp 'a) (letrec-exp'(h i)'((z) (w)) (list (var-exp 'b) (var-exp 'c))(var-exp 'j)))
                                                    (var-exp 'd)))
 
             (extend-env 'd (num-val 33) (extend-env 'j (num-val 67) (extend-env 'c (num-val 2) (extend-env 'b (num-val 1) (extend-env 'a (num-val 20) (empty-env)))))))



