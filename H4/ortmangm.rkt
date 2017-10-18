#lang racket
(require racket/trace)
(require "program.rkt")
(provide (all-defined-out))

(define (synchk expr)
  (cond
    [ (number? expr) true ]  ;; number
    
    [ (symbol? expr) true ]  ;; variable
    
    [ (and (equal? (car expr) 'var)   ;; first element is var
           (equal? 3 (length expr))) ;; there are three elements
      ;; then
      (and (> (length (cadr expr)) 0) ;; there must be at least one varassign
           (synchkvarassignseq (cadr expr)) ;; lets check the varassignseq
           (synchk (cadr (cdr expr)))) ]    ;; finally the expression at the end

    [ (and (equal? (car expr) 'fun) ;; first element is fun
           (equal? (length expr) 3)) ;; there are three elements
      
      (synchkfassign (cadr expr))
      (synchk (cadr (cdr expr)))]

    [(and (equal? (car expr) 'apply)
          (equal? (length expr) 2))

     (and (list? (cadr expr))
          (equal? (length (cadr expr)) 2))

     ;;(and (synchkfexpr (cadr expr))
          ;;(synchkfargs (cadr (cadr expr))))
     ]
      
    [ (and (arithop (car expr))        ;; first element is +, -, /, *
           (equal? 3 (length expr))) ;; there are three elements
      ;; then
      (and (synchk (cadr expr))
           (synchk (cadr (cdr expr)))) ]

    [ (and (list? (car expr))        ;; first element is a list
           (equal? 3 (length expr))) ;; there are three elements
      ;; then
      (and (synchkcond (car expr))   ;; it is a condition
           (synchk (cadr expr))      ;; validate the then expression
           (synchk (cadr (cdr expr)))) ] ;; validate the else expression

    [ else false ]))

(trace synchk)

(define (synchkfexpr expr) '())
(define (synchkfargs expr) '())

(define (synchkvarassignseq listofpairs)
  (if (null? listofpairs)
      true
      ;; else
      (and (equal? (length (car listofpairs)) 2)  ;; must be a pair
           (symbol? (car (car listofpairs)))      ;; first element in pair is variable
           (synchk (cadr (car listofpairs)))      ;; second element is an expression
           (synchkvarassignseq (cdr listofpairs)))))

(define (synchkfassign fassign)
  (if (null? fassign)
      true
      (and (symbol? (car (car fassign))) ;; must be a symbol 
           (list? (cadr (car fassign)))
           (synchkparamsargs (cadr (car fassign))) ;; synchk the params 
           (if (equal? (cadr fassign) 'apply)
               (synchkapply (cadr fassign) (car (cadr (cadr fassign))) (cadr (cadr (cadr fassign))))
               (synchk (cadr fassign))))))

;; function to synchk the arguments or parameters of fassign or apply 
(define (synchkparamsargs paramslist)
  (if (null? paramslist)
      true
      (if (synchk (car paramslist))
          (synchkparamsargs (cdr paramslist)) 
          false)))

;; function to synchk apply
(define (synchkapply expr fun paramlen)
  (if (and (equal? (car (cadr expr)) fun)
           (equal? (length (cadr (cadr expr)) paramlen)))
      (if (synchk (car (cdr expr)))
          (synchkparamsargs (cadr (cadr expr))) ;;synchk the arguments 
          false )
      false))
      
(define (synchkcond condition)
  (cond
    [ (and (or (equal? 'gt (car condition))
               (equal? 'lt (car condition))
               (equal? 'eq (car condition)))
           (equal? 3 (length condition)))
      (and (synchk (cadr condition))
           (synchk (cadr (cdr condition)))) ]
    [ (and (or (equal? 'and (car condition))
               (equal? 'or (car condition)))
           (equal? 3 (length condition)))
      (and (synchkcond (cadr condition))
           (synchkcond (cadr (cdr condition)))) ]
    [ (and (equal? 'not (car condition))
           (equal? 2 (length condition)))
      (synchkcond (cadr condition)) ]
    [ else false] ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval
;; input: program-expression, environment as list of pairs (variable value)
;; output: '(Cannot Evaluate) or a number

(define (eval expr env)
  (if (caneval env)      ;; the environment might end up with a pair (variable '(Cannot Evaluate))
      (cond
        [ (number? expr) expr ]  ;; number
        [ (symbol? expr) (findvalue expr env) ] ;; sigma(x)

        ;; implementation of the semantics of the function expression
        [ (equal? (car expr) 'fun) (eval (cadr (cdr expr)) (cons (cadr expr) env))]

        ;; implementation of the semantics of the apply expression
        [ (equal? (car expr) 'apply) (evalapply (cadr expr) env)]
        
        ;; implementation of the semantics of variable expression
        [ (equal? (car expr) 'var) (evalvarassign (cadr expr) (cadr (cdr expr)) env) ]
 
        ;; same as before with the arithmatic operations: environment is added
        [ (arithop (car expr)) (evalarith (car expr) (cadr expr) (cadr (cdr expr)) env) ]

        ;; ifthenelse function
        [ else  (ifthenelse (evalcond (car expr) env) 
                            (cadr expr)
                            (cadr (cdr expr)) env) ]
        )
      '(Cannot Evaluate)
      ))

(trace eval)

;; input: variable, environment
;; output: value to which the variable is mapped to in the environment
;;         It can be '(Cannot Evaluate) 
(define (findvalue x env)
  (if (null? env)  
      '(Cannot Evaluate)
      (if (equal? (car (car env)) x)
          (cadr (car env))
          (findvalue x (cdr env)))))

;; input: environment
;; output: true, if no variable is mapped to '(Cannot Evaluate)
;;         false, otherwise
;; Exercise: implement in another way, where it does not depend on '(Cannot Evaluate)
(define (caneval env)
  (if (null? env)
      true
      (and (not (equal? (cadr (car env)) '(Cannot Evaluate)))
           (caneval (cdr env)))))

;; input: list of (variable expression), expr to evaluate, environment
;; output: evaluate expr in some environment
(define (evalvarassign varassigns expr env)
  (if (null? varassigns)  ;; no variable expression pair, 
      (eval expr env)     ;; then just evaluate the expression in the current environment
      ;; else
      ;; recursive call with the suffix of varassigns, with the same expr
      ;; in the environment constructed by cons-ing (variable evaluation of expression)
      ;; to the given environment env.
      (evalvarassign (cdr varassigns)
                     expr
                     (cons (list (car (car varassigns))
                                 (eval (cadr (car varassigns)) env))
                           env))))

;; is op arithmatic operation
(define (arithop op)
  (or (equal? op '+)
      (equal? op '-)
      (equal? op '*)
      (equal? op '\))))

;; input: arithoperator, expr-operand1, expr-operand2, env
;; output: '(Cannot Evaluate) or some number
;; used: myapply 
(define (evalarith op expr1 expr2 env)
 (myapply op (eval expr1 env) (eval  expr2 env)))

;; input: true/false, '(Cannot Evaluate) expression values
;; output: '(Cannot Evaluate) or expression values
;;         expression values can be '(Cannot Evaluate)
(define (ifthenelse condition expr1 expr2 env)
  (if (equal? condition '(Cannot Evaluate))
      '(Cannot Evaluate)
      (if condition
          (eval expr1 env)
          (eval expr2 env))))

;; input: conditions of the form (gt/lt/eq expr1 expr2), (or/and cond1 cond2), (not cond)
;; output: true/false, '(Cannot Evaluate)
;; used: myapply
(define (evalcond condexpr env)
  (cond
    [ (equal? (car condexpr) 'gt)
      (myapply 'gt (eval (cadr condexpr) env) (eval (cadr (cdr condexpr)) env)) ]
    
    [ (equal? (car condexpr) 'lt)
      (myapply 'lt (eval (cadr condexpr) env) (eval (cadr (cdr condexpr)) env)) ]
    
    [ (equal? (car condexpr) 'and)
      (myapply 'and (evalcond (cadr condexpr) env)
               (evalcond (cadr (cdr condexpr)) env)) ]

    [ (equal? (car condexpr) 'or)
      (myapply 'or (evalcond (cadr condexpr) env)
               (evalcond (cadr (cdr condexpr)) env)) ]

    [ (equal? (car condexpr) 'not)
      (myapply 'not (evalcond (cadr condexpr) env)
               false) ] ;; dummy
    ))


;; input: some operator, arithmatic or conditional
;;        operand-values for the operator
;; output: '(Cannot Evaluate) or number or boolean 
(define (myapply op val1 val2)
  (if (or (equal? val1 '(Cannot Evaluate))
          (equal? val2 '(Cannot Evaluate)))
      '(Cannot Evaluate)
      (cond
        [ (equal? op '+) (+ val1 val2) ]
        [ (equal? op '-) (- val1 val2) ]
        [ (equal? op '*) (* val1 val2) ]
        [ (equal? op 'gt) (> val1 val2) ]
        [ (equal? op 'lt) (< val1 val2) ]
        [ (equal? op 'eq) (equal? val1 val2) ]
        [ (equal? op 'and) (and val1 val2) ]
        [ (equal? op 'or) (or val1 val2) ]
        [ (equal? op 'not) (not val1) ])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW 4
;; input: function name, arguments, envioronment
;; output: evaluation of the application of a function under the current environment 
;;(define (evalapply expr env)
;;  (if (null? (car expr)) ;; if there are no arguments
;;      (eval (findfunction (car expr) 0 env) env) ;; evaluate function under curr env
      ;; else 
;;      (eval (findfunction (car expr) (length (cadr (car expr))) (addargstoenv (car expr) (cadr (car expr)) env)) env))) ;; add arguments to env and evaluate 

(define (evalapply expr env)
  (eval (list 'var (paramarglist (findparams (car expr)
                                 (length (cadr expr))
                                 env)
                   (cadr expr)
                   env)
              (findfunction (car expr) (length (cadr expr)) env))
              (constructenv (car expr) (length (cadr expr)) env)))
(trace evalapply)

;; input function name, arg length, environment
;; output: function definition present in the environment
(define (findfunction fname arglen env)
  (if (and (list? (car (car env)))
               (equal? (car (car (car env))) fname)
               (equal? (length (cadr (car (car env)))) arglen))
          (cadr (car env)) ;; return function definition
          (findfunction fname arglen (cdr env)))) ;; keep going
(trace findfunction)

;; input: function name, argument length, environment
;; output: list of function parameters of function in env
(define (findparams fname arglen env)
  (if (and (list? (car (car env)))
           (equal? (car (car (car env))) fname)
           (equal? (length (cadr (car (car env)))) arglen))
      (cadr (car (car env))) ;; return function params
      (findparams fname arglen (cdr env)))) ;; keep going
(trace findparams)

;; construct env for apply evaluation
(define (constructenv fname arglen env)
  (if (and (list? (car (car env)))
           (equal? (car (car (car env))) fname)
           (equal? (length (cadr (car (car env)))) arglen))
      env
      (constructenv fname arglen (cdr env))))

;; input: list of parameters and arguments, env
;; output: list of parameters and evaluation of each argument
(define (paramarglist params args env )
  (if (null? params)
      '()
      (cons (list (car params) (eval (car args) env))
            (paramarglist (cdr params) (cdr args) env))))