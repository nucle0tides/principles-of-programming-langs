#lang racket
(require racket/trace)
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval
;; input: program-expression, environment as list of pairs (variable value)
;; output: '(Cannot Evaluate) or a number

;; ask about chaining heap into all of these commands
(define (eval expr env heap)
  (if (caneval env)      ;; the environment might end up with a pair (variable '(Cannot Evaluate))
      (cond
        [ (number? expr) (list expr heap) ]  ;; number
        [ (symbol? expr) (list (findvalue expr env) heap)] ;; sigma(x)

        ;; implementation of the semantics of variable expression
        [ (equal? (car expr) 'var) (evalvarassign (cadr expr) (cadr (cdr expr)) env heap) ]

        ;; implementation of the semantics of FExpr
        [ (equal? (car expr) 'fun) (eval (cadr (cdr expr)) (cons (cadr expr) env) heap) ]

        ;; implementation of the semantics of Apply
        ;; utter disregard for efficiency - searching the same environment three times using the same key
        ;; But this keeps it clean
        ;; We are going to translate the apply semantics to varexpression semantics
        ;; [[(apply (f (ArgList)))]]_env = [[(var Param-ArgValue list  FDef]]_staticenv
        [ (equal? (car expr) 'apply) (list (car (eval (list 'var
                                                 (paramargs (findfunparams (car (cadr expr))  ;; function name
                                                                           (length (cadr (cadr expr))) ;; number of params
                                                                           env)              ;; current environment
                                                            ;; findfunparams returns parameters of the function
                                                            (cadr (cadr expr)) ;; expressions representing arguments
                                                            env heap) 
                                                 ;; paramargs returns the list of variable-value pairs
                                                 
                                                 (findfundef (car (cadr expr)) (length (cadr (cadr expr))) env)) ;; definition of the function
                                           
                                           (staticenv (car (cadr expr)) (length (cadr (cadr expr))) env) ;; static environment
                                           heap
                                           ))
                                           (cadr (eval (list 'var
                                                 (paramargs (findfunparams (car (cadr expr))  ;; function name
                                                                           (length (cadr (cadr expr))) ;; number of params
                                                                           env)              ;; current environment
                                                            ;; findfunparams returns parameters of the function
                                                            (cadr (cadr expr)) ;; expressions representing arguments
                                                            env heap) 
                                                 ;; paramargs returns the list of variable-value pairs
                                                 
                                                 (findfundef (car (cadr expr)) (length (cadr (cadr expr))) env)) ;; definition of the function
                                           
                                           (staticenv (car (cadr expr)) (length (cadr (cadr expr))) env) ;; static environment
                                           heap
                                           )))
                                           ]

        [ (equal? (car expr) 'ref) (ref  (cadr expr) env heap)]

        [ (equal? (car expr) 'deref) (deref (cadr expr) env heap) ]

        [ (equal? (car expr) 'wref) (wref (cadr expr) (cadr (cdr expr)) env heap) ]

        [ (equal? (car expr) 'free) (free (cadr expr) env heap) ]
        
        ;; same as before with the arithmatic operations: environment is added
        [ (arithop (car expr)) (evalarith (car expr) (cadr expr) (cadr (cdr expr)) env heap) ]

        ;; ifthenelse function
        [ else  (ifthenelse (evalcond (car expr) env heap) 
                            (cadr expr)
                            (cadr (cdr expr)) env heap) ]
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

(trace findvalue)

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
;; FIX THIS
(define (evalvarassign varassigns expr env heap)
  (if (null? varassigns)  ;; no variable expression pair, 
      (eval expr env heap)    ;; then just evaluate the expression in the current environment
      ;; else
      ;; recursive call with the suffix of varassigns, with the same expr
      ;; in the environment constructed by cons-ing (variable evaluation of expression)
      ;; to the given environment env.
      (evalvarassign (cdr varassigns)
                     expr
                     (cons (list (car (car varassigns)) (car (eval (cadr (car varassigns)) env heap))) env)
                     (cadr (eval (cadr (car varassigns)) env heap)))))
(trace evalvarassign)

;; is op arithmatic operation
(define (arithop op)
  (or (equal? op '+)
      (equal? op '-)
      (equal? op '*)
      (equal? op '\))))

;; input: arithoperator, expr-operand1, expr-operand2, env
;; output: '(Cannot Evaluate) or some number
;; used: myapply 
(define (evalarith op expr1 expr2 env heap)
 (myapply op (eval expr1 env heap) (eval  expr2 env (cadr (eval expr1 env heap)))))

;; input: true/false, '(Cannot Evaluate) expression values
;; output: '(Cannot Evaluate) or expression values
;;         expression values can be '(Cannot Evaluate)
(define (ifthenelse condition expr1 expr2 env heap)
  (if (equal? condition '(Cannot Evaluate))
      '(Cannot Evaluate)
      (if (equal? condition '(exception ooma))
          '(exception ooma)
          (if (equal? condition '(exception oom))
              '(exception oom)
              (if (equal? condition '(exception fma))
                  '(exception fma)
                  (if condition
                      (eval expr1 env heap)
                      (eval expr2 env heap))))))) ;;beautiful code, really

(trace ifthenelse)

;; input: conditions of the form (gt/lt/eq expr1 expr2), (or/and cond1 cond2), (not cond)
;; output: (true/false heap), '(Cannot Evaluate)
;; used: myapply
;; fix?
(define (evalcond condexpr env heap)
  (if (list? condexpr)
      (cond
        [ (equal? (car condexpr) 'gt)
          (myapply 'gt (eval (cadr condexpr) env heap) (eval (cadr (cdr condexpr)) env heap)) ] ;;eval first argument, then to get second heap, eval first arg again 
    
        [ (equal? (car condexpr) 'lt)
          (myapply 'lt (eval (cadr condexpr) env heap) (eval (cadr (cdr condexpr)) env heap)) ]

        [ (equal? (car condexpr) 'eq)
          (myapply 'eq (eval (cadr condexpr) env heap) (eval (cadr (cdr condexpr)) env heap)) ]
    
        [ (equal? (car condexpr) 'and)
          (myapply 'and (evalcond (cadr condexpr) env heap)
                   (evalcond (cadr (cdr condexpr)) env heap)) ]

        [ (equal? (car condexpr) 'or)
          (myapply 'or (evalcond (cadr condexpr) env heap)
                   (evalcond (cadr (cdr condexpr)) env heap)) ]

        [ (equal? (car condexpr) 'not)
          (myapply 'not (evalcond (cadr condexpr) env heap)
                   false) ] ;; dummy
        )
      '(list expr heap)))


;; input: some operator, arithmatic or conditional
;;        operand-values for the operator
;; output: '(Cannot Evaluate) or number or boolean 
(define (myapply op val1 val2)
  (if (or (equal? (car val1) '(Cannot Evaluate))
          (equal? (car val2) '(Cannot Evaluate)))
      '(Cannot Evaluate)
      (if (or (equal? (car val1) '(exception ooma))
              (equal? (car val2) '(exception ooma)))
          '(exception ooma)
          (if (or (equal? (car val1) '(exception oom))
              (equal? (car val2) '(exception oom)))
          '(exception oom)
          (if (or (equal? (car val1) '(exception fma))
              (equal? (car val2) '(exception fma)))
          '(exception fma)
          (cond
            [ (equal? op '+) (list (+ (car val1) (car val2)) (cadr val2)) ]
            [ (equal? op '-) (list (- (car val1) (car val2)) (cadr val2)) ]
            [ (equal? op '*) (list (* (car val1) (car val2)) (cadr val2)) ]
            [ (equal? op 'gt) (list (> (car val1) (car val2)) (cadr val2)) ]
            [ (equal? op 'lt) (list (< (car val1) (car val2)) (cadr val2)) ]
            [ (equal? op 'eq) (list (equal? (car val1) (car val2)) (cadr val2)) ]
            [ (equal? op 'and) (list (and (car val1) (car val2)) (cadr val2)) ]
            [ (equal? op 'or) (list (or (car val1) (car val2)) (cadr val2)) ]
            [ (equal? op 'not) (list (not (car val1)) (cadr val1)) ]))))))

(trace myapply)

;; Functions added for the assignment 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (paramargs (x1 x2 .... xn) (e1 e2 .... en) env) = ((x1 e1val) (x2 e2val) ... (xn enval))
;; input: list of variables
;;        list of expressions
;;        environment
;; output: list of pairs of variable-expressionvalue
(define (paramargs paramlist exprlist env heap)
  (if (null? paramlist)
      '()
      (cons (list (car paramlist) (eval (car exprlist) env heap))
            (paramargs (cdr paramlist) (cdr exprlist) env heap))))

;; find the function parameters
;; input: function name and arg-length
;;        env
;; output: list of function parameters
(define (findfunparams fname paramlength env)
  (if (and (list? (car (car env)))   ;; is a function definition
           (equal? (car (car (car env))) fname)  ;; name matches
           (equal? (length (cadr (car (car env)))) paramlength)) ;; paramlength matchs
      ;; 
      (cadr (car (car env)))   ;; return the list of parameters
      (findfunparams fname paramlength (cdr env)))) ;; else keep looking

;; Same as above: just return the definition of the function
(define (findfundef fname paramlength env)
  (if (and (list? (car (car env)))
           (equal? (car (car (car env))) fname)
           (equal? (length (cadr (car (car env)))) paramlength))
      ;; 
      (cadr (car env)) ;; return the definition of the function
      (findfundef fname paramlength (cdr env)))) ;; else keep looking

;; Given an environment; generate the static environment corresponding
;; for a function
;; same as above again
(define (staticenv fname paramlength env)
  (if (and (list? (car (car env)))
           (equal? (car (car (car env))) fname)
           (equal? (length (cadr (car (car env)))) paramlength))
      env ;; return the environment at the time of definition
      (staticenv fname paramlength (cdr env)))) ;; else keep looking

;; Functions added for assignment 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (deref expr env heap)
  ;; return value from a location in the heap
  (if (equal? (readfromlocus (car (eval expr env heap)) (cadr (eval expr env heap))) '(exception fma))
      (list '(exception fma) heap)
      (if (equal? (readfromlocus (car (eval expr env heap)) (cadr (eval expr env heap))) '(exception ooma))
          (list '(exception ooma) heap)
          (cons (readfromlocus (car (eval expr env heap)) (cadr (eval expr env heap))) (list (cadr (eval expr env heap)))))))

(define (wref locus expr env heap)
  ;; write at a location a value and return that value
  (if (equal?
       ;; expr op locus heap
       (car (writetolocus (car (eval expr env heap)) 'wref (car (eval locus env heap)) (cadr (eval expr env heap))))
       '(exception ooma))
      (list '(exception ooma) heap)
      (if (equal?
           (car (writetolocus (car (eval expr env heap)) 'wref (car (eval locus env heap)) (cadr (eval expr env heap))))
           '(exception fma))
          (list '(exception fma) heap)
           (cons (car (eval expr env heap)) (list (writetolocus (car (eval expr env heap)) 'wref (car (eval locus env heap)) (cadr (eval expr env heap))))))))

(define (ref expr env heap)
  ;; write to first free location the evaluation of expr and return that location with the heap
  (if (equal? (findfreelocus heap) '(exception oom))
      (list '(exception oom) heap)
      (cons (findfreelocus (cadr (eval expr env heap))) (list (writetolocus (car (eval expr env heap)) 'ref (findfreelocus (cadr (eval expr env heap))) (cadr (eval expr env heap)))))))
(trace ref)

(define (free expr env heap)
    (if (equal?
       ;; expr op locus heap
       (car (writetolocus (car (eval expr env heap)) 'free (car (eval expr env heap)) (cadr (eval expr env heap))))
       '(exception ooma))
      (list '(exception ooma) heap)
      (if (equal?
           (car (writetolocus (car (eval expr env heap)) 'free (car (eval expr env heap)) (cadr (eval expr env heap))))
           '(exception fma))
          (list '(exception fma) heap)
           (cons (car (eval expr env heap)) (list (writetolocus (car (eval expr env heap)) 'free (car (eval expr env heap)) (cadr (eval expr env heap))))))))

(trace free)

;; return the first free location if there is one
;; otherwise oom exception 
(define (findfreelocus heap)
  (if (null? heap)
      '(exception oom)
      (if (equal? (cadr (car heap)) 'free)
          (car (car heap))
          (findfreelocus (cdr heap)))))

(trace findfreelocus)

(define (writetolocus expr op locus heap) ;; expr == value
  (cond
    [(equal? op 'ref) 
     (if (equal? (findfreelocus heap) '(exception oom))
         (list '(exception oom) heap)
         (if (equal? (car (car heap)) locus) ;; at the correct location
             (cons (list (car (car heap)) expr) (cdr heap))
             (cons (car heap) (writetolocus expr op locus (cdr heap)))))]
    [(equal? op 'wref)
     (if (equal? (readfromlocus locus heap) '(exception ooma))
         (list '(exception ooma) heap)
         (if (equal? (readfromlocus locus heap) '(exception fma))
             (list '(exception fma) heap)
             (if (equal? (car (car heap)) locus)
                 (cons (list (car (car heap)) expr) (cdr heap))
                 (cons (car heap) (writetolocus expr 'wref locus heap)))))]
    [(equal? op 'free)
     (if (equal? (readfromlocus locus heap) '(exception ooma))
         (list '(exception ooma) heap)
         (if (equal? (readfromlocus locus heap) '(exception fma))
             (list '(exception fma) heap)
             (if (equal? (car (car heap)) locus)
                 (cons (list (car (car heap)) 'free) (cdr heap))
                 (cons (car heap) (writetolocus expr 'free locus heap)))))]))
      

(trace writetolocus)

(define (readfromlocus locus heap)
  (if (null? heap)
      '(exception ooma)
      (if (and (equal? locus (car (car heap)))
               (not (equal? (cadr (car heap)) 'free)))
          (cadr (car heap))
          (if (and (equal? locus (car (car heap)))
                   (equal? (cadr (car heap)) 'free))
              '(exception fma)
              (readfromlocus locus (cdr heap))))))

(trace readfromlocus)          
                     
  