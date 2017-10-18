#lang racket
(require racket/trace)
(provide (all-defined-out))

(define (reftype prog)
  (checkref prog '()))

(define (checkref expr env)
    (cond
    [ (equal? expr true) true ]
    [ (equal? expr false) false ]
    ;; numbers do not have a specification...so false?
    [ (number? expr) false ]
    ;; is this variable being referenced?
    [ (symbol? expr) (findvalue expr env) ]
    ;; the ref type of a var is true iff it is assigned to some expr whose ref type is true
    [ (equal? (car expr) 'var) (refvarassigns (cadr expr) (cadr (cdr expr)) env) ]
    ;; the reference of an apply is true iff the ref type of the def of the fun being applied is true
    [ (equal? (car expr) 'fun) (checkref (cadr (cdr expr)) (cons (cadr expr) env))]
    [ (equal? (car expr) 'apply) (checkref (list 'var
                                                (paramarglist (findparams (car (cadr expr)) (length (cadr (cadr expr))) env) (cadr (cadr expr)) env)
                                                (findfunction (car (cadr expr)) (length (cadr (cadr expr))) env))
                                             (constructenv (car (cadr expr)) (length (cadr (cadr expr))) env)) ]
    ;; the ref type of ref expr is true
    [ (equal? (car expr) 'ref) true ]
    ;; the ref type of free expr is true
    [ (equal? (car expr) 'free) true ]
    ;; the ref type of deref expr is false
    [ (equal? (car expr) 'deref) true ]
    ;; the ref type of wref is false
    [ (equal? (car expr) 'wref) true ]
    ;; the reference type of an arithmetic expr is true iff the ref type of at least one operand is true
    [ (arithop (car expr)) (or (checkref (cadr expr) env)
                               (checkref (cadr (cdr expr)) env)) ]
    ;; the ref type of a conditional expr is true iff the ref type of at least one of th then expr or else expr is tue
    [ else (or (checkref (cadr expr) env) (checkref (cadr (cdr expr)) env)) ]
    )
  )

(trace checkref)

;; okay just take the necessary functions from HW4 and Go For It
(define (findvalue x env)
  (if (null? env)  
      false
      (if (equal? (car (car env)) x)
          (cadr (car env))
          (findvalue x (cdr env)))))

(define (arithop op)
  (or (equal? op '+)
      (equal? op '-)
      (equal? op '*)
      (equal? op '/)))

(define (refvarassigns varassigns expr env)
  (if (null? varassigns)   
      (checkref expr env)    
      (refvarassigns (cdr varassigns)
                     expr
                     (cons (list (car (car varassigns))
                                 (checkref (cadr (car varassigns)) env))
                           env))))

(define (findfunction fname arglen env)
  (if (and (list? (car (car env)))
               (equal? (car (car (car env))) fname)
               (equal? (length (cadr (car (car env)))) arglen))
          (cadr (car env))
          (findfunction fname arglen (cdr env))))

(define (findparams fname arglen env)
  (if (and (list? (car (car env)))
           (equal? (car (car (car env))) fname)
           (equal? (length (cadr (car (car env)))) arglen))
      (cadr (car (car env))) 
      (findparams fname arglen (cdr env))))

(define (constructenv fname arglen env)
  (if (and (list? (car (car env)))
           (equal? (car (car (car env))) fname)
           (equal? (length (cadr (car (car env)))) arglen))
      env
      (constructenv fname arglen (cdr env))))

(define (paramarglist params args env )
  (if (null? params)
      '()
      (cons (list (car params) (checkref (car args) env))
            (paramarglist (cdr params) (cdr args) env))))

#|
Does the reference-type depend on the static and dynamic scoping semantics of function? Justify
your response.

    Yes. For a functon like def f(x) (+ x 10), which would be part of a larger program, to evaluate #t,
the function parameter would need to be called by 'ref' to have #t added to the environment once f is called. 

|#