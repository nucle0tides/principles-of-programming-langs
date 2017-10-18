#lang racket
(require "program.rkt")
(provide (all-defined-out))
(require racket/trace) ;; in case you want to use tracing

;; Adapted from class - 02/07/2017

(define (eval expr env)
  (cond  
        [ (number? expr) expr ]   ;; semantics of a number is the number itself

        [ (symbol? expr) (getval expr env) ]
 
        [ (and (list? expr) (arithop (car expr)))   ;; if the expression is an arithexpression then
          (evalarith (car expr) (cadr expr) (cadr (cdr expr)) env) ]  ;; use evalarith to apply the operator on the evaluation of the operands
    
        [ (and (list? expr) (list? (car expr)))     ;; if the expression is an ifexpression then 
          (if (evalcond (car expr) env)                               ;; use evalcond to evaluate the condition (to true/false)
              (eval (cadr expr) env)                                  ;; based on that evaluate the "then-branch" or the "else-branch"
              (eval (cadr (cdr expr)) env))]
    
        [ (and (equal? 'var (car expr))
               (pair? (cadr expr))
               (not (empty? (cadr expr))))
          (eval (cadr (cdr expr)) (consnewenv (cadr expr) env))] ;; evaluate and add to the environment
        ))

(define (arithop op)
  (or (equal? op '+)
      (equal? op '-)
      (equal? op '*)
      (equal? op '/)))

(define (evalarith op operand1 operand2 env)
  (cond
    [ (equal? op '+) (+ (eval operand1 env) (eval operand2 env)) ]
    [ (equal? op '-) (- (eval operand1 env) (eval operand2 env)) ]
    [ (equal? op '*) (* (eval operand1 env) (eval operand2 env)) ]
    [ (equal? op '/) (/ (eval operand1 env) (eval operand2 env)) ]
    ))

(define (evalcond condexpr env)
  (cond
    [ (equal? (car condexpr) 'gt)
      (> (eval (cadr condexpr) env)
         (eval (cadr (cdr condexpr)) env)) ] ;; greater than 
    
    [ (equal? (car condexpr) 'lt)
      (< (eval (cadr condexpr) env)
         (eval (cadr (cdr condexpr)) env)) ] ;; less than 

    [ (equal? (car condexpr) 'eq)
      ((equal? (eval (cadr condexpr) env)
               (eval (cadr (cdr condexpr))) env)) ] ;; equal to
    
    [ (equal? (car condexpr) 'and)
      (and (eval (cadr condexpr) env)
          (eval (cadr (cdr condexpr))env)) ] ;; && 

    [ (equal? (car condexpr) 'or) 
      (or (eval (cadr condexpr) env)
          (eval (cadr (cdr condexpr))) env) ] ;; || 

    [ (equal? (car condexpr) 'not)
      (not (eval (cadr condexpr) env))] ;; ! 
    ))

;; function to get the environment of a variable
(define (getval v env)
  (if (null? env)
      '"cannot evaluate"
      (if (not (pair? (car env)))
      '"cannot evaluate"
      (if (equal? v (car(car env)))
          (cadr (car env))
          (getval v (cdr env))))))

(define (consnewenv lstofpairs env)
  (if (equal? 1 (length lstofpairs))
      (cons (cons (car (car lstofpairs)) (list (eval (cadr (car lstofpairs)) env))) env) ;; base case
      (cons (cons (car (car lstofpairs)) (list (eval (cadr (car lstofpairs)) env))) ;; recursive case
            (consnewenv (cdr lstofpairs) env))))

;;synchk and eval are almost identical 
(define (synchk prog)
  (cond
    [ (number? prog) #t ]
    [ (symbol? prog) #t ]
    [ (and (list? prog) (arithop (car prog)) (equal? 3 (length prog))) #t ]
    [ (and (list? prog) (list? (cdr prog)))     ;; if the expression is an ifexpression then 
          (if (synchkcond prog)                               ;; use evalcond to evaluate the condition (to true/false)
              (synchk (cadr prog))                                  ;; based on that evaluate the "then-branch" or the "else-branch"
              (synchk (cadr (cdr prog))))] 
    [ (and (equal? 'var (car prog))
               (pair? (cadr prog))
               (not (empty? (cadr prog))))
          (synchk (cadr (cdr prog)))]
    [ else #f ]
    ))

(define (synchkcond prog)
  (cond
    [ (and (equal? (car prog) 'gt) (equal? 2 (length prog)))
      (and (synchk (cadr prog))
           (synchk (cadr (cdr prog))))]
    [ (and (equal? (car prog) 'lt) (equal? 2 (length prog)))
      (and (synchk (cadr prog))
           (synchk (cadr (cdr prog))))]
    [ (and (equal? (car prog) 'and) (equal? 2 (length prog)))
      (and (synchk (cadr prog))
           (synchk (cadr (cdr prog))))]
    [ (and (equal? (car prog) 'or) (equal? 2 (length prog)))
      (and (synchk (cadr prog))
           (synchk (cadr (cdr prog))))]
    [ else #f ]
    ))
(trace synchk)