#lang racket
(provide (all-defined-out))

;; synchk: false 1
(define prog1
  '(var ((x (fun ((f (x)) x) 1))) (apply (f (1)))))

;; synchk: false 1
(define prog2
  '(var ((x (fun ((f (x y)) (+ x y)) (apply (f (1)))))) x))

;; synchk: false 1
(define prog3
  '((gt (fun ((f x) x) (apply (f (1))))
        1)
    1 2))

;; synchk: false 1
(define prog4  
  '((gt (fun ((f (x)) x) (apply (f (1))))
        (apply (f (2))))
    1 2))

;; synchk: true 1
;; eval: 2
;; (eval prog5 '()) = 2
(define prog5
  '(fun ((f (x)) x) (apply (f ((apply (f (2))))))))

;; synchk: false 1
(define prog6
  '(fun ((f ((apply (f (1))))) 1) (apply (f (2)))))

;; synchk: true 1
;; eval: 2pts
;; (eval prog7 '()) = 106 
(define prog7
  '(var ((z 1))
        (fun ((f (x)) x)
             (var ((z 2))
                  (fun ((f (x y)) (+ x y))
                        (apply (f ((+ z (apply (f (z (+ z 100)))))))))))))

;; synchk: true 1
;; eval: 2pts
;; (eval prog8 '()) = 3
(define prog8
  '(var ((z 1))
        (fun ((f ()) z)
             (var ((z 2))
                  (fun ((f (x y)) (+ x y))
                       (apply (f (z (apply (f ()))))))))))
;; synchk: true 1
;; eval: 2pts
;; (eval prog9 '()) = 5
(define prog9
  '(var ((z 1))
        (fun ((f (x)) (+ x z))
             (+ (var ((z 2)) (apply (f (z))))
                (apply (f (z)))))))

;; synchk: 1
;; eval: 1
;(eval prog10 '((y 10))) = 12
(define prog10
  '(fun ((f (a b)) (var ((x a) (y b)) (+ x y))) (apply (f (y 2)))))

;; synchk: 1
;; eval: 1
;(eval prog11 '()) = 2
(define prog11
  '(var ((x 1))
        (fun ((f (x)) x)
             (fun ((g ()) (var ((x (+ x 1))) (apply (f (x)))))
                  (apply (g ()))))))

;; synchk: 1
;; eval: 1
; (eval prog12 '()) = 1
(define prog12
  '(var ((x 1))
        (fun ((f ()) x)
             (fun ((g ()) (var ((x (+ x 1))) (apply (f ()))))
                  (apply (g ()))))))

;; synchk: 1
;; eval: 1
; (eval prog13 '((x 10))) = 55
(define prog13
  '(fun ((f (n))
         ((eq n 0) 0 ((eq n 1) 1 (+ (apply (f ((- n 1)))) (apply (f ((- n 2)))))))
         )
        (apply (f (x)))))

;; synchk: 1
;; eval: 1
; (eval prog14 '((x 10))) = 3628800
(define prog14
  '(fun ((f (n a))
         ((eq n 0) a (apply (f ((- n 1) (* n a))))))
        (fun ((g (n)) (apply (f (n 1))))
             (apply (g (x))))))

;; synchk: 1
;; eval: 2
;; (eval prog15 '()) = 24
(define prog15
  '(var ((x 100))
        (+ ((eq (fun ((f (x)) (var ((x 10)) x)) (apply (f (100)))) 100)
            3
            4)
           20)))
                