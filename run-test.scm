(define tests '(

0
(define a 100)
a
(+ (- (* (* 1 2) 3) (* 4 5)) (+ (* 6 7) (* 8 9)))
(define f (lambda (x) (+ x 1)))
(f 100)
(define g (lambda (x) (if (< x a) a x)))
(g (f 100))
(define l (cons 1 (cons 2 (cons 3 '()))))
l
(car (cdr l))
(define b 1)
(let ((a 10) (b (f a))) (+ a b))
(let ((a 10) (b (f a)) (c (- b 1))) (+ a b c) (let ((a 10) (b (f a)) (c (- b 1))) (+ (+ a b) c)))
a
(define h (lambda (x) (lambda (y) (+ x (* a y)))))
(let ((a 1)) ((h 9) 9))
(define fact (lambda (n) (if (< n 1) 1 (* n (fact (- n 1))))))
(fact 10)
))

(load "./sush.scm")
(top-eval-write tests)
