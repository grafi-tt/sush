;; functions for initialize interpreter
(define (make-builtin-env)
  (let* ((env (make-empty-env))
         (def (lambda (sym meta-func)
                (define-var! env sym (obj-proc-builtin meta-func)))))
    (def 'if-func builtin-if-func)
    (def 'eqv? builtin-eqv?)
    (def 'eq? builtin-eq?)
    (def 'equal? builtin-equal?)
    (def 'number? builtin-number?)
    (def 'complex? builtin-complex?)
    (def 'real? builtin-real?)
    (def 'rational? builtin-rational?)
    (def 'integer? builtin-integer?)
    (def 'exact? builtin-exact?)
    (def 'inexact? builtin-inexact?)
    (def '= builtin-=)
    (def '< builtin-<)
    (def '> builtin->)
    (def '<= builtin-<=)
    (def '>= builtin->=)
    (def '+ builtin-+)
    (def '* builtin-*)
    (def '- builtin--)
    (def '/ builtin-/)
    (def 'quotient builtin-quotient)
    (def 'remainder builtin-remainder)
    (def 'modulo builtin-modulo)
    (def 'boolean? builtin-boolean?)
    (def 'pair? builtin-pair?)
    (def 'cons builtin-cons)
    (def 'car builtin-car)
    (def 'cdr builtin-cdr)
    (def 'set-car! builtin-set-car!)
    (def 'set-cdr! builtin-set-cdr!)
    env))

(define-syntax assert-arg
  (syntax-rules (any forall)
    ((_ stack arg any ((forall assert1)) body ...)
      (if (let loop ((arg arg))
            (if (null? arg) #t (and (assert1 (car arg)) (loop (cdr arg)))))
        (begin body ...)
        (goto-error stack "invalid argument")))
    ((_ stack arg any (assert1) body ...)
      (if (assert1 (car arg))
        (begin body ...)
        (goto-error stack "invalid argument")))
    ((_ stack arg any (assert1 assert2 ...) body ...)
      (if (assert1 (car arg))
        (assert-arg stack arg any (assert2 ...) body ...)
        (goto-error stack "invalid argument")))
    ((_ stack arg any dummy body ...) (begin body ...))
    ((_ stack arg arity asserts body ...)
      (if (= (length arg) arity)
        (assert-arg stack arg any asserts body ...)
        (goto-error "invalid arity")))))

; dirty way to check whether object is #f or not
(define (builtin-if-func stack . args)
  (assert-arg stack args 3 ()
    (continue stack (if (car args) (cadr args) (caddr args)))))

; dirty way to compare object, but it works, since all object is S expr
(define (builtin-eqv? stack . args)
  (assert-arg stack args 2 ()
    (continue stack (eqv? (car args) (cadr args)))))

(define (builtin-eq? stack . args)
  (assert-arg stack args 2 ()
    (continue stack (eq? (car args) (cadr args)))))

(define (builtin-equal? stack . args)
  (assert-arg stack args 2 ()
    (continue stack (equal? (car args) (cadr args)))))

(define (builtin-number? stack . args)
  (assert-arg stack args 1 ()
    (continue stack (obj-number? (car args)))))

(define (builtin-complex? stack . args)
  (assert-arg stack args 1 ()
    (continue stack
      (and (obj-number? (car args))
           (complex? (car args))))))

(define (builtin-real? stack . args)
  (assert-arg stack args 1 ()
    (continue stack
      (and (obj-number? (car args))
           (real? (car args))))))

(define (builtin-rational? stack . args)
  (assert-arg stack args 1 ()
    (continue stack
      (and (obj-number? (car args))
           (rational? (car args))))))

(define (builtin-integer? stack . args)
  (assert-arg stack args 1 ()
    (continue stack
      (and (obj-number? (car args))
           (integer? (car args))))))

(define (builtin-exact? stack . args)
  (assert-arg stack args 1 ()
    (continue stack
      (and (obj-number? (car args))
           (exact? (car args))))))

(define (builtin-inexact? stack . args)
  (assert-arg stack args 1 ()
    (continue stack
      (and (obj-number? (car args))
           (inexact? (car args))))))

(define (builtin-= stack . args)
  (assert-arg stack args any ((forall obj-number?))
    (continue stack (apply = args))))

(define (builtin-< stack . args)
  (assert-arg stack args any ((forall obj-number?))
    (continue stack (apply < args))))

(define (builtin-> stack . args)
  (assert-arg stack args any ((forall obj-number?))
    (continue stack (apply > args))))

(define (builtin-<= stack . args)
  (assert-arg stack args any ((forall obj-number?))
    (continue stack (apply <= args))))

(define (builtin->= stack . args)
  (assert-arg stack args any ((forall obj-number?))
    (continue stack (apply >= args))))

(define (builtin-+ stack . args)
  (assert-arg stack args any ((forall obj-number?))
    (continue stack (apply + args))))

(define (builtin-- stack . args)
  (assert-arg stack args any ((forall obj-number?))
    (continue stack (apply - args))))

(define (builtin-* stack . args)
  (assert-arg stack args any ((forall obj-number?))
    (continue stack (apply * args))))

(define (builtin-/ stack . args)
  (assert-arg stack args any ((forall obj-number?))
    (continue stack (apply / args))))

(define (builtin-quotient stack . args)
  (assert-arg stack args 2 ((forall obj-number?))
    (continue stack (apply quotient args))))

(define (builtin-remainder stack . args)
  (assert-arg stack args 2 ((forall obj-number?))
    (continue stack (apply remainder args))))

(define (builtin-modulo stack . args)
  (assert-arg stack args 2 ((forall obj-number?))
    (continue stack (apply modulo args))))

(define (builtin-boolean? stack . args)
  (assert-arg stack args 1 ()
    (continue stack (obj-boolean? (car args)))))

(define (builtin-pair? stack . args)
  (assert-arg stack args 1 ()
    (continue stack (obj-pair? (car args)))))

(define (builtin-cons stack . args)
  (assert-arg stack args 2 ()
    (continue stack (obj-pair (car args) (cadr args)))))

(define (builtin-car stack . args)
  (assert-arg stack args 1 (obj-pair?)
    (continue stack (car (raw-obj (car args))))))

(define (builtin-cdr stack . args)
  (assert-arg stack args 1 (obj-pair?)
    (continue stack (cdr (raw-obj (car args))))))

(define (builtin-set-car! stack . args)
  (assert-arg stack args 2 (obj-pair?)
    (set-car! (raw-obj (car args)))
    (continue stack (obj-undefined))))

(define (builtin-set-cdr! stack . args)
  (assert-arg stack args 2 (obj-pair?)
    (set-cdr! (raw-obj (car args)))
    (continue stack (obj-undefined))))


;; base library
(define base-library-exprs '(
(define-syntax if
  (syntax-rules ()
    ((_ expr1 expr2 expr3)
     ((if-func expr1 (lambda () expr2) (lambda () expr3))))))
(define-syntax let
  (syntax-rules ()
    ((_ ((name val) ...) body ...)
     ((lambda (name ...) body ...) val ...))))
))
