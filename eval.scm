(define keywords '(quote quasiquote define define-syntax lambda syntax-rules))

(define (eval-expr stack expr)
  (let ((expanded (try-expand-macro stack expr)))
    (if expanded
      (begin (set-car! expr (car expanded))
             (set-cdr! expr (cdr expanded)))))
  (cond ((constant? expr) (continue stack expr))
        ((symbol? expr) (eval-symbol stack expr))
        ((symbol-hold-env? expr) (eval-symbol-hold-env stack expr))
        ((not (pair? expr)) (goto-error stack "not evaluatable constant"))
        ((memq (car expr) keywords)
          (let ((keyword (car expr)))
            (cond ((eq? keyword 'quote) (continue stack (quote-expr (cadr expr))))
                  ((eq? keyword 'quasiquote) (eval-quasiquote stack (cadr expr)))
                  ((eq? keyword 'define) (eval-define stack (cdr expr)))
                  ((eq? keyword 'define-syntax) (eval-define stack (cdr expr)))
                  ((eq? keyword 'lambda) (eval-lambda stack (cdr expr)))
                  ((eq? keyword 'syntax-rules) (eval-syntax-rules stack (cdr expr))))))
          (else (eval-procedure stack expr))))

(define (print-data val)
  (display (show val)))

(define (eval-symbol stack sym)
  (let ((obj (lookup-env (env-stack stack) sym)))
    (if obj
      (continue stack obj)
      (goto-error stack "not binded symbol"))))

(define (eval-symbol-hold-env stack sym-hold-env)
  (let* ((var (cadr sym-hold-env))
         (renamed-var (string->symbol (string-append "*renamed*-" (symbol->string var)))))
    (let ((val (or
                 (lookup-env (env-stack stack) renamed-var)
                 (lookup-env (cddr sym-hold-env) var))))
      (if val
        (continue stack val)
        (goto-error stack "not binded symbol holding environment of macro definition")))))

(define (eval-quasiquote stack expr)
  (goto-error stack "quasiquote is currently not supported"))

(define (eval-define stack expr)
  (if (= (length expr) 2)
    (eval-expr
      (cons-stack (stack-frame-define (car expr)) stack) (cadr expr))
    (goto-error stack "invalid define form")))

(define (eval-lambda stack expr)
  (continue stack (obj-proc-lambda (env-stack stack) expr)))

(define (eval-syntax-rules stack expr)
  (continue stack (obj-macro (env-stack stack) expr)))

(define (eval-procedure stack expr)
  (eval-expr
    (cons-stack (stack-frame-func (cdr expr)) stack) (car expr)))

; functions for consuming evaluated values
(define (continue stack . val)
  (if (empty-stack? stack)
    (car val)
    (let ((head (head-stack stack))
          (tail (tail-stack stack)))
      (cond ((stack-frame-func? head)
             (continue-func (cdr head) tail (car val)))
            ((stack-frame-arg? head)
             (continue-arg (cdr head) tail (car val)))
            ((stack-frame-multi-func? head)
             (continue-multi-func (cdr head) tail (car val)))
            ((stack-frame-multi-arg? head)
             (apply continue-multi-arg (cdr head) tail val))
            ((stack-frame-body? head)
             (continue-body (cdr head) tail (car val)))
            ((stack-frame-define? head)
             (continue-define (cdr head) tail (car val)))))))

(define (continue-func frame-body stack val)
  (if (obj-proc? val)
    (if (null? frame-body)
      (apply-procedure stack (cdr val))
      (eval-expr
        (cons-stack (stack-frame-arg (cdr val) '() (cdr frame-body)) stack) (car frame-body)))
    (goto-error stack "calling non-procedure")))

(define (continue-arg frame-body stack val)
  (let ((args-evaled-reversed (cons val (cadr frame-body)))
        (args-not-evaled (cddr frame-body)))
    (if (null? args-not-evaled)
      (apply apply-procedure stack (car frame-body) (reverse args-evaled-reversed))
      (eval-expr
        (cons-stack (stack-frame-arg (car frame-body) args-evaled-reversed (cdr args-not-evaled)) stack)
        (car args-not-evaled)))))

(define (continue-multi-func frame-body stack val)
  (if (obj-proc? val)
    (apply apply-procedure stack (cdr val) frame-body)
    (goto-error stack "calling non-procedure")))

(define (continue-multi-arg frame-body stack . vals)
  (eval-expr (stack-frame-multi-func vals) frame-body))

(define (continue-body frame-body stack val)
  (if (null? (cdr frame-body))
    (continue stack val)
    (eval-expr
      (cons-stack (stack-frame-body (car frame-body) (cddr frame-body)) stack)
      (cadr frame-body))))

(define (continue-define frame-body stack val)
  (define-var! (env-stack stack) frame-body val)
  (continue stack (obj-undefined)))

;; helper functions
(define (apply-procedure stack proc . args)
  (define (get-running-stack stack)
    (if (empty-stack? stack)
      stack
      (let ((head (head-stack stack)))
        (if (and (stack-frame-body? head) (null? (cddr head)))
          (tail-stack stack)
          stack))))
  (cond ((proc-lambda? proc)
         (apply-lambda (get-running-stack stack) (cdr proc) args))
        ((proc-builtin? proc)
         (apply (cdr proc) (get-running-stack stack) args))
        ((proc-continuation? proc)
         (apply continue (cdr proc) args))))

(define (apply-lambda stack lmd args)
  (let ((env (extend-env (car lmd))))
    (let loop ((arg-vars (cadr lmd)) (args args))
      (cond ((null? arg-vars) '())
            ((pair? arg-vars)
             (define-var! env (car arg-vars) (car args))
             (loop (cdr arg-vars) (cdr args)))
            (else (define-var! env arg-vars args))))
    (eval-expr
      (cons-stack (stack-frame-body env (cdddr lmd)) stack)
      (caddr lmd))))

(define (exec-bind stack var val)
  (let ((env (env-stack stack)))
    (cond ((symbol-hold-env? var)
           (let ((renamed-var
                   (string->symbol (string-append "*renamed*-" (symbol->string (cadr var))))))
             (define-var! env renamed-var val))
           (continue stack (obj-undefined)))
          ((symbol? var)
           (define-var! env var val)
           (continue stack (obj-undefined)))
          (else (goto-error stack "binding value to non-symbol")))))

(define (goto-error stack msg)
  (continue (stash-stack stack) (obj-error stack msg)))
