(load "./structure.scm")
(load "./eval.scm")
(load "./macro.scm")
(load "./builtin.scm")

(define (repl)
  (let ((stack (make-top-stack)))
    (define (loop)
      (display "sush> ")
      (flush)
      (let ((expr (read)))
        (if (eof-object? expr)
          (begin
            (display "Oaiso!\n")
            #t)
          (let ((val (eval-expr stack expr)))
            (newline)
            (loop)))))
    (loop)))

(define (top-eval-write exprs)
  (let ((stack (make-top-stack)))
    (let loop ((exprs exprs))
      (if (null? exprs)
        #t
        (let ((val (eval-expr stack (car exprs))))
          (print-data val)
          (newline)
          (loop (cdr exprs)))))))

(define (make-top-stack)
  (let ((stack (make-empty-stack (make-builtin-env))))
    (for-each (lambda (expr) (eval-expr stack expr)) base-library-exprs)
    stack))

(define (show val)
  (cond ((obj-error? val)
         (list 'error (cddr val) (show-stack (cadr val))))
        ((obj-pair? val) (cons (show (cadr val)) (show (cddr val))))
        ((obj-nil? val) '())
        (else val)))

(define (show-stack stack) (car stack) '())
