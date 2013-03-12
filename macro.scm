;; TODO check malformed macro

(define (unfold p f g seed)
  (if (p seed)
    '()
    (cons (f seed)
          (unfold p f g (g seed)))))

(define (map-variable-length f . lists)
  (let ((new-lists (remove null? lists)))
    (if (null? new-lists)
      (list)
      (cons (apply f (map car new-lists))
            (apply map-variable-length f (map cdr new-lists))))))

(define (try-expand-macro stack expr)
  (cond ((and (pair? expr) (lookup-env (env-stack stack) (car expr)))
         => (lambda (obj)
              (if (obj-macro? obj)
                (let ((result (expand-macro (cdr obj) (cdr expr))))
                  ;(print expr)
                  (if (eq? result 'error-no-pattern)
                    (goto-error stack "macro pattern doesn't match")
                    result))
                #f)))
        (else #f)))

(define (expand-macro macro body)
  (let ((env (car macro))
        (literals (cadr macro))
        (rules (caddr macro)))
    (define (find-match rules)
      (if (null? rules)
        #f
        (let ((matched (match-pattern literals (car rules) body)))
          (if matched
            (cons matched (cadr rules))
            (find-match (cddr rules))))))
    (let ((found (find-match rules)))
      (if found
        (inflate-template env (car found) (cdr found))
        'error-no-pattern))))

(define (match-pattern literals pattern body)
  (call/cc
    (lambda (cc)
      (define (recur-match pattern body)
        (cond ((and (null? pattern) (null? body))
                (list))
              ((and (pair? pattern) (pair? body))
                (if (and (pair? (cdr pattern)) (eq? (cadr pattern) '...))
                  (if (list? body)
                    (letrec
                      ((product-same-structure-mappings
                         (lambda mappings
                           (apply map product-same-keys-dicts mappings)))
                       (product-same-keys-dicts
                         (lambda dicts
                           (apply map product-same-key-pairs dicts)))
                       (product-same-key-pairs
                         (lambda pairs
                           (cons (caar pairs) (map cdr pairs))))
                       (inflate-one
                         (lambda (expr) (recur-match (car pattern) expr))))
                      (cons (list)
                            (apply product-same-structure-mappings (map inflate-one body))))
                    (cc #f))
                  (let ((res-car (recur-match (car pattern) (car body)))
                        (res-cdr (recur-match (cdr pattern) (cdr body))))
                    (map-variable-length append res-car res-cdr))))
              ((and (vector? pattern) (vector? body))
                (recur-match (vector->list pattern) (vector->list body)))
              ((symbol? pattern)
                (if (memq pattern literals)
                  (if (eq? body pattern)
                    (list)
                    (cc #f))
                  (list (list (cons pattern body)))))
              (else (cc #f))))
      (recur-match (cdr pattern) body))))

(define (inflate-template env mapping template)
  (call/cc
    (lambda (cc)
      (define (slice-dict dict)
        (unfold null?
                (lambda (dict)
                  (map (lambda (pair) (cons (car pair) (cadr pair)))
                       dict))
                (lambda (dict)
                  (remove (lambda (pair) (null? (cdr pair)))
                          (map (lambda (pair) (cons (car pair) (cddr pair)))
                               dict)))
                dict))
      (define sliced-deeper-mapping
        (apply map-variable-length
               (lambda x x)
               (map slice-dict (cdr mapping))))
      (define (recur-inflate mapping template)
        (cond ((null? template) (list '()))
              ((pair? template)
                (if (and (pair? (cdr template)) (eq? (cadr template) '...))
                  (let* ((inflated-pairs
                           (map (lambda (next-mapping) (recur-inflate next-mapping (car template)))
                                sliced-deeper-mapping))
                         (inflated-pairs-filtered
                           (remove (lambda (inflated-pair) (null? (cdr inflated-pair)))
                                   inflated-pairs))
                         (captured-length (length (cdar inflated-pairs-filtered))))
                    (if (fold (lambda (inflated-pair acc)
                                (and acc (= captured-length (length (cdr inflated-pair)))))
                              #t
                              inflated-pairs-filtered)
                      (cons (map car inflated-pairs-filtered)
                            (cdar inflated-pairs-filtered))
                      (cc (obj-error "f*ck"))))
                  (let ((car-inflated-pair (recur-inflate mapping (car template)))
                        (cdr-inflated-pair (recur-inflate mapping (cdr template))))
                    (cons (cons (car car-inflated-pair) (car cdr-inflated-pair))
                          (append (cdr car-inflated-pair) (cdr cdr-inflated-pair))))))
              ((vector? template)
                (list->vector (recur-inflate mapping (vector->list template))))
              ((and (symbol? template) (assq template (car mapping)))
                => (lambda (pair)
                     (cons (cdr pair)
                           (list (car pair)))))
              ((and (symbol? template) (not (memq template keywords)))
                (cons (hold-env template env)
                      (list)))
              (else
                (cons template
                      (list)))))
      (car (recur-inflate mapping template)))))
