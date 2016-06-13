(define-module (my-scheme))


(define-syntax assert
  (syntax-rules ()
    ((_ ex)
     (let ((v ex))
       (if v
           v
           (error "ASSERTION FAILURE: " 'ex))))))


(define (zip2 xs ys)
  (reverse (zip2-rev xs ys)))

(define (zip2-rev xs ys)
  (let loop ((xs xs)
             (ys ys)
             (ret '()))
    (if (and (pair? xs)
             (pair? ys))
        (loop (cdr xs)
              (cdr ys)
              (cons (list (car xs)
                          (car ys))
                    ret))
        ret)))


(define (comp2 f g) (lambda (x) (g (f x))))


(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (my-eval (cond->if exp) env))
        ((application? exp) (my-apply (my-eval (operator exp) env)
                                      (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))


(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define true #t)

(define false #f)

(define (true? x)
  (not (false? x)))

(define (false? x)
  (eq? x false))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
"Q 4.1 ->"
(define (list-of-values-q-4-1-> exps env)
  (if (no-operands? exps)
      '()
      (let ((a (my-eval (first-operand exps) env)))
        (cons a
              (list-of-values (rest-operands exps) env)))))
"Q 4.1 <-"
(define (list-of-values-q-4-1<- exps env)
  (if (no-operands? exps)
      '()
      (let ((d (list-of-values (rest-operands exps) env)))
        (cons (my-eval (first-operand exps) env)
              d))))


(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))


(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
        (else (my-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (my-eval (assignment-value exp) env)
                       env)
  'ok)


(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (my-eval (definition-value exp) env)
                    env)
  'ok)


(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(define variable? symbol?)


(define (quoted? exp)
  (tagged-list? exp 'quote))

(define text-of-quotation cadr)


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


(define (assignment? exp)
  (tagged-list? exp 'set!))

(define assignment-variable cadr)

(define assignment-value caddr)


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))


(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define lambda-parameters cadr)

(define lambda-body cddr)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp)
  (tagged-list? exp 'if))

(define if-predicate cadr)

(define if-consequent caddr)

(define (if-alternative exp)
  (if (null? (cdddr exp))
      'false
      (cadddr exp)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp)
  (tagged-list? exp 'begin))

(define begin-actions cdr)

(define (last-exp? seq)
  (null? (cdr seq)))

(define first-exp car)

(define rest-exps cdr)

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp exp))
        (else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))


(define application? pair?)

(define operator car)

(define operands cdr)

(define no-operands? null?)

(define first-operand car)

(define rest-operands cdr)


(define (cond? exp)
  (tagged-list? exp 'cond))

(define cond-clauses cdr)

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define cond-predicate car)

(define cond-actions cdr)

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause is not last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


(define (my-eval-4-2-b exp env)
  "Q 4.2 b"
  (define (application? exp)
    (tagged-list? exp 'call))

  (define operator cadr)

  (define operands cddr)

  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((application? exp) (my-apply (my-eval-4-2-b (operator exp) env)
                                      (list-of-values (operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (my-eval-4-2-b (cond->if exp) env))
        (else (error "Unknown expression type -- EVAL" exp))))


(define (make-table)
  (let ((table '()))
    (define (lookup key tbl)
      (if (pair? tbl)
          (let ((p (car tbl)))
            (if (equal? key (car p))
                p
                (lookup key (cdr tbl))))
          '()))
    (define (dispatch m)
      (cond
       ((eq? m 'lookup) (lambda (key) (lookup key table)))
       ((eq? m 'insert!) (lambda (key val) (set! table (cons (cons key val) table))))
       (else (error "Unknown dispatch type -- make-table/dispatch:" m))))
    dispatch))

(define (lookup- key table)
  ((table 'lookup) key))

(define (lookup key table)
  (let ((ret (lookup- key table)))
    (if (has-val? ret)
        (cdr ret)
        (error "KeyError -- lookup" key))))

(define (insert! key val table)
  ((table 'insert!) key val))

(define has-val? pair?)

(define val-kv cdr)


(define *method-table* (make-table))

(define (put f ts impl)
  (insert! (cons f ts) impl *method-table*))

(define (get f ts)
  (lookup- (cons f ts) *method-table*))

(define type-tag car)
(define contents cdr)


(define (apply-generic f . args)
  (let* ((ts (map type-tag args))
         (impl (get op ts)))
    (if (pair? impl)
        (apply impl (map contents args))
        (error "No implementation of " f " for type " ts))))


(define (my-eval-4-3 exp env)
  "Q 4.3"
  (define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (my-eval-4-3 (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((pair? exp) (let* ((op (operator exp))
                            (impl (get 'eval op)))
                       (if (has-val? impl)
                           ((val-kv impl) exp env)
                           (my-apply (my-eval-4-3 op env)
                                     (list-of-values (operands exp) env)))))
        (else (error "Unknown expression type -- EVAL" exp))))


(define (install-eval-4-3)
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (my-eval-4-3 (assignment-value exp) env)
                         env)
    'ok)
  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
      (my-eval-4-3 (definition-value exp) env)
      env)
    'ok)
  (define (eval-sequence exps env)
    (cond ((last-exp? exps) (my-eval (first-exp exps) env))
          (else (my-eval-4-3 (first-exp exps) env)
                (eval-sequence (rest-exps exps) env))))
  (define (text-of-quotation exp env)
    (cadr exp))
  (define (eval-if exp env)
    (if (true? (my-eval-4-3 (if-predicate exp) env))
        (my-eval-4-3 (if-consequent exp) env)
        (my-eval-4-3 (if-alternative exp) env)))
  (define (add f impl)
    (put 'eval f impl))

  (add 'quote text-of-quotation)
  (add 'set! eval-assignment)
  (add 'define eval-definition)
  (add 'if eval-if)
  (add 'lambda (lambda (exp env)
                 (make-procedure (lambda-parameters exp)
                                 (lambda-body exp)
                                 env)))
  (add 'begin (lambda (exp env)
                (eval-sequence (begin-actions exp) env)))
  (add 'cond (lambda (exp env)
               (my-eval-4-3 (cond->if exp) env)))
  )

(install-eval-4-3)


(define (test)
  (let ((tbl (make-table)))
    (insert! 'a 1 tbl)
    (insert! 'b 2 tbl)
    (assert (eq? (lookup- 'not tbl) '()))
    (assert (eq? (lookup 'b tbl) 2))
    (assert (eq? (lookup 'a tbl) 1))
    )
  (let ()
    (assert (equal? (my-eval-4-3 1 '()) 1))
    (assert (equal? (my-eval-4-3 '(quote QUOTE) '()) 'QUOTE))
    )
  (let ()
    (assert (equal? (zip2 '(1 2 3) '(a b c d))
                    '((1 a)
                      (2 b)
                      (3 c)))))
  )

(define (main)
  #t
  )


(test)
