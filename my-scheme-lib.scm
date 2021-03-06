(define-module (my-scheme-lib))


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


(define (comp2 g f) (lambda (x) (g (f x))))


(define number->symbol (comp2 string->symbol number->string))


(define (sym-n n)
  (string->symbol (string-append "sym" (number->string n))))


(define (my-eval-orig exp env)
  (cond ((self-evaluating? exp) exp)
        ((var? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (my-eval-orig (expand-and-clauses (and-clauses exp)) env))
        ((or? exp) (my-eval-orig (expand-or-clauses (or-clauses exp)) env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (my-eval-orig (cond->if exp) env))
        ((let? exp) (my-eval-orig (let->combination exp) env))
        ((letrec? exp) (my-eval-orig (letrec->let exp) env))
        ((let*? exp) (my-eval-orig (let*->nested-lets exp) env))
        ((application? exp) (my-apply (my-eval-orig (operator exp) env)
                                      (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))


(define (my-eval-analyze exp env)
  ((analyze exp) env))


(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((var? exp) (analyze-var exp))
        ((quoted? exp) (analyze-quoted exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((and? exp) (analyze (expand-and-clauses (and-clauses exp))))
        ((or? exp) (analyze (expand-or-clauses (or-clauses exp))))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((letrec? exp) (analyze (letrec->let exp)))
        ((let*? exp) (analyze (let*->nested-lets exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type -- EVAL" exp))))


(define (analyze-self-evaluating exp)
  (lambda (env) exp))


(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))


(define (analyze-var exp)
  (lambda (env) (lookup-variable-value exp env)))


(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))


(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))


(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))


(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence
                (scan-out-defines (lambda-body exp)))))
    (lambda (env) (make-procedure-analyze vars bproc env))))


(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE")
        (loop (car procs) (cdr procs)))))


(define (analyze-application exp)
  (let ((pproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (pproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))


(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))


(define (my-eval-lazy exp env)
  (cond ((self-evaluating? exp) exp)
        ((var? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (my-eval-lazy (expand-and-clauses (and-clauses exp)) env))
        ((or? exp) (my-eval-lazy (expand-or-clauses (or-clauses exp)) env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (my-eval-lazy (cond->if exp) env))
        ((let? exp) (my-eval-lazy (let->combination exp) env))
        ((letrec? exp) (my-eval-lazy (letrec->let exp) env))
        ((let*? exp) (my-eval-lazy (let*->nested-lets exp) env))
        ((application? exp) (my-apply (actual-value (operator exp) env)
                                      (operands exp)
                                      env))
        (else (error "Unknown expression type -- EVAL" exp))))


(define (actual-value exp env)
  (force-it (my-eval-lazy exp env)))


(define (my-apply-lazy procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence-lazy
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure))))
        (else
         (error
          ("Unknown procedure type -- APPLY" procedure)))))


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


(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc)
   args))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))


(define (make-procedure parameters body env)
  (list 'procedure
        parameters
        ; Q 4.16 make-procedure is called only once
        ; procedure-body is called many times
        (scan-out-defines body)
        env))


(define (make-procedure-analyze parameters bproc env)
  (list 'procedure
        parameters
        bproc
        env))

(define (scan-out-defines-4-16 body)
  "Q 4.16 b"
  (let* ((defs-others (filter2 definition? body))
         (defs (car defs-others))
         (vars (map definition-variable defs))
         (vals (map definition-value defs))
         (others (cdr defs-others)))
    (make-let
     (map (lambda (var)
            (list var (quote '*unassigned*)))
          vars)
     (append (map (lambda (var val)
                    (list 'set! var val))
                  vars
                  vals)
             others))))


(define (scan-out-defines body)
  "Q 4.17"
  (let* ((defs-others (filter2 definition? body))
         (defs (car defs-others))
         (vars (map definition-variable defs))
         (vals (map definition-value defs))
         (others (cdr defs-others)))
    (append
     (map (lambda (var)
            (list 'define var (quote '*unassigned*)))
          vars)
     (map (lambda (var val)
            (list 'set! var val))
          vars
          vals)
     others)))


(define (filter2 pred xs)
  (let ((ts-fs (rev-filter2 pred xs)))
    (cons (reverse (car ts-fs))
          (reverse (cdr ts-fs)))))


(define (rev-filter2 pred xs)
  (let loop ((xs xs)
             (ts '())
             (fs '()))
    (if (null? xs)
        (cons ts fs)
        (let ((x (car xs)))
          (if (pred x)
              (loop (cdr xs)
                    (cons x ts)
                    fs)
              (loop (cdr xs)
                    ts
                    (cons x fs)))))))


(define procedure-parameters cadr)
(define procedure-body caddr)
(define procedure-environment cadddr)


(define true #t)

(define false #f)

(define (true? x)
  (not (false? x)))

(define (false? x)
  (eq? x false))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval-orig (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
"Q 4.1 ->"
(define (list-of-values-q-4-1-> exps env)
  (if (no-operands? exps)
      '()
      (let ((a (my-eval-orig (first-operand exps) env)))
        (cons a
              (list-of-values (rest-operands exps) env)))))
"Q 4.1 <-"
(define (list-of-values-q-4-1<- exps env)
  (if (no-operands? exps)
      '()
      (let ((d (list-of-values (rest-operands exps) env)))
        (cons (my-eval-orig (first-operand exps) env)
              d))))


(define (eval-if exp env)
  (if (true? (my-eval-orig (if-predicate exp) env))
      (my-eval-orig (if-consequent exp) env)
      (my-eval-orig (if-alternative exp) env)))


(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval-orig (first-exp exps) env))
        (else (my-eval-orig (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


(define (eval-sequence-lazy exps env)
  (cond ((last-exp? exps) (my-eval-lazy (first-exp exps) env))
        (else (my-eval-lazy (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (my-eval-orig (assignment-value exp) env)
                       env)
  'ok)


(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (my-eval-orig (definition-value exp) env)
                    env)
  'ok)


(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(define var? symbol?)


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


(define (make-let kvs body)
  (cons 'let (cons kvs body)))


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

(define (cond-=>-actions? actions)
  (and (pair? actions)
       (eq? (car actions) '=>)))

(define cond-predicate car)

(define cond-actions cdr)

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  "Q 4.5"
  (if (null? clauses)
      false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause is not last -- EXPAND-CLAUSES"
                       clauses))
            (let ((actions (cond-actions first))
                  (pred (cond-predicate first)))
              (if (cond-=>-actions? actions)
                  (let ((action (cdr actions)))
                    (and (null? action) (error "=> clause does not contain an action -- EXPAND-CLAUSES"))
                    (and (pair? (cdr action)) (error "=> clause contains multiple actions -- EXPAND-CLAUSES"))
                    (make-let (list (list 'then (make-lambda '()
                                                             action))
                                    (list 'els (make-lambda '()
                                                            (list (expand-clauses rest))))
                                    (list 'p pred))
                              (list (make-if 'p
                                             '((then) p)
                                             '(els)))
                              ))
                  (make-if pred
                           (sequence->exp actions)
                           (expand-clauses rest))))))))


(define (let? exp)
  (tagged-list? exp 'let))

(define (letrec? exp)
  (tagged-list? exp 'letrec))

(define let-kvs cadr)

(define let-body cddr)

(define (let->combination exp)
  "Q 4.6 Q 4.8"
  (if (symbol? (let-kvs exp))
      (let ((name (cadr exp))
            (kvs (caddr exp))
            (body (cdddr exp)))
        (make-let '()
                  (list
                   (cons 'define
                         (cons (cons name (map car kvs))
                               body))
                    (cons name (map cadr kvs)))))
      (let ((kvs (let-kvs exp))
            (body (let-body exp)))
        (cons (make-lambda (map car kvs) body)
              (map cadr kvs)))))


(define (letrec->let exp)
  "Q 4.20"
  (let ((exp (cdr exp)))
    (if (null? exp)
        (error "No decl nor body provided -- letrec: " exp)
        (let ((kvs (car exp))
              (body (cdr exp)))
          (make-let (map (lambda (kv)
                           (list (car kv) (quote '*unassigned*)))
                         kvs)
                    (append (map (lambda (kv)
                                   (list 'set! (car kv) (cadr kv)))
                                 kvs)
                            body))))))


(define (and? exp)
  (tagged-list? exp 'and))

(define and-clauses cdr)

(define (eval-and-special exp env)
  "Q 4.4"
  (let loop ((s (and-clauses exp))
             (ret true))
    (if (null? s)
        ret
        (let ((ret (my-eval-orig (car s) env)))
          (if (true? ret)
              (loop (cdr s) ret)
              false)))))

(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

(define (expand-and-clauses clauses)
  "Q 4.4"
  (let* ((kvs (let loop ((bodies clauses)
                         (i 0)
                         (ret '()))
                (if (null? bodies)
                    ret
                    (loop (cdr bodies)
                          (1+ i)
                          (cons (list (sym-n i)
                                      (make-lambda '() (list (car bodies))))
                                ret)))))
         (ks (map car kvs)))
    (make-let kvs
              (list
               (if (null? ks)
                   true
                   (let loop ((ret (make-let (list (list 'v (list (car ks))))
                                             (list (make-if 'v 'v false))))
                              (ks (cdr ks)))
                     (if (null? ks)
                         ret
                         (loop (make-let (list (list 'v (list (car ks))))
                                         (list (make-if 'v ret false)))
                               (cdr ks)))))))))


(define (or? exp)
  (tagged-list? exp 'or))

(define or-clauses cdr)

(define (eval-or-special exp env)
  "Q 4.4"
  (let loop ((s (or-clauses exp)))
    (if (null? s)
        false
        (let ((ret (my-eval-orig (car s) env)))
          (if (true? ret)
              ret
              (loop (cdr s)))))))

(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

(define (expand-or-clauses clauses)
  "Q 4.4"
  (let* ((kvs (let loop ((bodies clauses)
                         (i 0)
                         (ret '()))
                (if (null? bodies)
                    ret
                    (loop (cdr bodies)
                          (1+ i)
                          (cons (list (sym-n i)
                                      (make-lambda '() (list (car bodies))))
                                ret)))))
         (ks (map car kvs)))
    (make-let kvs
              (list
               (if (null? ks)
                   false
                   (let loop ((ret (make-let (list (list 'v (list (car ks))))
                                             (list (make-if 'v 'v false))))
                              (ks (cdr ks)))
                     (if (null? ks)
                         ret
                         (loop (make-let (list (list 'v (list (car ks))))
                                         (list (make-if 'v 'v ret)))
                               (cdr ks)))))))))


(define (let*? exp)
    (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  "Q 4.7"
  (let loop ((kvs (reverse (let-kvs exp)))
             (ret (make-begin (let-body exp))))
    (if (null? kvs)
        ret
        (loop (cdr kvs)
              (make-let (list (car kvs)) (list ret))))))


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
         (impl (get f ts)))
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
    (cond ((last-exp? exps) (my-eval-4-3 (first-exp exps) env))
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


(define (lookup-variable-value var env)
  "Q 4.12"
  (loop-env
   var env
   (lambda (kv)
     (let ((v (cdr kv)))
       ; Q 4.16
       (if (eq? v '*unassigned*)
           (error "Access to unassigned variable: " (car kv))
           v)))))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


(define (define-variable! var val env)
  "Q 4.11"
  (let* ((frame (first-frame env))
         (kv (assoc var (frame-kvs frame))))
    (if kv
        (set-cdr! kv val)
        (add-binding-to-frame! var val frame))))


(define (set-variable-value! var val env)
  "Q 4.11 Q 4.12"
  (loop-env
   var env
   (lambda (kv)
     (set-cdr! kv val))))


(define (unbind-variable! var env)
  "Q 4.13"
  (let loop ((env env))
    (unless (eq? env the-empty-environment)
      (let* ((frame (first-frame env)))
        (let scan ((prev frame)
                   (curr (cdr frame)))
          (if (null? curr)
              (loop (enclosing-environment env))
              (if (eq? var (caar curr))
                  (set-cdr! prev (cdr curr))
                  (scan curr
                        (cdr curr)))))))))


(define (loop-env var env found)
  (let loop ((env env))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- LOOP-ENV" var)
        (let* ((frame (first-frame env))
               (kv (assoc var (frame-kvs frame))))
          (if kv
              (found kv)
              (loop (enclosing-environment env)))))))


(define enclosing-environment cdr)


(define (make-frame vars vals)
  "Q 4.11"
  (cons 'frame (map cons vars vals)))


(define (add-binding-to-frame! var val frame)
  "Q 4.11"
  (set-cdr! frame (cons (cons var val)
                        (frame-kvs frame))))


(define first-frame car)
(define frame-kvs cdr)


(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1))
         (fib(- n 2)))))


(define primitive-procedures
  (list
   (cons '= =)
   (cons '< <)
   (cons '> >)
   (cons '<= <=)
   (cons '>= >=)
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)
   (cons 'car car)
   (cons 'cdr cdr)
   (cons 'cons cons)
   (cons 'null? null?)
   (cons 'pair? pair?)
   ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (kv) (list 'primitive (cdr kv)))
       primitive-procedures))


(define (setup-environment)
  (let ((initial-env
         (extend-environment
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


(define the-empty-environment '())


(define input-prompt ";; M-eval input")
(define output-prompt ";; M-eval value")


(define my-eval my-eval-analyze)


(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (my-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


(define (prompt-for-input s)
  (newline)
  (newline)
  (display s)
  (newline))


(define (announce-output s)
  (newline)
  (display s)
  (newline))


(define (user-print obj)
  (display
   (if (compound-procedure? obj)
       (list 'compound-procedure
             (procedure-parameters obj)
             (procedure-body obj))
       obj)))


(define the-global-environment (setup-environment))
