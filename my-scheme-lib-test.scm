(include "my-scheme-lib.scm")


(define-syntax test-equal?
  (syntax-rules ()
    ((_ e1 e2)
     (let ((v1 e1)
           (v2 e2))
       (unless (equal? v1 v2)
         (error "TEST-EQUAL? FAILURE: "
                'e1
                'e2
                v1
                v2
                ))))))


(define (main)
  (let ()
    (test-equal?
     (my-eval
      '(begin
         (define (f x)
           ((lambda (even? odd?)
              (even? even? odd? x))
            (lambda (ev? od? n)
              (if (= n 0) true (od? ev? od? (- n 1))))
            (lambda (ev? od? n)
               (if (= n 0) false (ev? ev? od? (- n 1))))))
         (cons (f 2)
               (f 3)))
      (setup-environment))
     (cons true false)))
  (let ()
    (test-equal?
     (my-eval
      '(begin
         (define (f x)
           (define (even? n)
             (if (= n 0)
                 true
                 (odd? (- n 1))))
           (define (odd? n)
             (if (= n 0)
                 false
                 (even? (- n 1))))
           (even? x))
         (f 3))
      (setup-environment))
     false)
    (test-equal?
     (my-eval
      '(begin
         (define (f x)
           (define (even? n)
             (if (= n 0)
                 true
                 (odd? (- n 1))))
           (define (odd? n)
             (if (= n 0)
                 false
                 (even? (- n 1))))
           (even? x))
         (f 4))
      (setup-environment))
     true))
  (let ()
    (test-equal?
     (my-eval
      '((lambda (n)
          ((lambda (fact)
             (fact fact n))
           (lambda (ft k)
             (if (= k 1)
                 1
                 (* k (ft ft (- k 1)))))))
        10)
      (setup-environment))
     3628800))
  (let ()
    (test-equal?
     (letrec->let '(letrec ((a 1)
                            (b 2))
                     c))
     '(let ((a '*unassigned*)
            (b '*unassigned*))
        (set! a 1)
        (set! b 2)
        c)))
  (let ()
    (test-equal?
     (scan-out-defines
      '((define u (e1))
        (e3)
        (define (v) (e2))))
     '((define u '*unassigned*)
       (define v '*unassigned*)
       (set! u (e1))
       (set! v (lambda () (e2)))
       (e3))))
  (let ()
    (test-equal?
     (scan-out-defines-4-16
      '((define u (e1))
        (define (v) (e2))
        (e3)))
     '(let ((u '*unassigned*)
            (v '*unassigned*))
        (set! u (e1))
        (set! v (lambda () (e2)))
        (e3))))
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
  (let ()
    (assert (equal? (expand-and-clauses '())
                    (make-let '() (list true))))
    (assert (equal? (expand-and-clauses '((a) (b) (c)))
                    (make-let '((sym2 (lambda () (c)))
                                (sym1 (lambda () (b)))
                                (sym0 (lambda () (a))))
                              (list
                               (make-let '((v (sym0)))
                                         (list
                                          (make-if 'v
                                                   (make-let '((v (sym1)))
                                                             (list
                                                              (make-if 'v
                                                                       (make-let '((v (sym2)))
                                                                                 (list
                                                                                  (make-if 'v
                                                                                           'v
                                                                                           false)))
                                                                       false)))
                                                   false))))))))
  (let ()
    (assert (equal? (expand-or-clauses '())
                    (make-let '() (list false))))
    (assert (equal? (expand-or-clauses '((a) (b) (c)))
                    (make-let '((sym2 (lambda () (c)))
                                (sym1 (lambda () (b)))
                                (sym0 (lambda () (a))))
                              (list
                               (make-let '((v (sym0)))
                                         (list
                                          (make-if 'v
                                                   'v
                                                   (make-let '((v (sym1)))
                                                             (list
                                                              (make-if 'v
                                                                       'v
                                                                       (make-let '((v (sym2)))
                                                                                 (list
                                                                                  (make-if 'v
                                                                                           'v
                                                                                           false))))))))))))))
  (let ()
    (assert (equal? (cond->if '(cond (a b c)
                                     ((d) => (e))
                                     (else f g)))
                    '(if a
                         (begin b c)
                         (let ((then (lambda () (e)))
                               (els (lambda () (begin f g)))
                               (p (d)))
                           (if p
                               ((then) p)
                               (els)))))))

  (let ()
    (assert (equal? (let*->nested-lets '(let* ()
                                          c
                                          d))
                    '(begin c d)))
    (assert (equal? (let*->nested-lets '(let* ((a 1)
                                               (b 2))
                                          c
                                          d))
                    '(let ((a 1))
                       (let ((b 2))
                         (begin c
                                d)))))
    )
  (let ()
    (assert (equal? (let->combination '(let () a))
                    '((lambda () a))))
    (assert (equal? (let->combination '(let ((a b)) a))
                    '((lambda (a) a) b)))
    (assert (equal? (let->combination '(let f () a))
                    '(let () (define (f) a) (f))))
    (assert (equal? (let->combination '(let f ((x y)) p))
                    '(let () (define (f x) p) (f y)))))
  (let ((frm (list 'frame (cons 1 2))))
    (add-binding-to-frame! 3 4 frm)
    (assert (equal? frm
                    (list 'frame
                          (cons 3 4)
                          (cons 1 2)))))
  (let ((frm (list 'frame (cons 1 2) (cons 5 6))))
    (add-binding-to-frame! 3 4 frm)
    (assert (equal? frm
                    (list 'frame
                          (cons 3 4)
                          (cons 1 2)
                          (cons 5 6)))))
  (let ((env (extend-environment '(a b c)
                                 '(1 2 3)
                                 the-empty-environment)))
    (unbind-variable! 'a env)
    (assert (equal? env
                    (list
                     (list 'frame
                           (cons 'b 2)
                           (cons 'c 3)))))
    )
  (let ((env (extend-environment '(a b c)
                                 '(1 2 3)
                                 the-empty-environment)))
    (unbind-variable! 'b env)
    (assert (equal? env
                    (list
                     (list 'frame
                           (cons 'a 1)
                           (cons 'c 3)))))
    )
  (let ((env (extend-environment '(a b c)
                                 '(1 2 3)
                                 the-empty-environment)))
    (unbind-variable! 'c env)
    (assert (equal? env
                    (list
                     (list 'frame
                           (cons 'a 1)
                           (cons 'b 2)))))
    )
  (let ((env (extend-environment '(a b c)
                                 '(1 2 3)
                                 the-empty-environment)))
    (unbind-variable! 'd env)
    (assert (equal? env
                    (list
                     (list 'frame
                           (cons 'a 1)
                           (cons 'b 2)
                           (cons 'c 3)))))
    )
  )


(main)
