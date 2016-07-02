(include "my-scheme-lib.scm")


(define (main)
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
