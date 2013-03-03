;Coursework 2
;Question 3
;Marinos Mavrommatis mm1g10@ecs.soton.ac.uk

(define *not 'not)
(define *and 'and)
(define *or 'or)

(define evaluate-formula
  (lambda (formula values)
    (letrec ((lookup-op
              (let ((my-and (lambda (a b) (and a b)))
                    (my-or (lambda (a b) (or a b))))
                (let ((ops (list (cons 'and my-and) (cons 'or my-or))))
                  (lambda (sym) (cdr (assq sym ops))))))
             
             (lookup-value
              (lambda (sym) (cdr (assq sym values))))
             
             (eval-wffs
              (lambda (op wffs)
                (let ((first (car wffs)) (rest (cdr wffs)))
                  (if (null? rest)
                      (eval-wff first)
                      ((lookup-op op) (eval-wff first)
                                      (eval-wffs op rest))))))
             
             (eval-wff
              (lambda (wff)
                (if (pair? wff)
                    (let ((head (car wff)) (tail (cdr wff)))
                      (if (eq? head *not) 
                          (not (eval-wff (car tail)))
                          (eval-wffs head tail)))
                    (lookup-value wff)))))
      
      (eval-wff formula))))
