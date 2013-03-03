;Coursework 2
;Question 4
;Marinos Mavrommatis mm1g10@ecs.soton.ac.uk

(define *not 'not)
(define *and 'and)
(define *or 'or)

(define formula->nnf
    (letrec ((negate
              (lambda (wff)
                (if (pair? wff)
                    (let ((head (car wff)) (tail (cdr wff)))
                      (if (eq? head *not) 
                          (formula->nnf (car tail))
                          (cons (if (eq? head *and) *or *and)
                                (map negate tail))))
                    (list *not wff)))))
      
      (lambda (wff)
        (if (pair? wff)
          (let ((head (car wff)) (tail (cdr wff)))
            (if (eq? head *not)
                (negate (car tail))
                (cons head (map formula->nnf tail))))
          wff))))
