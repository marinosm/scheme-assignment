;Coursework 2
;Question 2
;Marinos Mavrommatis mm1g10@ecs.soton.ac.uk

(define *open-brackets #\()
(define *close-brackets #\))
(define *not-in 'not)
(define *not-out #\~)
(define *andor-table (list (cons 'and #\^)
                           (cons 'or #\v)))

(define formula->string
  (letrec ((wff->list
            (let ((combine
                   (letrec ((combine-rec
                             (lambda (l sym inner)
                               (let ((head (wff->list (car l) #t)))
                                 (if (null? (cdr l))
                                     (if inner (list #\space sym #\space head *close-brackets)(list #\space sym #\space head))
                                     (cons #\space (cons sym (cons #\space (cons head (combine-rec (cdr l) sym inner))))))))))
                     
                     (lambda (l sym inner)
                       (let ((head (wff->list (car l) #t))
                             (tail (combine-rec (cdr l) sym inner)))
                         (if inner
                             (cons *open-brackets (cons head tail))
                             (cons head tail)))))))
              
              (lambda (wff inner)
                (if (pair? wff)
                    (let ((head (car wff)) (tail (cdr wff)))
                      (cond ((eq? head *not-in) (cons *not-out (wff->list (car tail) #t)))
                            ((null? (cdr tail)) (wff->list (car tail) inner))
                            (else (combine tail (cdr (assq head *andor-table)) inner))))
                    (map char-upcase (string->list (symbol->string wff)))))))
           
           (flatten 
            (letrec ((flatten*
                      (lambda (head tail)
                        (if (null? head)
                            (flatten tail)
                            (if (pair? (car head)) 
                                (flatten* (flatten* (car head) (cdr head)) tail) 
                                (cons (car head)
                                      (flatten* (cdr head) tail)))))))
              
              (lambda (l)
                (if (null? l)
                    l
                    (let ((head (car l))(tail (cdr l)))
                      (if (pair? head)
                          (flatten* head tail)
                          (cons head (flatten tail)))))))))
    
    (lambda (wff)
      (list->string (flatten (wff->list wff #f))))))
