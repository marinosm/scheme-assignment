;Coursework 2
;Question 1
;Marinos Mavrommatis mm1g10@ecs.soton.ac.uk

(define not-sym 'not)
(define and-sym 'and)
(define or-sym 'or)

(define formula?
  (letrec ((exactly-one-wff? 
            (lambda (l)
              (and (null? (cdr l))
                   (formula? (car l)))))
           
           (wffs? 
            (lambda (l)
              (and (formula? (car l))
                   (or (null? (cdr l))
                       (wffs? (cdr l))))))
           
           (valid-symbol? 
            (lambda (exp)
              (and (symbol? exp)
                   (not (or (eq? exp not-sym)
                            (eq? exp and-sym)
                            (eq? exp or-sym)))))))
    
    (lambda (exp)
      (if (pair? exp)
          (let ((head (car exp)) (tail (cdr exp)))
            (and (pair? tail)
                 (or (and (eq? head not-sym)  
                          (exactly-one-wff? tail))
                     (and (or (eq? head and-sym) 
                              (eq? head or-sym))
                          (wffs? tail)))))
          (valid-symbol? exp)))))
