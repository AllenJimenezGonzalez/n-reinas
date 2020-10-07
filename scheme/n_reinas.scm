(define generate_row
  (lambda (n row)
    (cond
      ((zero? n) row )
      (else (generate_row (- n 1) ( append row '(*) ) ) ) 
      )
    ))


(define generate_column
  (lambda (n m table)
    (cond
      ((zero? n) table  )
      (else (generate_column (- n 1) m (cons (generate_row m '() ) table ) ) )  
      )
    ))

(define generate_table
  (lambda (n)
    (generate_column n n '() )
    ))

(define random_queens
  (lambda (x y table)
    (cond
      ((zero? n) table)
      (else ())
      )
   ))

(define redifine_column
  (lambda (pos counter t to)
    (cond
      ((= pos counter) (append to (append '(-) (cdr t))  ))  
      (else (redifine_column pos (+ counter 1) (cdr t) (append to (cons (car t) '() ) ) ) ) 
      )
    ))
(define redifine_row
  (lambda (posR posC counter t to)
    (cond
      (( = counter posR) (append to (append (redifine_column posC 0 (car t) '()  )  (cdr t) )       )) 
      (else (redifine_row posR posC (+ counter 1) (cons(cdr t) '()) (append to (cons(car t) '() ))     ))   
      )
  ))

(define set-list 
  (lambda (list x item)
  (cond
    ((empty? list) '())
    ((zero? x)     (cons item (cdr list)))
    (else
     (cons (car list) (set-list (cdr list) (- x 1) item))))))

(define set-matrix 
  (lambda (matrix x y item)
  (cond
    ((empty? matrix) '())
    ((zero? x)      (cons (set-list (car matrix) y item) (cdr matrix)))
    (else
     (cons (car matrix) (set-matrix (cdr matrix) (- x 1) y item))))))
