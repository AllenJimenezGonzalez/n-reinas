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


;;--------------------------------------------------------------------------------------------------------
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
      (else (redifine_row posR posC (+ counter 1) (cons(cdr t) '()) (append to (cons(car t) '() )))))
  ))
;;--------------------------------------------------------------------------------------------------------
(define edit_row 
  (lambda (list x item)
  (cond
    ((empty? list) '())
    ((zero? x)     (cons item (cdr list)))
    (else
     (cons (car list) (edit_row (cdr list) (- x 1) item))))
    ))

(define edit_table
  (lambda (table x y item)
  (cond
    ((empty? table) '())
    ((zero? x) (cons (edit_row (car table) y item) (cdr table)))
    (else (cons (car table) (edit_table(cdr table) (- x 1) y item))))
 ))

;;--------------------------------------------------------------------------------------------------------

(define random_queens_aux
  (lambda (n counter table)
    (cond
      ((zero? counter) table)
      (else (random_queens_aux n (- counter 1) (edit_table table (random n) (random n) '♕ ) ))
      ) 
   ))
(define random_queens
  (lambda (n)
    (random_queens_aux n n (generate_table n) )
  ))

;;--------------------------------------------------------------------------------------------------------

(define get_row
  (lambda (table row counter)
    (cond
    ((= row counter) (car table))
    (else (get_row (cdr table) row (+ counter 1) ) )
    )
    )
  )


(define evaluate_row_aux 
  (lambda (table)
  ;(display list)
    (cond
      ((empty? table) 0)
      ((equal? (car table) '♕) (+ 1 (evaluate_row_aux (cdr table) )))
      (else (evaluate_row_aux (cdr table)))
      )
    ))


(define evaluate_rows
  (lambda (table)
    (map (lambda (x) (evaluate_row_aux x)) table)
    ))

;;--------------------------------------------------------------------------------------------------------

(define get_column_element_aux
  (lambda (row counter position element)
    (cond
      ((= counter position) element)
      ((pair? row ) (get_column_element_aux (cdr row) (+ counter 1) position (car row) ) )
      (else (get_column_element_aux row (+ counter 1) position row ) )
      )
   )
  )


(define get_column_aux
  (lambda (table counter position columns)
    ;(display table)
    (cond
      ((empty? table) columns)
      (else (get_column_aux (cdr table) (+ counter 1) position (append columns (cons (get_column_element_aux (car table) -1 position 0 )   '() ) )  )  )    
    )
  ))

(define get_columns
  (lambda (table counter output)
    (display table)
    (cond
      ((= counter (length table)) (reverse output) )
      (else (get_columns table (+ counter 1) (cons  (get_column_aux table -1 counter '() ) output  ) )   )
      )
    
  ))

(define evaluate_colum_aux
  (lambda (table)
    (cond
      
      ((empty? table) 0)
      ((equal? (car table) '♕) (+ 1 (evaluate_colum_aux (cdr table) )))
      (else (evaluate_colum_aux (cdr table)))
      
      )
      ))

(define evaluate_column
  (lambda (table)
    (display table)
    (map (lambda (x) (evaluate_colum_aux x)) table)
   ))

;;--------------------------------------------------------------------------------------------------------

(define evaluate
  (lambda (table n )
    (cond

      )
   ))