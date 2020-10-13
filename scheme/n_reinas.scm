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

;; El contador se debe iniciar en 0
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

;;El contador se debe iniciar el -1
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
;;Funcion variable para obtener el barrido de la tabla ya sea desde la derecha o desde la izquierda
;;--------------------------------------------------------------------------------------------------------
;; X y Y se deben iniciar en 0
(define get_diagonal_aux
  (lambda (table x y fun output)
    ;(display table)
    (cond
      ((= x (length table)) (reverse output) ) 
      ;;Aca lo que hace es que se usa la funcion para traer la fila y luego se pasa la posicion de la columna que se pretende extraer
      (else (get_diagonal_aux table (fun x 1) (fun y 1) fun (cons (get_column_element_aux (get_row table x 0) -1 y 0) output   )  ) )
      )
    ))

;;--------------------------------------------------------------------------------------------------------
;;Bloque de funciones del barrido de la matriz de izquierda a derecha
;;--------------------------------------------------------------------------------------------------------
;;Barre de izquiera a derecha la tabla en la parte inferior

(define get_bottom_left_diagonal
  (lambda (table x y output)
    (cond
      ((= (length table) x) (reverse output))
      (else ( get_bottom_left_diagonal table (+ x 1) y (cons ( get_diagonal_aux table x y + '() ) output )) )
      )
    )
  )

;;Barre de izquierda a derecha la tabla en la parte superior
;;En el parametro Y se debe enviar con un numero 1
(define get_upper_left_diagonal
  (lambda (table x y output)
    (cond
      ((= (length table) y) output)
      (else ( get_upper_left_diagonal table x (+ y 1) (cons ( get_diagonal_aux table x y + '() ) output )) )
      )
    )
  )
;;Entrega todas las diagonales de izquierda a derecha en una tupla

(define get_left_diagonal
  (lambda (table)
    (append (get_upper_left_diagonal table 0 1 '()) (get_bottom_left_diagonal table 0 0 '() )  )
   ))
;;--------------------------------------------------------------------------------------------------------
;;Bloque de funciones del barrido de la matriz de derecha a izquierda
;;--------------------------------------------------------------------------------------------------------

(define get_bottom_rigth_diagonal
  (lambda (table x y output)
    (cond
      
      )
      ))

;;--------------------------------------------------------------------------------------------------------

(define evaluate
  (lambda (table n )
    (cond

      )
   ))