;;--------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------
;;Inicio del seccion del generador de cromosomas y poblacion aleatorios
;;--------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------

(define generate_row
  (lambda (n row)
    (cond
      ((zero? n) row )
      (else (generate_row (- n 1) ( append row '(*) ) ) ) 
      )
    ))

;;--------------------------------------------------------------------------------------------------------
(define generate_column
  (lambda (n m table)
    (cond
      ((zero? n) table  )
      (else (generate_column (- n 1) m (cons (generate_row m '() ) table ) ) )  
      )
    ))
;;--------------------------------------------------------------------------------------------------------
(define generate_table
  (lambda (n)
    (generate_column n n '() )
    ))
    
;;--------------------------------------------------------------------------------------------------------
(define edit_row 
  (lambda (list x item)
  (cond
    ((empty? list) '())
    ((zero? x)     (cons item (cdr list)))
    (else (cons (car list) (edit_row (cdr list) (- x 1) item)) )
   )
 ))
;;--------------------------------------------------------------------------------------------------------
(define edit_table
  (lambda (table x y item)
  (cond
    ((empty? table) '())
    ((zero? x) (cons (edit_row (car table) y item) (cdr table)))
    (else (cons (car table) (edit_table(cdr table) (- x 1) y item)))
    )
  ))

;;--------------------------------------------------------------------------------------------------------

(define random_queens_aux
  (lambda (n counter table)
    (cond
      ((zero? counter) table)
      (else (random_queens_aux n (- counter 1) (edit_table table (random n) (random n) '♕ ) ))
      ) 
   ))
;;--------------------------------------------------------------------------------------------------------
(define random_queens
  (lambda (n)
    (random_queens_aux n n (generate_table n) )
  ))
;;--------------------------------------------------------------------------------------------------------

(define get_population  
  (lambda (size psize output)
    (cond
      ((zero? size ) output)
      (else (get_population (- size 1) psize (cons (random_queens psize) output  ) ) )
      )
      ))

;;--------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------
;;Inicio del segmento de evaluacion de cromosomas
;;--------------------------------------------------------------------------------------------------------
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
    (cond
      ((empty? table) columns)
      (else (get_column_aux (cdr table) (+ counter 1) position (append columns (cons (get_column_element_aux (car table) -1 position 0 )   '() ) )  )  )    
    )
  ))

(define get_columns
  (lambda (table counter output)
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

;;--------------------------------------------------------------------------------------------------------
;;Diagonales <-> Funcion variable para obtener el barrido de la tabla ya sea desde la derecha o desde la izquierda
;;--------------------------------------------------------------------------------------------------------
;; X y Y se deben iniciar en 0
(define get_diagonal_aux
  (lambda (table x y fun1 fun2 output)
    (cond
      ((= x (length table)) (reverse output) ) 
      ;;Aca lo que hace es que se usa la funcion para traer la fila y luego se pasa la posicion de la columna que se pretende extraer
      (else (get_diagonal_aux table (fun1 x 1) (fun2 y 1) fun1 fun2 (cons (get_column_element_aux (get_row table x 0) -1 y 0) output   )  ) )
      )
    ))


(define get_diagonal_aux_y
  (lambda (table x y fun1 fun2 output)
    (cond
      (( > 0 y ) (reverse output) )
      (else (get_diagonal_aux_y table (fun1 x 1) (fun2 y 1) fun1 fun2 (cons (get_column_element_aux (get_row table x 0) -1 y 0) output   )  ) )
      )
    ))

;;--------------------------------------------------------------------------------------------------------
;;Diagonales <-> barrido de la matriz de izquierda a derecha
;;--------------------------------------------------------------------------------------------------------
;;Barre de izquiera a derecha la tabla en la parte inferior

(define get_bottom_left_diagonal
  (lambda (table x y output)
    (cond
      ((= (length table) x) (reverse output))
      (else ( get_bottom_left_diagonal table (+ x 1) y (cons ( get_diagonal_aux table x y + + '() ) output )) )
      )
    )
  )

;;Barre de izquierda a derecha la tabla en la parte superior
;;En el parametro Y se debe enviar con un numero 1
(define get_upper_left_diagonal
  (lambda (table x y output)
    (cond
      ((= (length table) y) output)
      (else ( get_upper_left_diagonal table x (+ y 1) (cons ( get_diagonal_aux table x y + + '() ) output )) )
      )
    )
  )
;;Entrega todas las diagonales de izquierda a derecha en una tupla
(define get_left_diagonal
  (lambda (table)
    (append (get_upper_left_diagonal table 0 1 '()) (get_bottom_left_diagonal table 0 0 '() )  )
   ))
;;--------------------------------------------------------------------------------------------------------
;;Diagonales <-> Bloque de funciones del barrido de la matriz de derecha a izquierda
;;--------------------------------------------------------------------------------------------------------
;;Hay que enviar y-1 como parametro en y o en su defecto -> (length table ) -1
(define get_bottom_rigth_diagonal
  (lambda (table x y output)
    (cond
      ((= x (length table)) (reverse output) )
      (else (get_bottom_rigth_diagonal table (+ x 1) y (cons ( get_diagonal_aux table x y + - '() ) output ) ))
      )
      ))
;;--------------------------------------------------------------------------------------------------------
;; El parametro Y se le debe restar 2 -> (Length table)-2
;; Se le debe restar 2 debido a que la diagonal identidad ya esta contemplada en la parte inferior del barrido de derecha a izquierda
(define get_upper_rigth_diagonal
  (lambda (table x y output)
    (cond
      ((< y 0) output)
      (else (get_upper_rigth_diagonal table x (- y 1) (cons (get_diagonal_aux_y table x y + - '() ) output )  ) )
      )
    ))
;;--------------------------------------------------------------------------------------------------------
;; Funcuion que agrupa todas las diagonales de la tabla barrida de izquierda a derecha
(define get_right_diagonal
  (lambda (table)
    (append (get_upper_rigth_diagonal table 0 (- (length table) 2 ) '() ) (get_bottom_rigth_diagonal table 0 (- (length table) 1 ) '() ) )
    ))
;;--------------------------------------------------------------------------------------------------------

(define evaluate_get_score
  (lambda (results)
    (cond
      ((empty? results) 0)
      ((equal? (car results) '♕) (+ 1 (evaluate_colum_aux (cdr results) )))
      (else (evaluate_colum_aux (cdr results)))
    
      )
    ))
;;--------------------------------------------------------------------------------------------------------
(define evaluate_data
  (lambda (results)
    (map (lambda (x) (evaluate_get_score x)) results)
    ))
;;--------------------------------------------------------------------------------------------------------
(define evaluate
  (lambda (table)
    ;(display table )
    (append
    ;;Evalua las filas
    (evaluate_data table )
    ;;Evalua las columnas
    (evaluate_data (get_columns table 0 '() ))
    ;;Evalua la diagonal con barrido de izquierda a derecha
    (evaluate_data (get_left_diagonal table))
    ;;Evalua la diagonal con barrido de derecha a izquierda
    (evaluate_data (get_right_diagonal table))
    )
   ))
;;--------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------
;;Inicio del segmento de cruce
;;--------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------
(define evaluate_new_gen
  (lambda (child)
    (apply + (evaluate_data child))
  ))
;;--------------------------------------------------------------------------------------------------------

(define take_left
  (lambda (table output counter)
    (display table)
    (cond
      ((empty? table) output)   
      ( (= counter (round( / (length table) 2)) ) (reverse output) )
      ((equal?(pair? (cdr table)) #f ) (reverse output)  )
      (else (take_left (cdr table) (cons (car table) output  ) (+ counter 1) ))
    )
   ))

(define take_rigth
  (lambda (table counter)
    (display table)
    (cond
      ((empty? table) '())   
      ( (= counter (round (/ (length table) 2)) ) (cons (car table) (cdr table)) )
      (else (take_rigth (cdr table) (+ counter 1) ))
    )
   ))


(define cross_gens
  (lambda (gen1 gen2)
    (display gen1)
    (display gen2)
    (cond
      ((equal? (get_row gen1 (- (round (/ (length gen1) 2)) 1) 0) (get_row gen2 (- (round (/ (length gen2) 2)) 1 ) 0 ) ) (append (take_left gen1 '() 0 ) (take_rigth gen2 0)   )  )
      (else '())
      )
    ))

;;Se obtiene una nueva generacion realizando mezclas aleatorias

(define new_generation
  (lambda (population counter output)
    (cond
      ((= counter (round( / (length population)4))) output )
      (else (new_generation population (+ counter 1) (append (cross_gens (get_row population (random (length population))0) (get_row population (random (length population))0) ) output)))
    )
   ))

;;--------------------------------------------------------------------------------------------------------
