;;--------------------------------------------------------------------------------------------------------
;;Create initial population
;;--------------------------------------------------------------------------------------------------------

;Domain   : "n" is an counter
;           "row" is paramter to return the result
;Codomain : return a row fullfilled with '*'
(define generate_row
  (lambda (n row)
    (cond
      ((zero? n) row )
      (else (generate_row (- n 1) ( append row '(*) ) ) ) 
      )
    ))
;Domain   : "n" is an counter
;           "m" is an position 
;           "row" is paramter to return the result         
;Codomain : return a matrix fullfilled with '*'
(define generate_column
  (lambda (n m table)
    (cond
      ((zero? n) table  )
      (else (generate_column (- n 1) m (cons (generate_row m '() ) table ) ) )  
      )
    ))
;Domain   : "n" is an size of matrix    
;Codomain : return a matrix fullfilled with '*'
(define generate_table
  (lambda (n)
    (generate_column n n '() )
    ))
;Domain   : "list" is an row filled with "*"
;         : "x" a position of column that find the item to edit
;Codomain : return a row with an item on random position
(define edit_row 
  (lambda (list x item)
  (cond
    ((empty? list) '())
    ((zero? x)     (cons item (cdr list)))
    (else (cons (car list) (edit_row (cdr list) (- x 1) item)) )
   )
 ))
;Domain   : "table" is an matrix filled with "*"
;         : "x" is a position of row that find the item to edit
;         : "y" is a position of column that find the item to edit
;Codomain : return a matrix with an item on random position
(define edit_table
  (lambda (table x y item)
  (cond
    ((empty? table) '())
    ((zero? x) (cons (edit_row (car table) y item) (cdr table)))
    (else (cons (car table) (edit_table(cdr table) (- x 1) y item)))
    )
  ))

;Domain   : "n" is a size of matrix to make a randon on this range
;         : "counter" a position of column that find the item to edit
;         : "table" is an chest table 
;Codomain : return a matrix with a items on random positions

(define random_queens_aux
  (lambda (n counter table)
    (cond
      ((zero? counter) table)
      (else (random_queens_aux n (- counter 1) (edit_table table (random n) (random n) '♕ ) ))
    ) 
   ))
;Domain   : "n" is a size of matrix and number of queens
;Codomain : return a matrix with a items on random positions
(define random_queens
  (lambda (n)
    (random_queens_aux n n (generate_table n) )
  ))

;Domain   : "size" is a how many chest tables generate
;         : "psize" is a size of each 'n*n' matrix
;         : "output" is a variable to store result of matrix
;Codomain : return population of chest tables

(define get_population  
  (lambda (size psize output)
    (cond
      ((zero? size ) output)
      (else (get_population (- size 1) psize (cons (random_queens psize) output  ) ) )
      )
      ))

;;--------------------------------------------------------------------------------------------------------
;;Chromosomes evaluation
;;--------------------------------------------------------------------------------------------------------

;Domain   : "table" is an matrix where we will get an row
;         : "row" refers a index position of element
;         : "counter" is a variable find the same index on list
;Codomain : return a selected row where have the same index 

(define get_row
  (lambda (table row counter)
    (cond
      ((= row counter) (car table))
      (else (get_row (cdr table) row (+ counter 1) ) )
      )
    )
  )

;Domain   : "row" refers a row of table that we will get a column
;         : "counter" is a variable to kwnow a actual index on row
;         : "position" is a position of searched index
;         : "element" is a variable to store the result
;Codomain : return a selected element of row - column selected 

;;Note: The counter need be initiated on -1
(define get_column_element_aux
  (lambda (row counter position element)
    (cond
      ((= counter position) element)
      ((pair? row ) (get_column_element_aux (cdr row) (+ counter 1) position (car row) ) )
      (else (get_column_element_aux row (+ counter 1) position row ) )
      )
   )
  )

;Domain   : "table" is an chest table matrix
;         : "counter" is a variable to kwnow a actual index on row
;         : "position" is a position of searched index
;         : "columns" is a list that have each column
;         Example:
;         (A B C)
;         (D E F)
;         (G H I)
;         columns OUTPUT -> (A D G)
;Codomain : return a list with a elements that have the same index on column

(define get_column_aux
  (lambda (table counter position columns)
    (cond
      ((empty? table) columns)
      (else (get_column_aux (cdr table) (+ counter 1) position (append columns (cons (get_column_element_aux (car table) -1 position 0 )   '() ) )  )  )    
    )
  ))

;Domain   : "table" is an chest table matrix
;         : "counter" is a variable to kwnow a actual index on row
;         : "output" is a variable to store the result
;Example:
;         (A B C)
;         (D E F)
;         (G H I)
;         columns OUTPUT -> ( (A D G) (B E H) (C F I) )
;Codomain : return a list with all columns by index on group

(define get_columns
  (lambda (table counter output)
    (cond
      ((= counter (length table)) (reverse output) )
      (else (get_columns table (+ counter 1) (cons  (get_column_aux table -1 counter '() ) output  ) )   )
      )
    
  ))
;Domain   : "table" is an chest table matrix
;Example: (1 2 0 1) a row evaluated where have a 4 queens on 4*4 matrix
;Codomain : return a number list of quantity queen by each column

(define evaluate_colum_aux
  (lambda (table)
    (cond
      ((empty? table) 0)
      ((equal? (car table) '♕) (+ 1 (evaluate_colum_aux (cdr table) )))
      (else (evaluate_colum_aux (cdr table)))
    )
  ))

;Domain   : "table" is an chest table matrix
;         : "x" refers a row position
;         : "y" refers a column position
;         : "fun1" refers to + or - functions to move along chest table
;         : "fun2" refers to + or - functions to move along chest table
;         : "output" is an variable for store the result
;Codomain : return a elements from selected diagonal

(define get_diagonal_aux
  (lambda (table x y fun1 fun2 output)
    (cond
      ((= x (length table)) (reverse output) ) 
      (else (get_diagonal_aux table (fun1 x 1) (fun2 y 1) fun1 fun2 (cons (get_column_element_aux (get_row table x 0) -1 y 0) output   )  ) )
      )
    ))

;Domain   : "table" is an chest table matrix
;         : "x" refers a row position
;         : "y" refers a column position
;         : "fun1" refers to + or - functions to move along chest table
;         : "fun2" refers to + or - functions to move along chest table
;         : "output" is an variable for store the result
;Codomain : return a elements from selected diagonal

(define get_diagonal_aux_y
  (lambda (table x y fun1 fun2 output)
    (cond
      (( > 0 y ) (reverse output) )
      (else (get_diagonal_aux_y table (fun1 x 1) (fun2 y 1) fun1 fun2 (cons (get_column_element_aux (get_row table x 0) -1 y 0) output   )  ) )
      )
    ))

;Domain   : "table" is an chest table matrix
;         : "x" refers a row position
;         : "y" refers a column position
;         : "output" is an variable for store the result
;Codomain : return a selected elements from diagonal, where start from left to rigth and get bottom
;Example:
;         (A B C)
;         (D E F)
;         (G H I)
;         OUTPUT -> ( (A E I) (D H) (G) )

(define get_bottom_left_diagonal
  (lambda (table x y output)
    (cond
      ((= (length table) x) (reverse output))
      (else ( get_bottom_left_diagonal table (+ x 1) y (cons ( get_diagonal_aux table x y + + '() ) output )) )
      )
    )
  )

;Domain   : "table" is an chest table matrix
;         : "x" refers a row position
;         : "y" refers a column position
;         : "output" is an variable for store the result
;Codomain : return a selected elements from diagonal, where start on left upper
;Example:
;         (A B C)
;         (D E F)
;         (G H I)
;         OUTPUT -> ( (C) (B F) )

;;Note: the "y" parameter need sended with value 1 beacuse the identity diagonal is contemplated on bottom diagonal
(define get_upper_left_diagonal
  (lambda (table x y output)
    (cond
      ((= (length table) y) output)
      ((equal? (pair? table) #f ) output  )
      (else ( get_upper_left_diagonal table x (+ y 1) (cons ( get_diagonal_aux table x y + + '() ) output )) ) 
      )
    )
  )

;Domain   : "table" is an chest table matrix
;Codomain : return all diagonals starting on left al fianalize on right
;Example:
;         (A B C)
;         (D E F)
;         (G H I)
;         OUTPUT -> ( (C) (B F) (A E I) (D H) (G) )

(define get_left_diagonal
  (lambda (table)
    (append (get_upper_left_diagonal table 0 1 '()) (get_bottom_left_diagonal table 0 0 '() )  )
   ))

;Domain   : "table" is an chest table matrix
;         : "x" refers a row position
;         : "y" refers a column position
;         : "output" is an variable for store the result
;Codomain : return a selected bottom elements from diagonal, where start from rigth to left
;Example:
;         (A B C)
;         (D E F)
;         (G H I)
;         OUTPUT -> ( (C E G) (F H) (I) )

;;Note: need to be sended y-1 as parameter on "y" by default -> (length table ) - 1

(define get_bottom_rigth_diagonal
  (lambda (table x y output)
    (cond
      ((= x (length table)) (reverse output) )
      (else (get_bottom_rigth_diagonal table (+ x 1) y (cons ( get_diagonal_aux table x y + - '() ) output ) ))
      )
      ))

;Domain   : "table" is an chest table matrix
;         : "x" refers a row position
;         : "y" refers a column position
;         : "output" is an variable for store the result
;Codomain : return a selected upper elements from diagonal, where start from rigth to left
;Example:
;         (A B C)
;         (D E F)
;         (G H I)
;         OUTPUT -> ( (A) (B D)  )

;; The paramter "y" need substract 2 -> (Length table)-2

(define get_upper_rigth_diagonal
  (lambda (table x y output)
    (cond
      ((< y 0) output)
      (else (get_upper_rigth_diagonal table x (- y 1) (cons (get_diagonal_aux_y table x y + - '() ) output )  ) )
      )
    ))
;Domain   : "table" is an chest table matrix
;Codomain : return all diagonals starting on rigth and finalize on left
;Example:
;         (A B C)
;         (D E F)
;         (G H I)
;         OUTPUT -> ( (A) (B D) (C E G) (F H) (I) )

(define get_right_diagonal
  (lambda (table)
    (append (get_upper_rigth_diagonal table 0 (- (length table) 2 ) '() ) (get_bottom_rigth_diagonal table 0 (- (length table) 1 ) '() ) )
    ))

;Domain   : "results" is a list with a elements from diagonals, columns or rows
;Codomain : return a list with numbers, this count each queen by diagonal, column or row

(define evaluate_get_score
  (lambda (results)
    (cond
      ((empty? results) 0)
      ((equal? (car results) '♕) (+ 1 (evaluate_colum_aux (cdr results) )))
      (else (evaluate_colum_aux (cdr results)))
    
      )
    ))

;Domain   : "results" is a list with a elements from diagonals, columns or rows
;Codomain : return a list with numbers, this apply "evaluate_get_score" function for each element of diagonal, column or row

(define evaluate_data
  (lambda (results)
    (map (lambda (x) (evaluate_get_score x)) results)
    ))

;Domain   : "table" is an chest table matrix
;Codomain : return a list with numbers that counts each queen on a chess table, by row, columns or diagonals and then append all results 

(define evaluate
  (lambda (table)
    (append
    (evaluate_data table )
    (evaluate_data (get_columns table 0 '() ))
    (evaluate_data (get_left_diagonal table))
    (evaluate_data (get_right_diagonal table))
    )
   ))

;;--------------------------------------------------------------------------------------------------------
;;Genome crossover
;;--------------------------------------------------------------------------------------------------------

;Domain   : "child" is an chess table part of population
;Codomain : return a score of number list that counts each queen

(define evaluate_new_gen
  (lambda (child)
    (apply + (evaluate_data child))
  ))

;Domain   : "gen1" is an object from population
;         : "gen2" is an object from population
;Codomain : return a cross genome where have a cross point in common between gen1 and gen2

(define cross_gens
  (lambda (gen1 gen2)
    (cond
      ((equal? (get_row gen1 (- (round (/ (length gen1) 2)) 1) 0)
               (get_row gen2 (- (round (/ (length gen2) 2)) 1 ) 0 ) )
               (append (get_group gen1 'l (round ( / (length gen1) 2)) ) (get_group gen2 'r (round ( / (length gen2) 2))    )  ))
      
      (else '())
      )
    ))

;Domain   : "input" is a list to be cleared
;         : "output" is an variable to store result
;Codomain : return a cleared list without "()"

(define clean_results
  (lambda (input output)
    (cond
      ((empty? input) output)
      (else(cond
             ((pair? (car input)) (clean_results (cdr input) (append (cons (car input) '() ) output ) )  )
             (else (clean_results (cdr input) output ) )
             ))
      )
  ))

;Domain   : "population" is a initial popupulation that will be filtered to get a new generation
;         : "counter" is an variable to get a counter to knwon how many times need crossing attemps
;         : "output" is an variable to store result
;Codomain : return a new generation population filtered

(define new_generation_aux
  (lambda (population counter output)
    (cond
      ((= counter 400) (clean_results output '() ) )
      (else (new_generation_aux population (+ counter 1) (cons (cross_gens (get_row population (random (length population)) 0) (get_row population (random (length population))0) ) output)))
      )
   ))

;Domain   : "population" is a initial popupulation that will be filtered to get a new generation
;Codomain : return a new generation population filtered

(define new_generation
  (lambda (population)
    (new_generation_aux population 0 '())
      ))

;Domain   : "population" is a initial popupulation that will be filtered to get a new generation
;         : "pos" is an index position of chess table
;Codomain : return a chess table on determinated position

(define get_table
  (lambda (population pos)
    (cond
      ((empty? population) '() )
      ((= pos 0) (car population)) 
      (else (get_table (cdr population) (- pos 1) ))
    )
   ))

;Domain   : "population" is a initial popupulation that will be filtered to get a new generation
;         : "orientation" is a variable that defines if get left part or rigth part of a list
;         : "counter" is a variable that carries a index
;         : "pos" is a variable that indicates where will be cut the list
;         : "output" is a variable to store a result
;Codomain : return a left or rigth part of list

(define get_group_aux
  (lambda (population orientation counter pos output)
    (cond
      ((equal? orientation 'r ) (cond
                                  ((= counter pos) (append (cons (car population) '() ) (cdr population) ) ) 
                                  (else(get_group_aux (cdr population) orientation (+ counter 1) pos '() ))
                                  ))
      (else (cond
              ((= counter pos) (reverse output) ) 
              (else (get_group_aux (cdr population) orientation (+ counter 1) pos (cons (car population) output ) ))
              ))
   )
 ))

;Domain   : "population" is a initial popupulation that will be filtered to get a new generation
;         : "orientation" is a variable that defines if get left part or rigth part of a list
;         : "pos" is a variable that indicates where will be cut the list
;Codomain : return a left or rigth part of list

(define get_group
  (lambda (population orientation pos)
    (get_group_aux population orientation 0 pos '())
 ))

;Domain   : "population" is a initial popupulation that will be filtered to get a new generation
;         : "rand" is a variable that have a random number that will be chaged on a position and that defines wher cut left and rigth part
;Codomain : return a new chess table mutated

(define mutate_gen_aux
  (lambda (population rand )
    (append (get_group population 'l rand ) (cons (random_queens_aux (length population) (length population) (get_table population rand ) ) '() ) (get_group population 'r rand ))
   ))

;Domain   : "population" is a initial popupulation that will be filtered to get a new generation
;         : "counter" is a variable that define how many mutations you need to do
;Codomain : return a new chess table mutated

(define mutate_gen
  (lambda (population counter )
    (cond
      ((empty? population) '())
      ((pair? (car population))(cond
                              (( = counter ( + (round( * (length population) 0.05)) 1)) population)  
                              (else (mutate_gen (mutate_gen_aux population (random (length population))) (+ counter 1) ))  
                              ))
      (else (mutate_gen (cdr population) counter  ) )
      
    )
   ))

;Domain   : "results" is a number list that have a score of each chess table
;Codomain : return boolean if whether the child is defective or not

(define evaluate_results
  (lambda (results)
    (cond
      ((empty? results) #t)
      ((> (car results) 1) #f)
      (else(evaluate_results (cdr results)))
    )
   ))

;Domain   : "table" is a chess table
;Codomain : return a new table with a new population

(define get_final_population 
  (lambda (table)
    (append (mutate_gen ( new_generation table ) 0) table )
  ))

;Domain   : "n" is a number of queens and a size of matrix (chess table)
;Codomain : return a new list with fitness function and mutated chess table

(define geneticNQueens_aux
  (lambda (n)
    (get_final_population (get_population n n '()) ) 
 ))

;Domain   : "population" is group of chess tables possibilities
;         : "champion" is a variable that store the best element of population
;Codomain : return the best element of a population

(define evaluate_population
  (lambda (population champion)
    (cond
      ((empty? population) champion )
      ((equal? (evaluate_results (evaluate (car population)) ) #t ) ( evaluate_population (cdr population) (car population)  ) )
      (else (evaluate_population (cdr population) champion  ))
    )
 ))

;Domain   : "list" is a list of score numbers that define the aptitude of element
;         : "quantity" is a variable that stores the score.
;Codomain : return a sum of score list.

(define queens_quantity_filter
  (lambda (list quantity)
    (cond
      ((= (apply + list) quantity)#t)
      (else #f)
    )
   ))

;Domain   : "championchild" is a elite chess table that maybe contains the solution
;Codomain : return true if the elite element is a solution

(define geneticNQueens_final
  (lambda (championchild)
    (display "\n")
    (display championchild )
    (display "\n")
    (cond
      ((equal? championchild '() ) #f)
      ((equal? (queens_quantity_filter (evaluate_data championchild ) (length championchild)  ) #t ) #t )
      (else #f) 
    )
  ))

;Domain   : "n" is a size of chess table and quantity of queens on the table
;Codomain : return true if is a solution else it is called recursively until a solution is found

(define geneticNQueens
  (lambda (n)
    (cond
      ((equal? (geneticNQueens_final(evaluate_population (geneticNQueens_aux n) '()  ) ) #t) #t )
      (else (geneticNQueens n))
    )
  ))

