(define random_custom
  (lambda (rand size entity)
    (cond
      ((member rand entity) (random_custom (random size) size entity) )
      (else rand) 
    )
  )
)

(define generate_entity_aux
  (lambda (size counter output)
    (cond
      ((= size counter) output )
      (else (generate_entity_aux size (+ counter 1) (append output (cons (random_custom (random size) size output) '() )) ) )
      )
   ))

(define generate_entity
  (lambda (size)
    (generate_entity_aux size 0 '())
      ))

(define generate_population_aux
  (lambda (size counter output)
    (cond
      ((= 50 counter) output )
      (else (generate_population_aux size (+ counter 1) (cons (generate_entity size) output  ) ))
      )
 ))

(define generate_population
  (lambda (size)
    (generate_population_aux size 0 '())
      ))

(define get_value 
  (lambda (elem1 elem2)
    (abs(- elem1 elem2))
  )
)

(define get_aptitude
  (lambda (entity counter)
    (cond
      ((empty? entity ) 1000000)
      ((empty? (cdr entity)) counter )   
      (else (cond
              ((< (get_value (car entity) (cadr entity)) 2 ) ( get_aptitude (cdr entity) (+ counter 1) ) )
              (else ( get_aptitude (cdr entity) counter ) )
              ))
      )
))
 
(define get_elite
  (lambda (population elite)
    (cond
      ((empty? population) elite)
      (( < (get_aptitude (car population) 0 ) (get_aptitude elite 0)) (get_elite (cdr population) (car population) ) )
      (else (get_elite (cdr population) elite ))
    )
   ))

(define cross_population
  (lambda (population output)
    ()
 ))