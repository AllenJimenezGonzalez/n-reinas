(define generate_entity_aux
  (lambda (size counter output)
    (cond
      ((= size counter) output )
      (else (generate_entity_aux size (+ counter 1) (append output (cons (random size) '() )) ) )
      )
   ))

(define generate_entity
  (lambda (size)
    (generate_entity_aux size 0 '())
      ))

(define generate_population_aux
  (lambda (size counter output)
    (cond
      ((= size counter) output )
      (else (generate_population_aux size (+ counter 1) (cons (generate_entity size) output  ) ))
      )
 ))

(define generate_population
  (lambda (size)
    (generate_population_aux size 0 '())
      ))

(define  get_aptitute_rc
  (lambda (entity counter)
      (cond
        ((empty? entity) counter )
        ((member (car entity) entity ) #f )
        (else (get_aptitute_rc (cdr entity) counter ))
      )
      ))

(define get_aptitude
  (lambda (entity)
    (cond
      
      )
))
(define evaluate_population
  (lambda (population output)
    ()
      ))

(define cross_population
  (lambda (population output)
    ()
      ))