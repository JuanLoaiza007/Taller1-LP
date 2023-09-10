#lang eopl

; Espacio de juegos con Racket que NO HACE PARTE DE LA ENTREGA FINAL

#| PUNTO 2
2) Elabore una funcion llamada down que
recibe como argumento una lista L,
y lo que debe realizar dicha funcion es
retornar una lista con cada elemento de L asociado a un nivel más de
parentesis comparado con su estado original en L.

> (down ’(1 2 3))
((1) (2) (3))

> (down ’((una) (buena) (idea)))
(((una)) ((buena)) ((idea)))

> (down ’(un (objeto (mas)) complicado))
((un) ((objeto (mas))) (complicado))
|#

(define prueba1 '(1 2 3))
(define prueba2 '((una) (buena) (idea)))
(define prueba3 '(un (objeto (mas)) complicado))

#| Teoria
> (cons (cons 1 empty)
        (cons (cons 2 empty) empty))
((1) (2))

(cons (cons 1 empty)
        (cons (cons 2 empty)
              (cons (cons 3 empty) empty)))
((1) (2) (3))
|#

(define down
  (lambda (l)
    (if (null? l)
        (list l)
        (if (null? (cdr l))
            (list l)
            (cons (list (car l)) (down (cdr l)))
        )
    )
  )
)