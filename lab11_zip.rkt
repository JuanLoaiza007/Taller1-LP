#lang eopl

; Espacio de juegos con Racket que NO HACE PARTE DE LA ENTREGA FINAL

#| PUNTO 11
11) Elabore una función llamada zip que recibe como entrada tres
parámetros: una función binaria (función que espera recibir dos argumentos)
F, y dos listas L1 y L2, ambas de igual tamaño. El procedimiento zip
debe retornar una lista donde la posición n-ésima corresponde al resultado
de aplicar la función F sobre los elementos en la posición n-ésima en L1 y
L2.

> (zip + ’(1 4) ’(6 2))
(7 6)
> (zip * ’(11 5 6) ’(10 9 8))
(110 45 48)

|#

; Casos prueba del enunciado
(define l1 '(1 4))
(define l11 '(6 2))

(define l2 '(11 5 6))
(define l22 '(10 9 8))
; uso: (zip + l1 l11)

#| Teoria
;; DUDA: ¿Hay que agregar una restricción por si las listas no
;; son del mismo tamaño?

; Hay que usar lo mismo que antes pero diferente

> (+ 1 2)
3

> (cons (+ 1 2) empty)
(3)

> (cons (+ 1 2) (cons (+ 1 4) empty))
(3 5)

> (cons (+ 1 2) (cons (+ 1 4) (cons (+ 2 6) empty)))
(3 5 8) 
|#

; v1, me estoy volviendo el durisimo de Racket siuuuuuuuuuuuuuuu

(define zip
  (lambda (F L1 L2)
    (if (null? L1)
        empty
        (if (number? L1)
        (F L1 L2)
        (cons (zip F (car L1) (car L2)) (zip F (cdr L1) (cdr L2)))))
  )
)