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
DUDA: ¿Hay que agregar una restricción por si las listas no
son del mismo tamaño?

...
|#

(define zip
  (lambda (F L1 L2)
    'a_ver_al_cine
  )
)