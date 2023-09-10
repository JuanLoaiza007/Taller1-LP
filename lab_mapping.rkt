#lang eopl

; Espacio de juegos con Racket que NO HACE PARTE DE LA ENTREGA FINAL

#| PUNTO 8
8) Elabore una función llamada mapping que debe recibir como
entrada 3 argumentos: una función unaria (que recibe un argumento) llamada
F, y dos listas de n´umeros L1 y L2. La función debe retornar una lista de
pares (a,b) siendo a elemento de L1 y b elemento de L2, cumpliendose la
propiedad que al aplicar la función unaria F con el argumento a, debe arrojar
el número b. Es decir, se debe cumplir que F(a) = b.

LAS LISTAS DEBEN SER DE IGUAL TAMAÑO

> (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
((1 2) (2 4) (3 6))
> (mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
((2 6))
> (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))
()

|#

; Casos prueba del enunciado
(define funar1 (lambda (d) (* d 2)))
(define funar2 (lambda (d) (* d 3)))
(define funar3 (lambda (d) (* d 2)))

(define l1 (list 1 2 3))
(define l11 (list 2 4 6))

(define l2 (list 1 2 2))
(define l22 (list 2 4 6))

(define l3 (list 1 2 3))
(define l33 (list 3 9 12))
; uso: (mapping (funar1 l1 l11))

#| Teoria
DUDA: ¿Hay que agregar una restricción por si las listas no
son del mismo tamaño?

...
|#

(define mapping
  (lambda (F L1 L2)
    'a_ver_al_cine
  )
)