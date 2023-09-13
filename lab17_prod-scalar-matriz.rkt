#lang eopl

; Espacio de juegos con Racket que NO HACE PARTE DE LA ENTREGA FINAL

#| PUNTO 17
17) Elabore una función llamada (prod-scalar-matriz mat vec)
que recibe una matriz mat representada como una lista de listas
y un vector vec representado como una lista y retorna el resultado
de realizar la multiplicación matriz por vector.


> (prod-scalar-matriz ’((1 1) (2 2)) ’(2 3))
((2 3) (4 6))
> (prod-scalar-matriz ’((1 1) (2 2) (3 3)) ’(2 3))
((2 3) (4 6) (6 9))

|#

(define mat0 '((1 1)))
(define vec0 '(1 2 3 4))

; Casos prueba del enunciado
(define mat1 '((1 1) (2 2)))
#| 1 1
   2 2
|#
(define mat2 '((1 1) (2 2) (3 3)))
#| 1 1
   2 2
   3 3
|#

(define vec1 '(2 3))

#| Teoria (en Desarrollo...)
> (* (car (car mat1)) (car vec1))
2
|#

(define prod-scalar-matriz
  (lambda (mat vec)
    'a_ver_al_cine
  )
)

(define getFila
  (lambda (numero matriz)
    (if (= numero 1)
        (car matriz)
        (getFila (- numero 1) (cdr matriz))
     )
   )
 )

(define getFilas
  (lambda (matriz)
    (if (null? (cdr matriz))
        (list (car matriz))
        (cons (car matriz) (getFilas (cdr matriz)))
     )
    )
  )

(define prodVec
  (lambda (vector1 vector2)
    (if (null? (cdr vector1))
        (list (* (car vector1) (car vector2)))
        (cons (* (car vector1) (car vector2)) (prodVec (cdr vector1) (cdr vector2)))
     )
    )
  )