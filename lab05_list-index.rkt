#lang eopl

; Espacio de juegos con Racket que NO HACE PARTE DE LA ENTREGA FINAL

#| PUNTO 5
Elabore una funcion llamada list-index que
debe recibir dos argumentos: un predicado P y una lista L.
La funcion retorna (desde una posicion inicial 0) el primer
elemento de la lista que satisface el predicado L.

Si llega a suceder que ningun elemento satisface el predicado recibido,
la funcion debe retornar #f.

> (list-index number? '(a 2 (1 3) b 7))
1
> (list-index symbol? '(a 2 (1 3) b 7))
0
> (list-index symbol? '(1 2 (a b) 3))
#f

|#

(define pred1 number?)
(define pred2 symbol?)
(define pred3 symbol?)

(define test1 '(a 2 (1 3) b 7))
(define test2 '(a 2 (1 3) b 7))
(define test3 '(1 2 (a b) 3))

#| Teoria
; Falla solo probando la cabeza
(pred1 (car test1))
#f

; Acierta al segundo elemento
(pred1 (cadr test1))
#t ; No es la respuesta que se busca xD

; Caso de lista vacía!!!
(pred1 '())
#f

;Programa que calcula la longitud de una lista - Clase 3
;list-length : List -> Int
;usage: (list-length l) = the length of l
; Con esta se cuenta la longitud, si se agrega un punto de parada (P (car L)) entonces cuenta hasta el indice
; del elemento que coincide
(define list-length
  (lambda (lst)
   (if (null? lst)
   0
   ; Aqui va mi condicion de parada nueva
   (+ 1 (list-length (cdr lst)))))) ; Esto se puede cambiar por recursión iterativa pa no volarme la cabeza
|#

(define list-index
  (lambda (P L)

    (define list-index-auxiliar
      (lambda (indice predicado lista)
        (if (null? lista)
          #f ; No esta, simplemente F
          (if (predicado (car lista))
              indice ; Lo encontreeeeeeeeeeeeeeeeeeeee, toma tu indice mi papacho
              (list-index-auxiliar (+ indice 1) predicado (cdr lista)) ; seguir buscando en la cola y cuenta un indice mas
          )
        )
       )
     )

    (list-index-auxiliar 0 P L)
  )
)
