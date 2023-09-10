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

... 
|#

(define list-index
  (lambda (P L )
    (if (null? L)
        #f
        (if (P (car L))
            0
            'a_ver_al_cine))
    )
  )
