#lang eopl
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;; TALLER #1 - FUNDAMENTOS DE COMPILACION E INTERPRETACION DE LP
;;
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;; INTEGRANTES
;;
;; Alejandro Guerrero Cano (2179652)- Alejo101102
;; alejandro.cano@correounivalle.edu.co
;;
;; Juan David Loaiza Santiago (2177570)- JuanLoaiza007
;; juan.loaiza.santiago@correounivalle.edu.co
;;
;; Juan Sebastian Muñoz Rojas (2177436)- sebastianmr18
;; juan.munoz.rojas@correounivalle.edu.co
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;; DESARROLLO

;; 2.
;; down :
;; Proposito:
;; (L) -> L' : Funcion que recibe un argumento:
;; Recibe una lista L y devuelve una nueva lista L', 
;; donde cada elemento de L está asociado a un nivel más de paréntesis 
;; comparado con su versión original en L.
;; 
;; <lista> := ()
;; <lista> := (<valor-de-scheme> <lista>)

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

;; Pruebas
(down '(4 5 6))
(down '(f l a n d e r s))
(down '(algo (bien) ((pro))))
(down '((hola) (mund())))
(down '((h) 0 ((la)) mund (1) llo))

;;  5.
;; list-index
;; Proposito:
;; (P, L) -> Resultado: Función que recibe dos argumentos: 
;; Un predicado P y una lista L, la función retorna (desde una posición inicial 0) 
;; el primer elemento de la lista que cumple con el predicado P.
;; Si no encuentra ningún elemento que cumpla retorna #f.
;; 
;; <lista> := ()
;; <lista> := (<valor-de-scheme> <lista>)

(define list-index
  (lambda (P L)

    (define list-index-auxiliar
      (lambda (indice predicado lista)
        (if (null? lista)
          #f
          (if (predicado (car lista))
              indice
              (list-index-auxiliar (+ indice 1) predicado (cdr lista))
          )
        )
       )
     )

    (list-index-auxiliar 0 P L)
  )
)

;; Pruebas
(list-index number? '(no tengo numeros))
(list-index number? '(tengo numero en la 4 mira))
(list-index symbol? '(1 2 3))
(list-index symbol? '(0 1 2 tres 4 5))
(list-index null? '())
(list-index null? '(null?))
(list-index null? '(null ... hey racket, s 0 y una lista vacia? ))
(list-index boolean? '(0))
(list-index boolean? '(hey racket, tengo booleanos?))
(list-index boolean? '(yo si mira #t jaja))





