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

;; Pruebas (en Desarrollo)

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

;; Pruebas (en Desarrollo)





