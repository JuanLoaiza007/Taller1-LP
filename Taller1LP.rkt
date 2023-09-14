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
(down '(4 5 6)) ; Resultado esperado: ((4) (5) (6))
(down '(g o k u u u u u)) ; Resultado esperado: ((g) (o) (k) (u) (u) (u) (u) (u)) 
(down '(algo (bien) ((pro)))) ; Resultado esperado: ((algo) ((bien)) (((pro))))
(down '((hola) (mund()))) ; Resultado esperado: (((hola)) ((mund ())))
;; Pruebas del enunciado
(down '(1 2 3)) ; Resultado esperado: ((1) (2) (3))
(down '((una) (buena) (idea))) ; Resultado esperado: (((una)) ((buena)) ((idea)))
(down '(un (objeto (mas)) complicado)) ; Resultado esperado: ((un) ((objeto (mas))) (complicado))


;; 5.
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
(list-index number? '(no tengo numeros)) ; Resultado esperado: #f
(list-index number? '(tengo numero en la 4 mira)) ; Resultado esperado: 4
(list-index symbol? '(1 2 3)) ; Resultado esperado: #f
(list-index symbol? '(0 1 2 tres 4 5)) ; Resultado esperado: 3
(list-index null? '()) ; Resultado esperado: #f
(list-index null? '(null?)) ; Resultado esperado: #f
(list-index null? '(null ... hey racket, s 0 y una lista vacia? )) ; Resultado esperado: #f
(list-index boolean? '(0)) ; Resultado esperado: #f
(list-index boolean? '(hey racket, tengo booleanos?)) ; Resultado esperado: #f
(list-index boolean? '(yo si mira #t jaja)) ; Resultado esperado: 3
;; Pruebas enunciado
(list-index number? '(a 2 (1 3) b 7)) ; Resultado esperado: 1
(list-index symbol? '(a 2 (1 3) b 7)) ; Resultado esperado: 0
(list-index symbol? '(1 2 (a b) 3)) ; Resultado esperado: #f


;; 8.
;; mapping
;; Proposito:
;; ...
;; Advertencia de listas no vacias y de igual tamaño

(define mapping
  (lambda (F L1 L2)

    (define mapping-aux
      (lambda (f l1 l2)
        (cond
          ((null? L1)
           empty)
          ((number? L1)
           (if (= (F L1) L2)
               (list L1 L2)
               empty))
          (else
           (cons (mapping F (car L1) (car L2)) (mapping F (cdr L1) (cdr L2))))
          )
        )
      )

    (define mapping-empty-pair-clear
      (lambda (lista-de-pares)
        (if (null? lista-de-pares)
            '()
            (if (null? (car lista-de-pares))
                (mapping-empty-pair-clear (cdr lista-de-pares))
                (cons (car lista-de-pares) (mapping-empty-pair-clear (cdr lista-de-pares)))))))

    (mapping-empty-pair-clear (mapping-aux F L1 L2))
    
    )
  )

;; Pruebas
(mapping (lambda (x) (+ x 1)) (list 1 2 3) (list 2 3 4)) ; Resultado esperado: ((1 2) (2 3) (3 4))
(mapping (lambda (x) (* x 2)) (list 2 4 6) (list 1 2 3)) ; Resultado esperado: ()
(mapping (lambda (x) (- x 1)) (list 5 10 15) (list 6 11 14)) ; Resultado esperado: ((15 14))
(mapping (lambda (x) (* x 3)) (list 2 4 6) (list 6 12 18)) ; Resultado esperado: ((2 6) (4 12) (6 18))
(mapping (lambda (x) (/ x 2)) (list 12 6 4) (list 2 3 4)) ; Resultado esperado: ((6 3))
;; Pruebas del enunciado
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6)) ; Resultado esperado: ((1 2) (2 4) (3 6))
(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6)) ; Resultado esperado: ((2 6))
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12)) ; Resultado esperado: ()


;; 11.
;; zip
;; Proposito:
;; ...

(define zip
  (lambda (F L1 L2)
    (if (null? L1)
        empty
        (if (number? L1)
            (F L1 L2)
            (cons (zip F (car L1) (car L2)) (zip F (cdr L1) (cdr L2)))))
    )
  )

;; Pruebas
(zip + '(1 2 3) '(4 5 6)) ; Resultado esperado: (5 7 9)
(zip - '(10 20 30) '(5 10 15)) ; Resultado esperado: (5 10 15)
(zip * '(2 3 4) '(5 6 7)) ; Resultado esperado: (10 18 28)
(zip / '(80 120 33) '(2 4 3)) ; Resultado esperado: (40 30 11)
(zip (lambda (x y) (+ (* 2 x) (* 3 y))) '(1 2 3) '(4 5 6)) ; Resultado esperado: (14 19 24)
;; Pruebas del enunciado
(zip + '(1 4) '(6 2)) ; Resultado esperado: (7 6)
(zip * '(11 5 6) '(10 9 8)) ; Resultado esperado: (110 45 48)


;; 14.
;; path
;; Proposito:
;; ...

(define path 
  (lambda (n BST)

    (define valor-nodo
      (lambda (tree)
        (if (null? (car tree))
            '()
            (car tree)
            )
        )
      )

    (define cons-end
      (lambda (lista elemento)
        (cond
          ((null? lista) elemento)       
          (else (cons (car lista)      
                      (cons-end (cdr lista) elemento)
                      )
                )
          )
        )
      )
    
    (define path-aux
      (lambda (numero arbol camino)
        
        (cond
          ((null? arbol) 'nosta)
          ((= numero (valor-nodo arbol)) camino) 
          ((< numero (valor-nodo arbol))
           (path-aux numero (cadr arbol) (cons-end camino '(left))))
          ((> numero (car arbol))
           (path-aux numero (caddr arbol) (cons-end camino '(right))))
          )
        
        )
      )
    
    (path-aux n BST '())
    )
  )

;; Pruebas (ALERTA XD)
;; Pruebas del enunciado
  (path 17 '(14 (7 () (12 () ()))
  (26 (20 (17 () ())
          ())
      (31 () ())))) ; Resultado esperado: (right left left)


;; 17.
;; prod-scalar-matriz
;; Proposito:
;; ...

(define prod-scalar-matriz
  (lambda (mat vec)
    
    (define prodVec
      (lambda (vector1 vector2)
        (if (null? (cdr vector2))
            (list (* (car vector1) (car vector2)))
            (cons (* (car vector1) (car vector2)) (prodVec (cdr vector1) (cdr vector2)))
            )
        )
      )

    (if (null? (cdr mat))
        (cons (prodVec (car mat) vec) empty)
        (cons (prodVec (car mat) vec) (prod-scalar-matriz (cdr mat) vec))
        )

    )
  )

;; Pruebas
(prod-scalar-matriz '((1 2 3)) '(2 3 4)) ; Resultado esperado: ((2 6 12))
(prod-scalar-matriz '((0 0 0) (0 0 0) (0 0 0)) '(2 3 4)) ; Resultado esperado: ((0 0 0) (0 0 0) (0 0 0))
(prod-scalar-matriz '((1 -2 3) (-4 5 -6) (7 -8 9)) '(-2 3 -4)) ; Resultado esperado: 
(prod-scalar-matriz '((15 25 35) (25 35 45) (35 45 55)) '(20 30 40)) ; Resultado esperado: ((300 750 1400) (500 1050 1800) (700 1350 2200))
;; Pruebas del enunciado
(prod-scalar-matriz '((1 1) (2 2)) '(2 3)) ; Resultado esperado: ((2 3) (4 6))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3)) ; Resultado esperado: ((2 3) (4 6) (6 9))