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
;; down
;; Proposito:
;; (L) -> L' : Funcion que recibe un argumento:
;; Recibe una lista L y devuelve una nueva lista L', 
;; donde cada elemento de L está asociado a un nivel más de paréntesis 
;; comparado con su versión original en L.
;; 
;; <lista> := ()
;; <lista> := (<elemento> <lista>)
;; 
;; <elemento> := <valor>

(define down
  (lambda (L)
    (if (null? L)
        (list L)
        (if (null? (cdr L))
            (list L)
            (cons (list (car L)) (down (cdr L)))
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
;; (P L) -> n : La función list-index recibe dos argumentos: un predicado P y una lista L. 
;; La función busca el primer elemento en la lista L que satisface el predicado P 
;; y retorna su posición n (desde una posición inicial de 0).
;; Si no se encuentra ningún elemento que satisfaga el predicado, la función retorna #f.
;;
;; list-index-auxiliar
;; Proposito:
;; (indice predicado lista) -> n : La función list-index-auxiliar es una función auxiliar utilizada por la función principal list-index.
;; Recibe tres argumentos: un índice i, un predicado p y una lista l. La función busca el primer elemento en la lista l
;; que satisface el predicado p y retorna su posición (índice) n. Si no se encuentra ningún elemento que satisfaga el predicado,
;; la función retorna #f.
;;
;; <lista> := ()
;; <lista> := (<elemento> <lista>) 
;;
;; <elemento> := <valor>

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
;; Propósito:
;; (F L1 L2) -> L' : La función mapping recibe tres argumentos: una función unaria F y dos listas del mismo tamaño 
;; no vacias L1 y L2 que tienen numeros.
;; El propósito de esta función es retornar una lista de pares (a, b) donde 'a' es un elemento de L1 y 'b' es un elemento de L2
;; cumpliendo la propiedad de que al aplicar la función unaria F con el argumento 'a' se obtiene el número 'b'. 
;; Sigue la regla de que se debe cumplir que F(a) = b.
;; Advertencia: Las listas deben ser no vacias y de igual tamaño.

;; mapping-aux
;; Propósito:
;; (f l1 l2) -> L'' : La función mapping-aux es una función auxiliar utilizada por la función principal mapping.
;; Recibe tres argumentos: una función unaria f y dos listas de números l1 y l2. Su propósito es construir una lista de pares (a, b)
;; donde 'a' es un elemento de l1 y 'b' es un elemento de l2 siempre y cuando se cumpla la propiedad de que al aplicar la función unaria 'f'
;; con el argumento 'a' se obtenga el número 'b' y cuando no se cumple la propiedad crea un par vacio.
;; La lista resultante se denota como L''.
;;
;; mapping-empty-pair-clear
;; Propósito:
;; (lista-de-pares) -> L''' : La función mapping-empty-pair-clear es una función auxiliar utilizada por la función principal mapping.
;; Recibe una lista de pares (a, b) donde hay tambien pares vacíos  y su propósito es eliminar los pares vacíos de la lista, denotada como L'''.
;;
;; <lista> := ()
;; <lista> := (<elemento> <lista>)
;;
;; <lista-de-pares> := ()
;; <lista-de-pares> := (<elemento> <elemento>)
;;
;; <elemento> := empty
;;            := <int>

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
;; Propósito:
;; (F L1 L2) -> L' : La función zip recibe tres parámetros: una función binaria F y dos listas L1 y L2 ambas de igual tamaño.
;; Su propósito es retornar una lista L' donde la posición n-ésima corresponde al resultado de aplicar la función F sobre los elementos
;; en la posición n-ésima en L1 y L2.
;;
;; <lista-numeros> := ()
;;                 := (<int> <lista-numeros>)

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
;; Propósito:
;; (n BST) -> lista : La función path recibe dos argumentos: un número n y un árbol binario de búsqueda (BST)
;; representado como una lista de listas. El propósito de esta función es retornar una lista que representa la ruta desde el nodo raíz
;; del árbol hasta el número n indicada por las cadenas 'left' y 'right'. 
;; Si el número n se encuentra en el nodo raíz la función retorna una lista vacía.
;;
;; valor-nodo
;; Propósito:
;; (arbol) -> valor : La función valor-nodo es una función auxiliar utilizada para obtener el valor almacenado en el nodo raíz
;; del árbol 'arbol'.
;;
;; cons-end
;; Propósito:
;; (lista elemento) -> lista' : La función cons-end es una función auxiliar utilizada para agregar un elemento al final de la lista 
;; generando una nueva lista'.
;;
;; path-auxiliar
;; Propósito:
;; (numero arbol camino) -> lista : La función path-auxiliar es una función auxiliar utilizada por la función principal path.
;; Recibe tres argumentos: un número 'numero', un árbol 'arbol' y una lista 'camino'. Su propósito es buscar el número 'numero'
;; en el árbol 'arbol' y construir la lista 'camino' que representa la ruta desde el nodo raíz hasta el número 'numero' que se esta
;; buscando.
;; El camino se indica mediante las cadenas 'left' y 'right'.
;;
;; <arbol-binario> := (arbol-vacio) empty
;;                 := (nodo) <int> <arbol-binario> <arbol-binario>
;;
;; <camino> := (camino-vacio) empty
;;          := (lista-camino) <direccion> 
;;
;; <direccion> := () 
;;             := ('left) 
;;             := ('right)

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

;; Pruebas
(define mybst0 '()) ;;
(define mybst1 '(15                  ;;         (15)
                 (7                  ;;      /        \
                  (4 () ())          ;;    (7)       (25)
                  (10 (9 () ())      ;;    / \        / \  
                      (12 () ())))   ;;  (4) (10)   (17)(31)
                 (25                 ;;       / \        
                  (17 () ())         ;;     (9) (12)        
                  (31 () ()))
                 ))
(path 0 mybst0) ; Resultado esperado: nosta
(path 1 mybst0) ; Resultado esperado: nosta
(path 0 mybst1) ; Resultado esperado: nosta
(path 1 mybst1) ; Resultado esperado: nosta
(path 7 mybst1) ; Resultado esperado: (left)
(path 4 mybst1) ; Resultado esperado: (left left)
(path 17 mybst1) ; Resultado esperado: (right left) 
(path 9 mybst1) ; Resultado esperado: (left right left)
(path 12 mybst1) ; Resultado esperado: (left right right)
;; Pruebas del enunciado
(path 17 '(14 (7 () (12 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ())))) ; Resultado esperado: (right left left)


;; 17.
;; prod-scalar-matriz
;; Propósito:
;; (mat vec) -> lista : La función prod-scalar-matriz recibe dos argumentos: una matriz 'mat'
;; representada como una lista de listas y un vector 'vec' representado como una lista,
;; la longitud de las filas de mat' debe ser igual a la longitud del vector. 
;; El propósito de esta función es realizar la multiplicación de la matriz 'mat' por el vector 'vec'
;; y retornar el resultado como una lista.
;;
;; prod-vec
;; Propósito:
;; (vector1 vector2) -> lista : La función prod-vec es una función auxiliar utilizada 
;; por la función principal prod-scalar-matriz. 
;; Recibe dos vectores 'vector1' y 'vector2' representados como listas y tiene como 
;; propósito multiplicar elemento por elemento ambos vectores y retornar el resultado como una lista.
;;
;; <matriz> := ()                   
;;          := (<vector> <matriz>) 
;;
;; <vector> := ()                  
;;          := (<int> <vector>)  

(define prod-scalar-matriz
  (lambda (mat vec)
    
    (define prod-vec
      (lambda (vector1 vector2)
        (if (null? (cdr vector2))
            (list (* (car vector1) (car vector2)))
            (cons (* (car vector1) (car vector2)) (prod-vec (cdr vector1) (cdr vector2)))
            )
        )
      )

    (if (null? (cdr mat))
        (cons (prod-vec (car mat) vec) empty)
        (cons (prod-vec (car mat) vec) (prod-scalar-matriz (cdr mat) vec))
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