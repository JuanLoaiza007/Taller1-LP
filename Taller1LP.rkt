#lang eopl
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;; TALLER #1 - FUNDAMENTOS DE COMPILACION E INTERPRETACION DE LP


;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;; INTEGRANTES 
; Alejandro Guerrero Cano (2179652)- Alejo101102
; alejandro.cano@correounivalle.edu.co

; Juan David Loaiza Santiago (2177570)- JuanLoaiza007
; juan.loaiza.santiago@correounivalle.edu.co

; Juan Sebastian Muñoz Rojas (2177436)- sebastianmr18
; juan.munoz.rojas@correounivalle.edu.co


;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;; DESARROLLO


#|1.

invert

Proposito:
L -> L' : Procedimiento que recibe una entrada:
Una lista L, la cual se compone de pares x,y que a
su vez son listas (de tamaño 2). Retorna una lista
L' con pares ordenado invertidos, es decir y,x. 


<lista-de-pares> ::= ()
                 ::= (<par> <lista-de-pares>)

<par> ::= (<elemento> <elemento>)

<elemento> ::= <entero>
           ::= <cadena>
|#

(define invert
  ( lambda (l)
     (if (null? l)
         '()
         (cons (list (cadr (car l)) (car (car l))) (invert (cdr l))))
      )

   )

;;Pruebas
(invert '()) ;Retorna una lista vacía
(invert '((4 5) (7 6))) ;Retorna la lista de pares ((5 4) (6 7))
(invert '((8 7)("Cali" "Deportivo")(e o))) ;Retorna la lista de pares ((7 8) ("Deportivo" "Cali") (o e)) 
(invert '((5 4)(7 6)(9 8))) ;Retorna la lista de pares ((4 5) (6 7) (8 9))
(invert '(("República" "La")("Colombia" "De"))) ;Retorna la lista de pares (("La" "República") ("De" "Colombia"))


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


#| 3. 
list-set
Proposito:
(L, n, x)--> L' : Procedimiento que recibe tres argumentos:
una lista L, un numero n y un elemento x. Cuyo retorno es una lista
L' que intercambia en la posición n el elemento x.

<lista> := ()
<lista> := (<valor-de-scheme> <lista>)
|#

(define list-set
  (lambda (L n x)
    (cond
     ((null? L) empty)
     ((zero? n) (cons x (cdr L)))
     (else (cons (car L) (list-set (cdr L) (- n 1) x))))))


;Pruebas
(list-set '() 1 3) ; retorna ()
(list-set '(5 8 9 0 1) 0 3) ; retorna (3 8 9 0 1)
(list-set '(a b c d) 1 'a) ; retorna (a a c d)
(list-set '(a 2 4 g) 1 '(1 2)) ; retorna (a (1 2) 4 g)
(list-set '(a b c d) 3 '(1 5 10)) ; retorna (a b c (1 5 10))


#|4.
filter-in

Proposito:
(P, L) -> L' : Procedimiento que recibe dos entradas:
Un predicado P y una lista L. Retorna una lista que contiene 
los elementos que pertenecen a L y que satisfacen el predicado P.

<lista-filtro> ::= ()
               ::= (<elemento> <lista-filtro>)

<elemento> ::= (<predicado> <valor> <elemento>)

<predicado> ::= <symbol?>
            ::= <number?>
            ::= <string?>

<valor> ::= <symbol>
        ::= <number>
        ::= <string>
|#


(define filter-in
  (lambda (P L)
    (cond
      [(null? L) '()]
      [(P (car L)) (cons (car L) (filter-in P (cdr L)))]
      (else (filter-in P (cdr L))))))

;;Pruebas
(filter-in number? '(1 "hola" (4 3) z 7)) ;Retorna la lista (1 7)
(filter-in number? '(3 (a 0) x 5 "Colombia")) ;Retorna la lista (3 5)
(filter-in symbol? '(a t (d 9) m e (7 "hola") "Cali")) ;Retorna la lista (a t m e)
(filter-in symbol? '(v i 8 w ("mundo" r) "hola" 9 t)) ;Retorna la lista (v i w t)
(filter-in string? '("Messi" 9 a ("Jupiter" a) 7 k "Ronaldo" "Ronaldinho")) ;Retorna la lista ("Messi" "Ronaldo" "Ronaldinho")
(filter-in string? '(9 a ("Jupiter" a) 7 k 8 g)) ;Retorna una lista vacía


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

#| 6. 
swapper
Proposito:
(E1, E2, L)--> L' : Procedimiento que recibe tres argumentos:
un elemento E1, un elemento E2 y una lista L. Cuyo retorno es una lista
L' que intercambia cada E1 por E2, y cada E2 por E1 en L.

<lista> := ()
<lista> := (<valor-de-scheme> <lista>)
|#

(define swapper
  (lambda (E1 E2 L)
    (cond
      ((null? L) empty)
      ((equal? E1 (car L)) (cons E2 (swapper E1 E2 (cdr L))))
      ((equal? E2 (car L)) (cons E1 (swapper E1 E2 (cdr L))))
      (else (cons (car L) (swapper E1 E2 (cdr L)))))))

;Pruebas
(swapper 'a 'b '()) ; retorna ()
(swapper 'a 'b '(a b c d e)) ; retorna (b a c d e)
(swapper '3 '6 '(1 2 3 4 5 6 7 8)) ; retorna (1 2 6 4 5 3 7 8)
(swapper 'ab 'ba '(a2 ab ba b2 aa bb)) ; retorna (a2 ba ab b2 aa bb)
(swapper 'x 'y '(x y y x x x y y y y x h x xy yx xx y yx x)) ; retorna (y x x y y y x x x x y h y xy yx xx x yx y)


#|7

cartesian-product
Proposito:
(L1, L2) -> L' : Procedimiento que recibe dos entradas:
Recibe 2 listas de śımbolos sin repeticiones L1 y L2. Retorna
una lista de tuplas L' con el producto cartesiano entre L1 y L2.

cartesian-product-aux
Proposito:
(l1, l2, rest) -> l': procedimiento auxiliar que recibe tres argumentos:
Recibe dos listas de símbolos sin repeticiones, l1 y l2, y una lista de tuplas rest.
Retorna una lista de tuplas l' que representa el producto cartesiano entre las listas l1 y l2.

<lista-cartesiana> := ()
                   := (<producto> <lista-cartesiana>)

<producto> ::= ()
           ::= (<valor> <valor> <producto>)

<valor> ::= <symbol>
        ::= <number>

|#


(define cartesian-product
  (lambda (L1 L2)
    (letrec ((cartesian-product-aux
              (lambda (l1 l2 rest)
                (cond
                  ((null? l1) rest)
                  ((null? l2) (cartesian-product-aux (cdr l1) L2 rest))
                  (else
                   (cons (list (car l1) (car l2))(cartesian-product-aux l1 (cdr l2) rest))
                   )
                  )
                )
              ))
      (cartesian-product-aux L1 L2 '()))
    )
  )

;;Pruebas
(cartesian-product '() '(a)) ; Retorna la lista vacía ()
(cartesian-product '(a w y o) '(9 3 a 2)) ; Retorna la lista de tuplas ((a 9) (a 3) (a a) (a 2) (w 9) (w 3) (w a) (w 2) (y 9) (y 3) (y a) (y 2) (o 9) (o 3) (o a) (o 2))
(cartesian-product '(t r) '(u i o)) ; Retorna la lista de tuplas ((t u) (t i) (t o) (r u) (r i) (r o))
(cartesian-product '(v b n) '(s d)) ; Retorna la lista de tuplas ((v s) (v d) (b s) (b d) (n s) (n d))
(cartesian-product '(p) '(a b c d)) ; Retorna la lista de tuplas ((p a) (p b) (p c) (p d))

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


#| 9.
inversions
Proposito:
(L) --> Int: Procedimiento que recibe una lista de números L=(a1, a2, ..., an),
esta realiza el conteo de las veces que un número ai > aj, siempre que i < j
(es decir que haya un número mayor a otro que esta ubicado antes del segundo
número en la lista), esto lo realiza con ayuda de las funciones auxiliares
count y count-greater.

--Funciones auxiliares:
count
Proposito:
(L, accumulator) --> Int: Procedimiento que toma la lista L y el acumulador de inversiones,
se encarga de recorrer la lista L, guardando el elemento actual en 'current' e invocando
a count-greater para contar cuantos elementos son mayores a 'current', y el conteo se
guarda en acumulador hasta recorrer toda L.

count-greater
Proposito:
(x, L) --> Int:Procedimiento que toma un valor x, que es el valor con el que se compara y la
lista L en la que se realiza la busqueda, esta función compara x con cada elemento de L,
y cada que x es mayor agraga 1 al acumulador.

<lista-numeros> := (<int> <lista-numeros>)
|#
(define inversions
  (lambda (L)
    (define count
      (lambda (L accumulator)
        (cond
          ((null? L) accumulator)
          (else
           (let ((current (car L)))
           (count (cdr L)
           (+ accumulator (count-greater current (cdr L)))))))))
  
    (define count-greater
      (lambda (x L)
        (cond
          ((null? L) 0)
          ((> x (car L)) (+ 1 (count-greater x (cdr L))))
          (else (count-greater x (cdr L))))))
    (count L 0)
  )
)

(inversions '()) ; retorna 0
(inversions '(1)) ; retorna 0
(inversions '(2 3 8 6 1)) ; retorna 5
(inversions '(1 2 3 4))  ; retorna 0
(inversions '(3 2 1)) ; retorna 3
(inversions '(1 3 2 4 3 5 4 6 5)) ; retorna 4


#|10

up:
Proposito:
L -> L' : Procedimiento que recibe una entrada:
Una lista L lo que debe realizar en la funcion es remover
un par de paréntesis a cada elemento del nivel más alto
de la lista. Si un elemento de este nivel no es una lista
no tiene paréntesis), este elemento es incluido en la salida
resultante sin modificación alguna. Retorna una lista L'.

up-aux:
Proposito:
(l1, l2) -> l' : Procedimiento que recibe dos entradas:
Una lista l1 y otra l2 lo que debe realizar en la funcion es remover
un par de paréntesis a cada elemento del nivel más alto
en l1 y luego concatenar los elementos resultantes con l2. Teniendo
como resultante una lista l'.

<lista-up> := ()
           := (<valor-de-scheme> <lista-up>)
 
|#

(define up
  (lambda (L)
    (define (up-aux l1 l2)
      (cond
        ((null? l1) l2)
        (else (cons (car l1) (up-aux (cdr l1) l2)))))
    
    (cond
      ((null? L) '())
      ((list? (car L)) (up-aux (car L) (up (cdr L))))
      (else (cons (car L) (up (cdr L)))))))

;;Pruebas
(up '((a ((w)((y)))) d)) ; Retorna la lista (a ((w) ((y))) d)
(up '(((a) ((b))) c)) ; Retorna la lista ((a) ((b)) c)
(up '(((q w)) ((e r)))) ; Retorna la lista ((q w) (e r))
(up '(((z) (x)) (((c)) ((v))))) ; Retorna la lista ((z) (x) ((c)) ((v)))
(up '((a s) (d f))) ; Retorna la lista (a s d f)


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


#| 12.
filter-acum
(a b F acum filter) --> Int: Procedimiento que recibe dos números a y b, una función
binaria F, un valor inicial acum y una función unitaria filter, esta aplica la función
F a los enteros que estan en el intervalo [a, b] y que a su vez cumplen con la función
filter, luego esto lo retorna acumulandolo en acum, que actua como un valor inicial.

--Funciones auxiliares
iterator
(i, accumulator) --> Int: Procedimiento auxiliar que realiza la iteración y el calculo
acumulativo en el rango de 'a' a 'b', aplicando 'filter' a cada elemento del rango
y a los que pasen filtro aplicarles 'F' ademas de acumular este resultado y retornarlo
al final.

<a> := <int>
<b> := <int>
<F> := <funcion-binaria>
<acum> := <int>
<filter> := <funcion-unitaria>
<i> := <int>
<accumulator> := <int>

|#

(define filter-acum
  (lambda (a b F acum filter)
    (define iterator
      (lambda (i accumulator)
        (cond
          ((> i b) accumulator)
          ((filter i)
           (iterator (+ i 1) (F accumulator i)))
          (else
           (iterator (+ i 1) accumulator)))))
    (iterator a acum)))

; Ejemplos de uso
(filter-acum 1 10 + 0 odd?)  ; Debería imprimir 25
(filter-acum 1 10 + 0 even?) ; Debería imprimir 30
(filter-acum -5 5 + 0 positive?) ; Debería imprimir 15
(filter-acum 1 10 * 1 odd?) ; Debería imprimir 945
(filter-acum 1 10 * 1 even?) ; Debería imprimir 3480


#|13

(operate lrators lrands):
Proposito:
(P,L) -> R : Procedimiento que recibe dos entradas:
Elabore una función llamada (operate lrators lrands) donde
P es una lista de funciones binarias de tamaño n y L es una lista
de números de tamaño n + 1. La función retorna el resultado de aplicar
sucesivamente las operaciones en P a los valores en L. Retornando un
resultado R.

operations:
(p, l) -> r : Función auxiliar que recibe dos entradas:
p, una lista de operadores binarios, y l, una lista de operandos.
Retorna el resultado (r) de aplicar sucesivamente las operaciones en p a los
valores en l.

<lista-operate> := ()
                := (<valor-de-scheme> <lista-operate>)
|#


(define (operate lrators lrands)
  (letrec ((operations
            (lambda (operators operands)
              (cond
                ((null? operators) (car operands))
                (else (operations (cdr operators)
                                       (cons ((car operators) (car operands) (cadr operands))
                                             (cddr operands))))))))
    (operations lrators lrands)))


;Pruebas
(operate (list + * - *) '(8 7 11 3 20)) ;Retorna la resultado 3240
(operate (list * * + + -) '(2 9 17 4 13 30)) ;Retorna la resultado 293
(operate (list + - - * -) '(3 4 15 6 12 25)) ;Retorna la resultado -193 
(operate (list + * - *) '(3 2 7 5 0)) ;Retorna la resultado 0
(operate (list + + - - * *) '(2 7 9 5 32 27 47)) ;Retorna la resultado -24111


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
          ((null? arbol) '#f)
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
(path 0 mybst0) ; Resultado esperado: #f
(path 1 mybst0) ; Resultado esperado: #f
(path 0 mybst1) ; Resultado esperado: #f
(path 1 mybst1) ; Resultado esperado: #f
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


#|15.
count-odd-and-even
Proposito:
(arbol) --> L: Procedimiento que toma un arbol binario como entrada y retorna
una lista con el número de nodos pares e impares respectivamente del arbol,
esta realiza un llamado a la función auxiliar que va recorriendo cada nodo del arbol.

--Funciones auxiliares
count-odd-and-even-par
Proposito:
(subarbol) --> L: Porcedimiento que recibe un subarbol de arbol original, esta es la que
analiza cada nodo del arbol, descomponiendo el arbol en cada componente (valor y subarboles
izquierdo y derecho), para llamar recursivamente a si misma en cada subarbol, al hacer esto lleva
el conteo de nodos tanto pares como impares, al finalizar de recorrer el arbol se hace el conteo
y se retorna el número de nodos pares e iompares en una lista.

<arbol-binario> := (arbol-vacıo) empty
                := (nodo) <int> <arbol-binario> <arbol-binario>

<lista-par-de-numeros> := ()    (cuando no encuentra nodos)
<lista-par-de-numeros> := (<int> <int>)    (cuando si encuentra nodos)

|#

(define count-odd-and-even
  (lambda (arbol)
    (define count-odd-and-even-aux
      (lambda (subarbol)
        (cond
          ((null? subarbol) '(0 0))
          ((pair? subarbol)
           (let* ((nodo (car subarbol))
                  (subarbol-izq (cadr subarbol))
                  (subarbol-der (caddr subarbol))
                  (conteo-izq (count-odd-and-even-aux subarbol-izq))
                  (conteo-der (count-odd-and-even-aux subarbol-der))
                  (conteo-izq-par (car conteo-izq))
                  (conteo-izq-impar (cadr conteo-izq))
                  (conteo-der-par (car conteo-der))
                  (conteo-der-impar (cadr conteo-der)))
             (if (even? nodo)
                 (list (+ 1 conteo-izq-par conteo-der-par) (+ conteo-izq-impar conteo-der-impar))
                 (list (+ conteo-izq-par conteo-der-par) (+ 1 conteo-izq-impar conteo-der-impar)))))
          ((number? subarbol)
           (if (even? subarbol)
               '(1 0)
               '(0 1)))
          (else '(0 0)))))
    (count-odd-and-even-aux arbol)))


(define arbol-vacio '()) ; retorna (0 0)
(define arbol-un-nodo '(5 () ())) ; retorna (0 1)
(define arbol-mediano '(10 (5 () ()) (15 () (20 () ())))) ; retorna (2 2)
(define arbol-grande '(7 (3 () (9 () ())) (15 () (12 () (20 () ())))))  ; retorna (2 4)
(define arbol-mas-grande '(50 (30 (20 () ()) (40 () ())) (70 () (90 () (100 () ()))))) ; retorna (7 0)
(define arbol-ejemplo '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ())))) ; retorna (4 3)

(count-odd-and-even arbol-vacio)
(count-odd-and-even arbol-un-nodo)
(count-odd-and-even arbol-mediano)
(count-odd-and-even arbol-grande)
(count-odd-and-even arbol-mas-grande)
(count-odd-and-even arbol-ejemplo)


#|16

(Operar-binarias operacionB) 
Proposito:
L -> R : Procedimiento que recibe una entrada:
Función que recibe como parámetro L, una operación binaria válida
y retorna el resultado R de hacer las operaciones suma, resta y
multiplicación correspondientes.

<OperacionB>::= <int>
            ::= (<OperacionB> ’suma <OperacionB>)
            ::= (<OperacionB> ’resta <OperacionB>)
            ::= (<OperacionB> ’multiplica <OperacionB>)
|#


(define (Operar-binarias operacionB)
  (cond
    ((number? operacionB) operacionB) 
    ((list? operacionB)
     (let ((op (cadr operacionB))
           (arg1 (Operar-binarias (car operacionB)))
           (arg2 (Operar-binarias (caddr operacionB))))
       (cond
         ((eq? op 'suma) (+ arg1 arg2))
         ((eq? op 'resta) (- arg1 arg2))
         ((eq? op 'multiplica) (* arg1 arg2)))))))
         
;;Pruebas
(Operar-binarias 7) ;Retorna la resultado 7
(Operar-binarias '(12 suma 3)) ;Retorna la resultado 15
(Operar-binarias '(1 resta 4)) ;Retorna la resultado -3
(Operar-binarias '(5 multiplica 5)) ;Retorna la resultado 25
(Operar-binarias '(((9 multiplica 5) suma (10 resta 8 )) resta 6 )) ;Retorna la resultado 41
(Operar-binarias '((8 multiplica (22 suma 13)) resta((6 multiplica 6) multiplica 7))) ;Retorna la resultado 28


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


#|18.
pascal
Proposito:
(N) --> L: Procedimiento que recibe un entero positivo N y retorna la fila N correspondiente
al triangulo de Pascal, inicialmente se analiiza N, cuando se entrega un entero menor a uno
se retorna una lista vacia, cuando no, se invoca a la funcion auxiliar 'construct-row'.

--Funciones auxiliares
construct-row
Proposito:
(row) --> L: Este procedimiento contruye la fila N del triangulo de pascal, haciendo un llamado
a otra función auxiliar nombrada 'construct-row-aux'

construct-row-aux
Proposito:
(col) --> L: Este procedimiento contruye cada elemento correspondiente a la fila N del triangulo
de Pascal, lo hace columna por columna, inciando desde la columna 0, luego almacena estos
números en la lista L.

calculate-element
Proposito:
(row col) --> Int: Procedimiento que calcula el valor de un elemento especifico en el triangulo
de Pascal, para esto verifica si el elemento esta en los extremos del triangulo (donde siempre
el valor es 1), o en la parte interna, para este caso suma los valores de los elementos en las
filas anteriores del triangulo para calcular el valor de cada elemento especifico.

<lista-numeros> := () (cuando se ingresa un N que no existe en el triangulo de Pascal)
<lista-numeros> := (<int> <lista-numeros>) (cuando se ingresa un N que si existe en el triangulo de Pascal)
|#


(define pascal
  (lambda (N)
    (define calculate-element
      (lambda (row col)
        (cond
          ((or (= col 0) (= col row)) 1)
          (else (+ (calculate-element (- row 1) (- col 1))
                   (calculate-element (- row 1) col))))))
  
    (define construct-row
      (lambda (row)
        (define construct-row-aux
          (lambda (col)
            (if (= col row)
                (list (calculate-element row col))
                (cons (calculate-element row col) (construct-row-aux (+ col 1))))))
        (construct-row-aux 0)))
  
    (if (< N 1)
        '()
        (construct-row (- N 1)))))

; Pruebas
(pascal -1) ; retorna ()
(pascal 0) ; retorna ()
(pascal 1) ; retorna (1)
(pascal 2) ; retorna (1 1)
(pascal 3) ; retorna (1 2 1)
(pascal 4) ; retorna (1 3 3 1)
(pascal 5) ; retorna (1 4 6 4 1)
(pascal 6) ; retorna (1 5 10 10 5 1)
(pascal 7) ; retorna (1 6 15 20 15 6 1)
(pascal 8) ; retorna (1 7 21 35 35 21 7 1)
(pascal 9) ; retorna (1 8 28 56 70 56 28 8 1)
(pascal 10) ; retorna (1 9 36 84 126 126 84 36 9 1)

