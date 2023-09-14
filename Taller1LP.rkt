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
(list-set '() 1 3)
(list-set '(a b c d) 1 'a)
(list-set '(a 2 4 g) 1 '(1 2))
(list-set '(a b c d) 3 '(1 5 10))

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

#| 9.
inversions
Proposito:
(L) --> Int: Procedimiento que recibe una lista de números L=(a1, a2, ..., an),
esta realiza el conteo de las veces que un número ai > aj, siempre que i < j
(es decir que haya un número mayor a otro que esta ubicado antes del segundo
número en la lista)

--Funciones auxiliares:
count-inversions
Propósito:
(L) --> Int: Procedimiento que calcula el número de inversiones en la lista L.
En esta se realiza el llamado a la función count, que toma la lista L y un acumulador.

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

<lista-numeros> := ()
<lista-numeros> := (<int> <lista-numeros>)
|#

(define (inversions L)
  (define (count-inversions L)
    (define (count L accumulator)
      (cond
        ((null? L) accumulator)
        (else
         (let ((current (car L)))
           (count (cdr L)
                  (+ accumulator (count-greater current (cdr L))))))))
    (count L 0))

  (define (count-greater x L)
    (cond
      ((null? L) 0)
      ((> x (car L)) (+ 1 (count-greater x (cdr L))))
      (else (count-greater x (cdr L)))))

  (count-inversions L))

(inversions '(2 3 8 6 1)) 
(inversions '(1 2 3 4))  
(inversions '(3 2 1))     

#| 12.
filter-acum
(a b F acum filter) --> Int: Procedimiento que recibe dos números a y b, una función
binaria F, un valor inicial acum y una función unitaria filter, esta aplica la función
F a los enteros que estan en el intervalo [a, b] y que a su vez cumplen con la función
filter, luego esto lo retorna acumulandolo en acum, que actua como un valor inicial.

--Funciones auxiliares
iterator
(i, accumulator) --> : Procedimiento auxiliar que realiza la iteración y el calculo
acumulativo en el rango de 'a' a 'b', aplicando 'filter' a cada elemento del rango
y a los que pasen filtro aplicarles 'F' ademas de acumular este resultado y retornarlo
al final.

|#

(define (filter-acum a b F acum filter)
  (define (iterator i accumulator)
    (cond
      ((> i b) accumulator)
      ((filter i)
       (iterator (+ i 1) (F accumulator i)))
      (else
       (iterator (+ i 1) accumulator))))
  (iterator a acum))


; Ejemplos de uso
(filter-acum 1 10 + 0 odd?)  ; Debería imprimir 25
(filter-acum 1 10 + 0 even?) ; Debería imprimir 30

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

|#
(define (count-odd-and-even arbol)
  (define (count-odd-and-even-aux subarbol)
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
      (else '(0 0))))
  (count-odd-and-even-aux arbol))

(define arbol-ejemplo '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ()))))


(count-odd-and-even arbol-ejemplo)

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

<lista-numeros> := ()
<lista-numeros> := (<int> <lista-numeros>)
|#


(define (pascal N)
  (define (calculate-element row col)
    (cond
      ((or (= col 0) (= col row)) 1)
      (else (+ (calculate-element (- row 1) (- col 1))
               (calculate-element (- row 1) col)))))
  
  (define (construct-row row)
    (define (construct-row-aux col)
      (if (= col row)
          (list (calculate-element row col))
          (cons (calculate-element row col) (construct-row-aux (+ col 1)))))
    (construct-row-aux 0))
  
  (if (< N 1)
      '()
      (construct-row (- N 1))))





