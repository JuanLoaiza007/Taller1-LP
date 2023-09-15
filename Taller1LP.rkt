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
(list-set '() 1 3) ; retorna ()
(list-set '(5 8 9 0 1) 0 3) ; retorna (3 8 9 0 1)
(list-set '(a b c d) 1 'a) ; retorna (a a c d)
(list-set '(a 2 4 g) 1 '(1 2)) ; retorna (a (1 2) 4 g)
(list-set '(a b c d) 3 '(1 5 10)) ; retorna (a b c (1 5 10))

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




