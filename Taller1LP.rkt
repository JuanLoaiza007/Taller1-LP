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