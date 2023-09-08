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



