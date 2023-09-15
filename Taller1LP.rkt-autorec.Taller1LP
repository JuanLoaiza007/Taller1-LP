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

<lista> := ()
<lista> := (<valor-de-scheme> <lista>)
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

#| 12. filter-acum


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


|#


(define (pascal N)
  (define (calculate-element row col)
    (cond
      ((or (= col 0) (= col row)) 1)
      (else (+ (calculate-element (- row 1) (- col 1))
               (calculate-element (- row 1) col)))))
  
  (define (construct-row row)
    (define (construct-row-helper col)
      (if (= col row)
          (list (calculate-element row col))
          (cons (calculate-element row col) (construct-row-helper (+ col 1)))))
    (construct-row-helper 0))
  
  (if (< N 0)
      '()
      (construct-row (- N 1))))





