#lang eopl

; Espacio de juegos con Racket que NO HACE PARTE DE LA ENTREGA FINAL

#| PUNTO 8
8) Elabore una función llamada mapping que debe recibir como
entrada 3 argumentos: una función unaria (que recibe un argumento) llamada
F, y dos listas de n´umeros L1 y L2. La función debe retornar una lista de
pares (a,b) siendo a elemento de L1 y b elemento de L2, cumpliendose la
propiedad que al aplicar la función unaria F con el argumento a, debe arrojar
el número b. Es decir, se debe cumplir que F(a) = b.

LAS LISTAS DEBEN SER DE IGUAL TAMAÑO

> (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
((1 2) (2 4) (3 6))
> (mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
((2 6))
> (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))
()

|#

; Casos prueba del enunciado
(define funar1 (lambda (d) (* d 2)))
(define funar2 (lambda (d) (* d 3)))
(define funar3 (lambda (d) (* d 2)))

(define l1 (list 1 2 3))
(define l11 (list 2 4 6))

(define l2 (list 1 2 2))
(define l22 (list 2 4 6))

(define l3 (list 1 2 3))
(define l33 (list 3 9 12))
; uso: (mapping funar1 l1 l11)

#| Teoria
DUDA: ¿Hay que agregar una restricción por si las listas no
son del mismo tamaño?

> (= (* (car l1) 2) (car l11))
#t
...
|#

#| 
;; Version v1 - Tiene el problema de que genera listas vacias
(define mapping
  (lambda (F L1 L2)
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
|#

; Version 2 - Esta re gorda porque elimina las listas vacias en la lista principal
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