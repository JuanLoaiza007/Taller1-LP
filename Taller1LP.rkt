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


;;#|1.
;; invert:
;; Proposito:
;; L -> L' : Procedimiento que invierte los pares
;; de una lista L.
;;
;;<lista> := ()
;; := (<valor-de-scheme> <lista>)



(define invert
  ( lambda (l)
     (if (null? l)
         '()
         (cons (list (cadr (car l)) (car (car l))) (invert (cdr l))))
      )

   )


;;#|4.
;; filter-in:
;; Proposito:
;; P x L -> P' x L' : Procedimiento que invierte los pares
;; de una lista L.
;;
;;<lista> := ()
;; := (<valor-de-scheme> <lista>)



(define filter-in
  (lambda (P L)
    (cond
      [(null? L) '()]
      [(P (car L)) (cons (car L) (filter-in P (cdr L)))]
      (else (filter-in P (cdr L))))))



;;|6
;; cartesian-product:
;; Proposito:
;; L1 x L2 -> L1' x L2' : Procedimiento que hace producto cartesiano
;; entre los datos de dos listas.
;;
;;<lista> := ()
;; := (<valor-de-scheme> <lista>)

;; (cartesian-product '(a b c) '(x y))

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
