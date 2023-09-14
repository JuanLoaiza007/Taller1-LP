#lang eopl

; Espacio de juegos con Racket que NO HACE PARTE DE LA ENTREGA FINAL

#| PUNTO 14
14) Elabore una función llamada path que recibe como entrada dos
parámetros: un número n y un árbol binario de búsqueda (representando
con listas) BST (el árbol debe contener el número entero n). La función
debe retornar una lista con la ruta a tomar (iniciando desde el nodo raíz del
árbol), indicada por cadenas left y right, hasta llegar al número n recibido.
Si el número n es encontrado en el nodo raíz, el procedimiento debe retornar
una lista vacía.

> (path 17 '(14 (7 () (12 () ()))
  (26 (20 (17 () ())
          ())
      (31 () ()))))
(right left left)

         14
       /    \
      7      26
       \    /  \
       12  20   31
           /\
         17
|#

; Casos prueba del enunciado
; Ni idea de como formular esto aún (en Desarrollo...)

#| Teoria (en Desarrollo...)
; T1: Generamos todos los caminos posibles y su resultado con un #f y #t, luego rescatamos el camino correcto #t
; T2: (Correccion) La teoria de un arbol binario de busqueda decia era que el nodo y el valor buscado se comparaban. Entonces:
       a) Si el valor que estoy buscando es menor al nodo me voy pa la rama izquierda, si no, pa la derecha xD
          esto es porque

          "el subárbol izquierdo de cualquier nodo (si no está vacío)
          contiene valores menores que el que contiene dicho nodo, y el subárbol derecho
          (si no está vacío) contiene valores mayores"

          Fuente: https://es.wikipedia.org/wiki/%C3%81rbol_binario_de_b%C3%BAsqueda
|#

(define unArbol '(14
                  (7 () (12 () ()) )
                  (26  (20 (17 () ()) ())  (31 () ()) )
                 )
)

#|
(define valor-nodo
  (lambda (tree)
    (if (null? (car tree))
        '()
        (car tree)
        )
    )
  )
    
(define valor-izquierdo
  (lambda (tree)
    (if (null? (cadr tree))
        '()
        (car (cadr tree)))
    )
  )

(define valor-derecho
  (lambda (tree)
    (if (null? (caddr tree))
        '()
        (car (caddr tree)))
    )
  )


(define rama-izquierda
  (lambda (tree)
    (car (cdr tree))
    )
  )

(define rama-derecha
  (lambda (tree)
    (car (cdr (cdr tree)))
    )
  )
|#


#|
; AVISO DE INCOMPLETA
; T1: Obtiene TODOS los caminos y su resultado con mucho ruido visual
; Falta rescatar el camino correcto
(define path
  (lambda (n BST)
    
    (define esNodo?
      (lambda (objeto)
        (if (or (boolean? objeto)
                (eqv? 'left objeto)
                (eqv? 'right objeto)
                )
            #t
            #f
            )
        )
      )
    
    (cond
          ((null? BST) (list #f))
          ((= n (valor-nodo BST)) (list #t))
          (else
           (list
            (cons 'left (path n (rama-izquierda BST)))
            (cons 'right (path n (rama-derecha BST)))))))

    ) |#

; T2: Piola 👍
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
    
    (define path-aux
      (lambda (numero arbol camino)
        
       (cond
         ((null? arbol) 'nosta)
         ((= numero (valor-nodo arbol)) camino) 
         ((< numero (valor-nodo arbol))
          (path-aux numero (cadr arbol) (list camino 'left)))
         ((> numero (car arbol))
          (path-aux numero (caddr arbol) (list camino 'right)))
         )
        
       )
      )
    
    (reparar (path-aux n BST '()))
    )
  )

(define reparar
  (lambda (lcaotica)

    (define rescatar-ultimo
      (lambda (lista)
        (cond
          ((null? lista) '())
          ((null? (cdr lista)) (car lista))
          (else (rescatar-ultimo (cdr lista)))
          )
        )
      )

    (define valido? (or 'left 'right))
        
    (if (null? (car lcaotica))
        (cons (cadr lcaotica) empty)
        (cons (rescatar-ultimo lcaotica) (reparar (car lcaotica))))
    )
  )

(define lista (path 17 unArbol))
(define lista1 '(right (left #f) (right #f)))
(define lista0 '(a (b (#f) (#f)) (c (#f) (#f))))


