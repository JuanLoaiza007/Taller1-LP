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

> (path 17 ’(14 (7 () (12 () ()))
  (26 (20 (17 () ())
          ())
      (31 () ()))))
(right left left)

|#

; Casos prueba del enunciado
; Ni idea de como formular esto aún (en Desarrollo...)

#| Teoria (en Desarrollo...)
|#

(define path
  (lambda (n BST)
    'a_ver_al_cine
  )
)