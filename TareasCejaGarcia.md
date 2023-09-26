# Todas las tareas de Prolog por Ceja Garcia Dante Dali
### Tareas en el documento:

1. Sumar ingresando numeros
1. Areas
1. Descabechamiento

# Tareas ->

## Sumar dos numeros
Primer tarea donde aprendimos a pedir numeros por teclado para posteriormente sumarlos
```
(defun sumarnumero()
(princ "dame un numero ")
(setq num1(read))
(princ "dame un segundo numero ")
(setq num2(read))
(setq res (+ num1 num2))
;(write res)
)
```


## Areas
Tarea donde sacamos 10 areas y 10 volumenos piediendo todo lo necesario por teclado
```
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;areas
(defun cuadro()
(princ "dame un numero ")
(setq x(read))
(setq res (* x x)))

(defun rectangulo()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(setq res (* x y)))

(defun triangulo()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(setq res ( / (* x y) 2 )))

(defun circulo(x)
(princ "dame un numero ")
(setq x(read))
(setq res ( * (expt x 2) 3.1416)))

(defun trapecio()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(princ "dame un numero ")
(setq z(read))
(setq res ( / ( * (+ x y) z ) 2 )))

(defun paralelogramo()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(setq res (* x y)))

(defun rombo()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(setq res ( / (* x y) 2 )))

(defun pentagonoregular()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(setq res ( / (* x y) 2 )))

(defun hexagonoregular()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(setq res ( / (* x y) 2 )))

(defun circulosectorial()
(princ "dame un numero ")
(setq x(read))
(setq res (expt (* (/ x 360) 3.1416) 2 )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;volumenes
(defun cubo(x)
(princ "dame un numero ")
(setq x(read))
(setq res (expt x 3)))

(defun paralelepipedo()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(princ "dame un numero ")
(setq z(read))
(setq res (* * x y z)))

(defun prismarectangular(x y z)
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(princ "dame un numero ")
(setq z(read))
(setq res (* * x y z)))

(defun esfera(x)
(princ "dame un numero ")
(setq x(read))
(setq res (* (* (/ 4 3)3.1416 ) x 3)))

(defun cilindro(x y)
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(setq res (* (* 3.1416 (expt x 2) y))))

(defun cono()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(princ "dame un numero ")
(setq z(read))
(setq res (* (* ( * (/ 1 3) 3.1415) (expt x 2)) y )))

(defun piramidecuadrada()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(setq res (* * (/ 1 3) (expt x ) y )))

(defun tetraedro()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(setq res (* * (/ 1 3) x y )))

(defun cilindrohueco()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(princ "dame un numero ")
(setq x(read))
(setq res (* * 3.1415 (- (expt x 2) (expt y 2) ) z )))

(defun cilindrooblicuo()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(setq res (* x y)))
```

## Descabechamiento
Tarea donde recorremos e imprimimos una lista mientras no sea nula asi como darle formato y otra funcion donde verificamos que un numero ingresado exista en la lista
```
    (defun recorre (lista)
    (princ(car lista))
    (princ " ")
    (if (not (null lista))
        (recorre (cdr lista))
    )
    )

    (defun recorre2 (lista)
    (format t "~A-> ~%" (car lista)
    (if lista 
        (recorre2 (cdr lista))
    )
    )    
    )

    (defun recorre3 (lista)
    (princ(car lista))
    (princ " ")
    (if (not (null lista)) 
        (recorre (cdr lista))
    )
    (format t "~%")
    (princ "dame un numero a buscar ")
    (setq x(read))
    (if ( member x lista) (princ "Si esta el numero")
    (princ "no existe el numero"))
    )
```
