# Todas las tareas de Prolog por Ceja Garcia Dante Dali
### Tareas en el documento:

1. Sumar ingresando numeros
1. Areas
1. Descabechamiento
1. 5 Ejercicios
1. Areas con lambda

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

## 5 Ejercicios
5 Ejecicios donde hacemos uso de When Unless, cond, uless y case
Los ejercicios son: 
1. Descuentos en tienda de pantalones Dickies
1. Determinar si se puede solicitar un prestamo
1. Imprimir sin una letra es vocal, semivocal o consonante
1. Determinar si un año es bisiesto o no
1. Imprimir numero de dias de un mes

```
;1. Descuentos en tienda de pantalones Dickies
(defun pantalon()
(format t "Dame la cantidad de pantalones Dickies a comprar $1,000 cu. ~%")
(setq x(read))
(setq total (* x 1000))
(format t "Cantidad total sin descuento, ~a" total)
(format t "~%")
  (cond
    ((< x 5)
    (format t "No hay descuento. ~%")
    )
    ((and (>= x 5) (< x 12))
    (format t "Tiene un 15% de descuento . ~%")
    (setq desc (* total 0.15))
    (setq total2 (- total desc))
    (format t "descuento a aplicar, ~a" desc)
    (format t "~%")
    (format t "total a pagar, ~a" total2)
    )
    ((>= x 12)
    (format t "Tiene un 30% de descuento. ~%")
    (setq desc (* total 0.30))
    (setq total2 (- total desc))
    (format t "descuento a aplicar, ~a" desc)
    (format t "~%")
    (format t "total a pagar, ~a" total2)
    )
  )
)

;2. Determinar si se puede solicitar un prestamo
(defun prestamo ()
(setq pnts 0)
(format t "Dame tu nombre. ~%")
(setq nom(read))
(format t "Dame tu historia crediticia (‘b’buena o ‘m’ mala). ~%")
(setq histcred(read))
(format t "Dame la cantidad a pedir. ~%")
(setq cantidad(read))
(format t "Dame tu salario anual. ~%")
(setq salanu(read))
(format t "Dame el valor de otras propiedades. ~%")
(setq valprop(read))

(case histcred
(b 
  ;salario anual
(if (>= salanu (* cantidad 0.5))
    (progn
      (setq pnts (+ pnts 5))
      (format t "Tu salario es más de la mitad, 5 puntos. ~%")))

(if (and (< salanu (* cantidad 0.5)) (>= salanu (* cantidad 0.25)))
    (progn
      (setq pnts (+ pnts 3))
      (format t "Tu salario es más del 25% pero menos de la mitad, 3 puntos. ~%")))

(if (and (< salanu (* cantidad 0.25)) (>= salanu (* cantidad 0.10)))
    (progn
      (setq pnts (+ pnts 1))
      (format t "Tu salario es más del 10% pero menos del 25%, 1 punto. ~%")))

  ;valor de propiedad
(if (>= valprop (* cantidad 2))
    (progn
      (setq pnts (+ pnts 5))
      (format t "Tus propiedades son más del doble, 5 puntos. ~%")))

(if (and (> valprop cantidad) (< valprop (* cantidad 2)))
    (progn
      (setq pnts (+ pnts 3))
      (format t "Tus propiedades son más del préstamo pero menos del doble, 3 puntos. ~%")))

  ;comprobamos puntos
(if (>= pnts 6)
  (format t "Tienes mas de 5 puntos, eres acredor al prestamo. ~%")
  (format t "Tienes menos de 5 puntos, no podemos darte prestamo. ~%")
)


)
(m (print "Prestamo invalido por mal historial crediticio"))
)
)

;3. Imprimir sin una letra es vocal, semivocal o consonante
(defun letra ()
(setq pnts 0)
(format t "Dame una letra minuscula. ~%")
(setq letra (read-char))
(cond
  ((or (char= letra #\a) (char= letra #\e) (char= letra #\i) (char= letra #\o) (char= letra #\u))
   (format t "Tu letra, ~a, es una vocal.~%" letra))

  ((char= letra #\y)
   (format t "Tu letra, ~a, es una semivocal.~%" letra))

  ((or (char= letra #\1) (char= letra #\2) (char= letra #\3) (char= letra #\4) (char= letra #\5) (char= letra #\6) (char= letra #\7) (char= letra #\8) (char= letra #\9) (char= letra #\0))
   (format t "Ingresaste, ~a, que es un numero.~%" letra))

  (t
   (format t "Tu letra, ~a, es una consonante.~%" letra))
   )
)

;4. Determinar si un año es bisiesto o no
(defun anio ()
  (format t "Dame un año y te diré si es bisiesto. ~%")
  (setq anio (read))
  (cond
    ((and (= (mod anio 4) 0) (not (= (mod anio 100) 0)))
     (format t "~a es bisiesto.~%" anio))

    ((= (mod anio 400) 0)
     (format t "~a es bisiesto.~%" anio))

    (t
     (format t "~a no es bisiesto.~%" anio)))
  )

;5. Imprimir numero de dias de un mes
(defun mes ()
(format t "Dame un mes, en numero. ~%")
(setq mes (read))
(cond
  ((= mes 2)
   (format t "Tu mes, ~a, tiene 28 dias.~%" mes))

  ((or (= mes 4) (= mes 6) (= mes 9) (= mes 11))
   (format t "Tu mes, ~a, tiene 30 dias.~%" mes))

  ((or (= mes 1) (= mes 3) (= mes 5) (= mes 7) (= mes 8) (= mes 10) (= mes 12))
   (format t "Tu mes, ~a, tiene 31 dias.~%" mes))

  (t
   (format t "Introduciste algo que no es un mes.~%"))
   )
)
```

## Areas con lambda
El mismo ejercicio de las areas presentado anteriormente pero ahora usando solo expresiones lambda

```
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;lambdas
(defvar *cuadrado* (lambda (x) (* x x)))
(defvar *cuboi* (lambda (x) (* * x x x)))
(defvar *multiplicacion* (lambda (x y) (* x y)))
(defvar *divicion* (lambda (x y) (/ x y)))
(defvar *divicion360* (lambda (x) (/ x 360)))
(defvar *divicion2* (lambda (x) (/ x 2)))
(defvar *sumacion* (lambda (x y) (+ x y)))
(defvar *restacion* (lambda (x y) (- x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;areas
(defun cuadro()
(princ "dame un numero ")
(setq x(read))
(funcall *cuadrado* x))

(defun rectangulo()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(funcall *multiplicacion* x y))

(defun triangulo()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(funcall *divicion2* (funcall *multiplicacion* x y)))

(defun circulo()
(princ "dame un numero ")
(setq x(read))
(funcall *multiplicacion* (funcall *cuadrado* x) 3.1416))

(defun trapecio()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(princ "dame un numero ")
(setq z(read))
(funcall *divicion2* (funcall *multiplicacion* (funcall *sumacion* x y) z )))

(defun paralelogramo()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(funcall *multiplicacion* x y))

(defun rombo()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(funcall *divicion2* (funcall *multiplicacion* x y)))

(defun pentagonoregular()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(funcall *divicion2* (funcall *multiplicacion* x y)))

(defun hexagonoregular()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(funcall *divicion2* (funcall *multiplicacion* x y)))

(defun circulosectorial()
(princ "dame un numero ")
(setq x(read))
(funcall *cuadrado* (funcall *multiplicacion* (funcall *divicion360* x) 3.1416 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;volumenes
(defun cubo(x)
(princ "dame un numero ")
(setq x(read))
(funcall *cuboi* x))

(defun paralelepipedo()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(princ "dame un numero ")
(setq z(read))
(funcall *multiplicacion* (funcall *multiplicacion* x y) z))


(defun prismarectangular(x y z)
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(princ "dame un numero ")
(setq z(read))
(funcall *multiplicacion* (funcall *multiplicacion* x y) z))

(defun esfera(x)
(princ "dame un numero ")
(setq x(read))
(funcall *multiplicacion* (funcall *multiplicacion* (funcall *multiplicacion* (funcall *divicion* 4 3) 3.1416) x) 3))

(defun cilindro(x y)
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(funcall *multiplicacion* (funcall *multiplicacion* (funcall *cuadrado* x) 3.1416) y ))

(defun cono()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(princ "dame un numero ")
(setq z(read))
(funcall *multiplicacion* (funcall *multiplicacion* (funcall *multiplicacion* (funcall *divicion* 1 3) 3.1415) (funcall *cuadrado* x)) y))

(defun piramidecuadrada()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(funcall *multiplicacion* (funcall *multiplicacion* (funcall *divicion* 1 3) (funcall *cuadrado* x )) y))

(defun tetraedro()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(funcall *multiplicacion* (funcall *multiplicacion* (funcall *divicion* 1 3) x) y))

(defun cilindrohueco()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(princ "dame un numero ")
(setq x(read))
(funcall *multiplicacion* (funcall *multiplicacion* (funcall *restacion* (funcall *cuadrado* x) (funcall *cuadrado* y)) z) 3.1415))

(defun cilindrooblicuo()
(princ "dame un numero ")
(setq x(read))
(princ "dame un numero ")
(setq y(read))
(funcall *multiplicacion* x y))

```