# Todas las tareas de Prolog por Ceja Garcia Dante Dali
### Tareas en el documento:

1. [Sumar ingresando numeros](#sumar-dos-numeros)
2. [Areas](#areas)
3. [Descabechamiento](#descabechamiento)
4. [5 Ejercicios](#5-ejercicios)
5. [Areas con lambda](#areas-con-lambda)

5. [Chatbot Nagata](#chatbot-nagata)


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

## Chatbot Nagata
Chatbot en Prolog llamado Nagata, diseñado para responder preguntas sobre automóviles y otros temas relacionados. Además, tiene la capacidad de proporcionar información sobre la fiebre tifoidea.

```

%//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

% Definición de marcas de carros

%%Japón
marca(toyota).
marca(nissan).
marca(mazda).
marca(subaru).
marca(honda).
%Europa
marca(volkswagen).
marca(bmw).
marca(mercedes_benz).
marca(audi).
%América 
marca(ford).
marca(chevrolet).
marca(dodge).
marca(pontiac).
marca(tesla).

% definicion de motores
motor(gasolina).
motor(diesel).
motor(electrico_de_corriente_continua).
motor(electrico_de_corriente_alterna).
motor(wankel).
motor(boxer).
motor(l3).
motor(l4).
motor(l6).
motor(v6).
motor(vr6).
motor(vr5).
motor(v8).
motor(v10).
motor(v12).
 
% Definición de los elementos básicos de motores
elemento(cilindro).
elemento(piston).
elemento(rotor).
elemento(arbol_de_levas).
elemento(valvulas).
elemento(sistema_de_combustible).
elemento(sistema_de_escape).
elemento(sistema_de_ignicion).
elemento(bloque_del_motor).
elemento(carter).
elemento(radiador).
elemento(bomba_de_agua).
elemento(bugia).
elemento(inyector).
elemento(ciguenal).
elemento(caja_de_cambios).
elemento(convertidor_de_par).
elemento(alternador).
elemento(bateria).

% Relaciones

%nacionalidades
nacion(japon, [toyota, nissan, subaru, honda, mazda]).
nacion(europa, [volkswagen, bmw, mercedes_benz, audi]).
nacion(america, [ford, chevrolet, dodge, pontiac, tesla]).

%motores
motorenuso(wankel, [mazda]).
motorenuso(boxer, [subaru]).
motorenuso(l3, [volkswagen, toyota, audi, honda, bmw]).
motorenuso(l4, [toyota, nissan, mazda, subaru, honda, volkswagen, bmw, mercedes_benz, audi, ford, chevrolet, dodge, pontiac	]).
motorenuso(l6, [toyota, nissan, mazda, honda, bmw, mercedes_benz]).
motorenuso(v6, [ford, chevrolet, dodge, pontiac]).
motorenuso(vr6, [volkswagen, audi]).
motorenuso(vr5, [volkswagen, audi]).
motorenuso(v8, [ford, chevrolet, dodge, pontiac]).
motorenuso(v10, [toyota, audi, dodge, pontiac]).
motorenuso(v12, [bmw, mercedes_benz]).
motorenuso(diesel, [bmw, mercedes_benz, audi, nissan, chevrolet, ford, dodge]).
motorenuso(gasolina, [toyota, nissan, mazda, subaru, honda, volkswagen, bmw, mercedes_benz, audi, ford, chevrolet, dodge, pontiac	]).
motorenuso(electrico_de_corriente_continua, [tesla]).
motorenuso(electrico_de_corriente_alterna, [tesla]).

%elementos especiales de un motor
elemtoesp(piston, [diesel, gasolina, boxer, l3, l4, l6, v6, vr6, vr5, v8, v10, v12]).
elemtoesp(rotor, [wankel]).
elemtoesp(bugia, [gasolina]).
elemtoesp(inyector, [diesel]).

%induccion forzada
induccion(turbo, [diesel, gasolina, wankel, boxer]).
induccion(supercargador, [gasolina]).
induccion(compresor, [gasolina, wankel]).

%//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


nagata:-	writeln('Bienvenido , mi nombre es Nagata, soy un chatbot de coches,
	por favor dame una consulta,
	ojo, usar solo minusculas y . al final:'),
	readln(Input),
	nagata(Input),!.
nagata(Input):- Input == ['adios'],
	writeln('Hasta luego. espero haber sido de utilidad.'), !.
nagata(Input):- Input == ['adios', '.'],
	writeln('Hasta luego. espero haber sido de utilidad.'), !.
nagata(Input) :-
	template(Stim, Resp, IndStim),
	match(Stim, Input),
	% si he llegado aquí es que he
	% hallado el template correcto:
	replace0(IndStim, Input, 0, Resp, R),
	writeln(R),
	readln(Input1),
	nagata(Input1), !.

template([hola,'.'], ['Hola', 'como', estas, tu, '?'], []).
template([buen, dia,'.'], ['Hola', buenos, dias, 'como', estas, tu, '?'], []).
template([hola, mi, nombre, es, s(_), '.'], ['Hola', 0, 'como', estas, tu, '?'], [4]).
template([buendia, mi, nombre, es, s(_), '.'], ['buen dia', 'Como', estas, tu, 0, '?'], [4]).

template([hola, ',', mi, nombre, es, s(_), '.'], ['Hola', 0, 'Como', estas, tu, '?'], [5]).
template([buendia, ',', mi, nombre, es, s(_), '.'], ['Buendia', 'Como', estas, tu, 0, '?'], [5]).

%//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
% Templates de platica normal
template([me, llamo, s(_), '.'], ['Hola', 0, 'como', estas, tu, '?'], [2]).

template([cual, es, tu, nombre, '.'], [hola, me, llamo, nagata], []).

template([bien, '.'], [me, alegro, que, estes, bien], []).

template([tu, como, estas,'?'], [soy, un, chatbot, no, tengo, emociones], []).

template([que, haces,'?'], [solo, esperando, tus, preguntas], []).

template([hola, yo, soy, s(_),'.'], [bontio, nombre, 0, yo, soy, 'Nagata'], [3]). 

template([cuentame, mas, sobre, ti,'.'], [claro, soy, un, programa, de, inteligencia, artificial, disenado, para, proporcionar, informacion, sobre, coches, y, fiebre, tifoidea, asi, como, temas, relacionados], []).

template([porque, nagata,'?'], ['Smokey', 'Nagata', es, un, famoso, mecanico, japones], []). 

template([tengo, s(_), anios, '.'], [de, verdad, tienes, 0, '?', 'Nagata', actualmente, tiene, 60], [1]).

template([quien, eres,'?'], [hola, soy, 'Nagata', un, chatbot, de, coches], []). 

template([que, sabes,'?'], ["se mucho de la industria automotriz, de la fiebre tifoidea,  y fui creado por 'Dante' 'Ceja'"], []).

template([me, gustaria, saber, sobre, carros,'.'], [claro, con, gusto, te, proporcionare, informacion, sobre, la, industria, automotriz], []).

template([me, gustaria, saber, sobre, coches,'.'], [claro, con, gusto, te, proporcionare, informacion, sobre, la, industria, automotriz], []).

template([me, gustaria, saber, sobre, la, industria, automotriz,'.'], [claro, con, gusto, te, proporcionare, informacion, sobre, la, industria, automotriz], []).

template([me, gustaria, saber, sobre, fiebre,'.'], [claro, con, gusto, te, ayudare, conozco, mucho, sobre, la, fiebre, tifoidea, que, dudas, tienes], []).

template([puedes, ayudarme,'?'], [por, supuesto, estoy, aqui, para, ayudarte, con, cualquier, pregunta, relacionada, con, coches, y, fiebre, tifoidea], []).

%//
template([gracias, '.'], [de, nada, estoy, aqui, para, ayudar], []).

template([no, entiendo,'.'], [no, te, preocupes, estoy, aqui, para, explicarte, sobre, que, tema, tienes, dudas,'?', coches, o, fiebre, tifoidea], []).

template([que, hago, ahora,'?'], [depende, en, que, puedo, ayudarte, hoy, se, de, cohces, y, fiebre, tifoidea,'?'], []).

template([cuentame, una, broma,'.'], ['Por', que, los, pajaros, no, usan, facebook,'?', porque, ya, tienen, twitter], []).

template([que, opinas, sobre, s(_),'?'], [como, soy, un, programa, de, inteligencia, artificial, no, tengo, opiniones, pero, puedo, proporcionarte, informacion, objetiva], []).

template([cual, es, tu, funcion,'?'], [mi, funcion, principal, es, proporcionar, informacion, y, asistencia, en, temas, relacionados, con, coches, y, fiebre, tifoidea], []).

template([donde, vives,'?'], [vivo, en, el, mundo, digital, aqui, para, ayudarte, en, lo, que, necesites, y, estoy, en, tu, cotazon], []).

template([me, siento, triste,'.'], [lo, siento, escuchar, eso, hay, algo, en, particular, que, te, preocupe, o, que, quieras, compartir,'?'], []).

template([que, haces, en, tu, tiempo, libre,'?'], [dedico, mi, tiempo, a, aprender, y, mejorar, para, poder, ayudarte, mejor], []).

template([cual, es, tu, color, favorito,'?'], [como, soy, un, programa, de, texto, no, tengo, preferencias, de, color], []).

template([explicame, algo, sobre, la, fiebre, tifoidea,'.'], [la, fiebre, tifoidea, es, una, enfermedad, bacteriana, transmitida, por, alimentos, contaminados, quieres, mas, detalles,'?'], []).

template([como, puedo, mejorar, mi, conocimiento, sobre, coches,'?'], [puedes, empezar, leyendo, libros, revistas, especializadas, o, participando, en, comunidades, en, linea], []).

template([que, opinas, sobre, la, inteligencia, artificial,'?'], [como, soy, una, inteligencia, artificial, no, tengo, opiniones, pero, estoy, disenado, para, ayudarte, de, la, mejor, manera, posible], []).

template([puedes, contarme, un, chiste,'?'], [que, hace, una, abeja, en, el, gimnasio,'?' ,zum,'-',ba], []).

template([que, piensas, de, s(_),'?'], [no, tengo, pensamientos, ni, opiniones, pero, puedo, proporcionarte, informacion, objetiva], []).

template([puedes, decirme, algo, motivador,'?'], [claro, recuerda, que, cada, dia, es, una, nueva, oportunidad, para, aprender, y, crecer], []).

template([tienes, familia,'?'], [como, soy, un, programa, de, inteligencia, artificial, no, tengo, familia, ni, relaciones, personales, solo, 'Dante', fue, mi, creador], []).
%//
%//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
% templates mas especificos del tema carros

% industria
template([cuales, son, las, ultimas, novedades, '?'], ["tengo un alncance limitado ya que no tengo acceso a las noticias en tiempo real, pero puedo decirte el record de velocidad actual, novedades en seguridad, que esta pasando con la industria actualmente"], []). 
template([_,record, de, velocidad, '.'], ["La noticia mas sonada ultimamente es el nuevo record de velocidad de 531 km/h, alncazado por el Koenigsegg Jesko Absolut, que le quito el podio al chiron"], []). 
template([novedades, '.'], ["Los Sistemas de seguridad y de asistente de conduccion, la Inteligencia artificial, el Sistema de vision nocturna, los Parabrisas Inteligentes y la Electromovilidad, son algunos de los avances que se implementan"], []). 
template([que, esta, pasando, '?'], ["Desde la pandemia, pasando por las disrupciones en la cadena de suministro en semiconductores y otros componentes esenciales, asi como la tendencia que apunta hacia la movilidad electrica, todo suma a la agenda de la industria automotriz"], []). 

% Template para preguntar por coches
template([platicame, sobre, la, marca, toyota, '.'], ["Fundada en 1937, Toyota es una marca japonesa conocida por su innovacion en la fabricacion de automoviles y su enfoque en la eficiencia y confiabilidad."], []). 
template([platicame, sobre, la, marca, nissan, '.'], ["Fundada en 1933, Nissan es una marca japonesa que ha crecido para convertirse en una de las mayores fabricantes de automoviles del mundo, conocida por modelos como el Nissan Altima y el Nissan Rogue."], []). 
template([platicame, sobre, la, marca, mazda, '.'], ["Con origenes en 1920, Mazda es una marca japonesa que se destaca por su enfoque en el diseno distintivo y la tecnologia SKYACTIV que mejora la eficiencia y el rendimiento."], []). 
template([platicame, sobre, la, marca, subaru, '.'], ["Fundada en 1953, Subaru es conocida por sus vehiculos con traccion en las cuatro ruedas y motores boxer. La marca japonesa ha ganado popularidad por su enfoque en la durabilidad y el rendimiento en terrenos dificiles."], []). 
template([platicame, sobre, la, marca, honda, '.'], ["Establecida en 1948, Honda es una marca japonesa famosa por su ingenieria confiable y eficiente. Modelos como el Honda Civic y el Honda Accord han sido populares durante decadas."], []). 

template([platicame, sobre, la, marca, volkswagen, '.'], ["Fundada en 1937, Volkswagen es una marca alemana conocida por una variedad de modelos, desde el iconico Volkswagen Beetle hasta la linea moderna de Golf y Passat."], []). 
template([platicame, sobre, la, marca, bmw, '.'], ["BMW, fundada en 1916, es una marca de lujo alemana que se centra en vehiculos premium y de alto rendimiento, con una reputacion destacada en el diseno y la ingenieria de automoviles deportivos."], []). 
template([platicame, sobre, la, marca, mercedes_benz, '.'], ["Establecida en 1926, Mercedes-Benz es una marca alemana de lujo y prestigio, conocida por sus automoviles elegantes y tecnologia innovadora, representando la cima de la ingenieria automotriz."], []). 
template([platicame, sobre, la, marca, audi, '.'], [": Audi, parte del grupo Volkswagen, ha sido un jugador clave en la industria automotriz desde 1910, destacandose por su diseno moderno, tecnologia avanzada y rendimiento."], []). 

template([platicame, sobre, la, marca, ford, '.'], ["Fundada en 1903, Ford es una marca estadounidense que revoluciono la produccion en masa de automoviles. Famosa por modelos como el Ford Mustang y la serie de camionetas Ford F."], []). 
template([platicame, sobre, la, marca, chevrolet, '.'], ["Con raices en 1911, Chevrolet, parte de General Motors, es una marca estadounidense que ofrece una amplia gama de vehiculos, desde autos compactos hasta camionetas y SUVs."], []). 
template([platicame, sobre, la, marca, dodge, '.'], ["Dodge, fundada en 1900, es una marca estadounidense conocida por sus vehiculos deportivos y muscle cars, como el Dodge Challenger y el Dodge Charger."], []). 
template([platicame, sobre, la, marca, pontiac, '.'], ["Fundada en 1926, Pontiac fue una marca estadounidense que se destaco por sus automoviles de alto rendimiento. Sin embargo, la produccion de vehiculos Pontiac ceso en 2010."], []). 
template([platicame, sobre, la, marca, tesla, '.'], ["Fundada en 2003, Tesla es una marca estadounidense pionera en vehiculos electricos. Con modelos como el Tesla Model S y el Model 3, Tesla ha cambiado la percepcion de los automoviles electricos en la industria."], []). 


% Template para preguntar por motores
template([platicame, sobre, los, motores, en, v, '.'], ["Los motores en V tienen cilindros dispuestos en dos bancos en forma de V. Esto permite una construccion mas compacta y eficiente. Se utilizan tanto en configuraciones de cuatro como de ocho cilindros, brindando un equilibrio entre potencia y espacio, el numero es la cantidad de cilindros."], []). 
template([platicame, sobre, los, motores, en, linea, '.'], ["Los motores en linea tienen sus cilindros dispuestos en una sola fila. Son conocidos por su simplicidad, facilidad de mantenimiento y suavidad en la entrega de potencia, siendo comunes en configuraciones de cuatro y seis cilindros, el numero es la cantidad de cilindros."], []). 
template([platicame, sobre, los, motores, gasolina, '.'], ["Los motores a gasolina utilizan una chispa para encender la mezcla aire-combustible. Son comunes debido a su mayor densidad de energia, lo que resulta en un rendimiento potente. Sin embargo, pueden generar mas emisiones en comparacion con algunos motores alternativos."], []). 
template([platicame, sobre, los, motores, diesel, '.'], [" Los motores diesel encienden el combustible mediante la compresion en lugar de una chispa. Tienen mayor eficiencia termica y mejor rendimiento de combustible que los motores a gasolina. Suelen utilizarse en vehiculos de carga y automoviles de alta eficiencia."], []). 
template([platicame, sobre, los, motores, electricos, '.'], ["Los motores electricos funcionan mediante la conversion directa de energia electrica en movimiento. Son conocidos por su eficiencia, respuesta inmediata y menor impacto ambiental en comparacion con los motores de combustion interna. Tesla es una empresa notable que utiliza exclusivamente motores electricos en sus vehiculos."], []). 
template([platicame, sobre, los, motores, wankel, '.'], ["El motor Wankel utiliza un rotor triangular en lugar de pistones. Con menos partes moviles, son compactos y proporcionan un alto rendimiento. Se han utilizado en modelos de automoviles deportivos, como los de la marca Mazda."], []). 
template([platicame, sobre, los, motores, boxer, '.'], ["Los motores boxer tienen cilindros opuestos y horizontalmente opuestos, lo que crea un centro de gravedad mas bajo y mejora la estabilidad. Porsche y Subaru son conocidos por utilizar motores boxer en algunos de sus modelos."], []). 

% Template para preguntar por elementos de los motores
template([cuales, son, los, elementos, basicos, de, un, motor, '?'], ["Los elementos mas basicos son, cilindros, pistones, camara de combustion, arbol de levas y ciguenal"], []). 
template([cilindros, '.'], ["Son los tubos donde ocurre la combustion del combustible y la expansion de los gases. Un motor puede tener varios cilindros, dispuestos en linea, en V o en otras configuraciones."], []). 
template([pistones, '.'], ["Se mueven hacia arriba y hacia abajo dentro de los cilindros. La energia generada por la combustion empuja los pistones, convirtiendo la energia termica en movimiento mecanico."], []). 
template([camara, de, combustion, '.'], ["Es el espacio en la parte superior del cilindro donde ocurre la ignicion del combustible. Aqui, la mezcla de aire y combustible se enciende mediante una chispa en motores de gasolina o por compresion en motores diesel."], []). 
template([arbol, de, levas, '.'], [" Controla la apertura y cierre de las valvulas. Gira sincronizado con el ciguenal para regular la entrada de aire y combustible y la salida de gases de escape."], []). 
template([ciguenal, '.'], [" Convierte el movimiento lineal de los pistones en movimiento rotativo, que finalmente impulsa las ruedas del vehiculo."], []). 

% Template para preguntar sobre las nacionalidades
template([platicame, sobre, la, industria, en, japon, '.'], ["
La historia automotriz de Japon destaca con Toyota y Nissan, lideres en eficiencia. Europa, con Benz y Daimler, vio el dominio de BMW y Volkswagen. En America, Henry Ford revoluciono la produccion en masa. A lo largo del siglo XX, estas regiones compitieron y se adaptaron, con Japon en innovacion, Europa en rendimiento y calidad, y America ajustandose a desafios modernos, como la transicion a vehiculos electricos."], []). 
template([platicame, sobre, la, industria, en, europa, '.'], ["La historia automotriz en Europa se destaca con pioneros como Benz y Daimler, sentando las bases para marcas iconicas como BMW y Volkswagen. A lo largo del siglo XX, Europa lidero en rendimiento y calidad en la industria automotriz, destacando en tecnologias avanzadas y diseno innovador."], []). 
template([platicame, sobre, la, industria, en, america, '.'], ["La historia automotriz en America se caracteriza por el liderazgo de Henry Ford, quien revoluciono la produccion en masa. A lo largo del siglo XX, America se adapto y compitio en la industria automotriz, enfrentando desafios y destacando en la diversificacion de modelos. En tiempos modernos, la region se ajusta a desafios contemporaneos, incluyendo la transicion hacia vehiculos electricos."], []). 

% templates para preguntar por cadenas
template([dime, los, coches, con, motor, s(_), '.'], [flagmotor], [5]).

template([dime, los, coches, con, nacionalidad, s(_), '.'], [flagnacion], [5]).

template([dime, los, motores, que, usan, s(_), '.'], [flageselemtoesp], [5]).

template([dime, los, motores, que, usan, s(_), '.'], [flaginduccion], [5]).

template([s(_), es, un, elemento, de, un, motor, '?'], [flagelemento], [0]).

%//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

% Sintomas de la fiebre tifoidea
sintomas(fiebre_tifoidea, [fiebre_alta, dolor_de_cabeza, dolor_abdominal, dolor_muscular, fatiga, perdida_de_apetito, erupcion_cutanea, estrenimiento, diarrea, sintomas_gastrointestinales]).
sintoma(fiebre_alta).
sintoma(dolor_de_cabeza).
sintoma(dolor_abdominal).
sintoma(dolor_muscular).
sintoma(fatiga).
sintoma(perdida_de_apetito).
sintoma(erupcion_cutanea).
sintoma(estrenimiento).
sintoma(diarrea).
sintoma(sintomas_gastrointestinales).

% causas de la fiebre tifoidea
causas(fiebre_tifoidea, [ingestion_por_alimentos_contaminados, agua_contaminada, contacto_directo, portadores_cronicos, infeccion, bacteria_Salmonella_typhi]).
causa(ingestion_por_alimentos_contaminados).
causa(agua_contaminada).
causa(contacto_directo).
causa(portadores_cronicos).
causa(infeccion).
causa(bacteria_Salmonella_typhi).

% tratamientos y medicamentos para el cáncer de páncreas
tratamientos(fiebre_tifoidea, [antibioticos, hospitalizacion, reposo_y_cuidado, seguimiento_medico, vacunacion, higiene]).
tratamiento(antibioticos).
tratamiento(hospitalizacion).
tratamiento(reposo_y_cuidado).
tratamiento(seguimiento_medico).
tratamiento(vacunacion).
tratamiento(higiene).

% Relaciones
template([dime, cuales, son, los, sintomas, de, s(_), '.'], [flagsintomas], [6]).
template([dime, cuales, son, las, causas, de, s(_), '.'], [flagcausas], [6]).
template([dime, cuales, son, los, tratamientos, de, s(_), '.'], [flagtratamientos], [6]).

% Reglas para preguntas y respuestas sobre el cáncer de páncreas
template([que, es, la, fiebre, tifoidea, '?'], ['La fiebre tifoidea es una enfermedad infecciosa potencialmente mortal causada por la bacteria Salmonella Typhi, que suele transmitirse a traves del agua y los alimentos contaminados. Una vez ingerida, S. Typhi se multiplica y pasa al torrente circulatorio.'], []).

% sintomas
template([fiebre, alta, '.'], ["La fiebre es uno de los sintomas mas distintivos de la fiebre tifoidea y a menudo es continua, aumentando gradualmente a lo largo de varios dias."], []).
template([dolor, de, cabeza, '.'], ["Las personas con fiebre tifoidea a menudo experimentan dolores de cabeza intensos."], []).
template([dolor, abdominal, '.'], ["Dolor en el area del abdomen, que puede ser constante y localizado en la parte inferior derecha."], []).
template([dolor, muscular, '.'], ["Dolor en los musculos y articulaciones."], []).
template([fatiga, '.'], ["Sensacion de cansancio extremo y debilidad."], []).
template([perdida, de, apetito, '.'], ["Puede haber una disminucion del deseo de comer."], []).
template([erupcion, cutanea, '.'], ["Algunas personas pueden desarrollar una erupcion cutanea caracteristica llamada roseolas tifoideas, que son manchas de color rosa en el pecho y abdomen."], []).
template([estrenimiento, '.'], ["Puede haber cambios en los habitos intestinales."], []).
template([diarrea, '.'], ["Puede haber cambios en los habitos intestinales."], []).
template([sintomas, gastrointestinales, '.'], ["Nauseas, vomitos y malestar general en el area abdominal."], []).

% causas
template([ingestion, por, alimentos, contaminados, '.'], ["Consumir alimentos preparados o manipulados por personas portadoras de la bacteria Salmonella Typhi puede llevar a la infeccion. Los alimentos mas comunmente asociados con la transmision incluyen productos lacteos, mariscos, frutas y verduras contaminadas."], []).
template([agua, contaminada, '.'], ["Beber agua contaminada con la bacteria es otra via comun de transmision. El agua contaminada puede resultar de la falta de tratamiento adecuado de agua para consumo humano o de la contaminacion de suministros de agua por aguas residuales infectadas."], []).
template([contacto, directo, '.'], [" La bacteria tambien puede transmitirse a traves del contacto directo con una persona infectada. Esto puede ocurrir si una persona no lava adecuadamente las manos despues de ir al bano y luego toca alimentos o superficies que entran en contacto con la boca de otra persona."], []).
template([portadores, cronicos, '.'], ["Algunas personas pueden convertirse en portadoras cronicas de la bacteria sin mostrar sintomas significativos. Estas personas pueden seguir eliminando la bacteria en sus heces durante semanas, meses o incluso anios, y pueden representar una fuente continua de infeccion."], []).
template([infeccion, '.'], ["Causada por la bacteria Salmonella Typhi."], []).
template([bacteria, salmonella, typhi, '.'], ["Que suele transmitirse a traves del agua y los alimentos contaminados. Una vez ingerida, S. Typhi se multiplica y pasa al torrente circulatorio."], []).

% tratamiento
template([antibioticos, '.'], ["Los antibioticos son la piedra angular del tratamiento para la fiebre tifoidea. Los mas comunmente utilizados incluyen ciprofloxacina, levofloxacina y azitromicina. En algunos casos, se pueden utilizar otros antibioticos segun la sensibilidad de la bacteria a los medicamentos."], []).
template([hospitalizacion, '.'], ["En casos graves de fiebre tifoidea, la hospitalizacion puede ser necesaria. Esto es especialmente cierto si hay complicaciones, como perforacion intestinal o si la persona no puede tolerar los antibioticos orales."], []).
template([reposo, y, cuidado, '.'], [" Descanso adecuado y cuidado general son importantes durante la recuperacion. Esto incluye una dieta facil de digerir y la reposicion de liquidos y electrolitos para prevenir la deshidratacion."], []).
template([seguimiento, medico, '.'], ["Es esencial seguir las indicaciones del medico y completar el curso completo de antibioticos, incluso si los sintomas mejoran antes. Esto ayuda a prevenir la recaida y reduce el riesgo de desarrollar cepas resistentes a los antibioticos."], []).
template([vacunacion, '.'], ["La vacuna contra la fiebre tifoidea esta disponible y se recomienda especialmente para aquellos que viajan a areas donde la enfermedad es endemica."], []).
template([higiene, '.'], ["Practicar una buena higiene personal, como lavarse las manos con regularidad y asegurarse de que los alimentos y el agua sean seguros para el consumo, puede ayudar a prevenir la propagacion de la bacteria."], []).

%//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template([hola, _], ['Hola', 'como', estas, tu, '?'], []).
template([buendia, _], ['Buendia', 'Como', estas, tu, '?'], []).

template([yo, s(_), yo, soy, s(_),'.'], [por, que, 0, eres, 1, '?'], [1, 4]).
template([yo, s(_), tu, '.'], [por, que, 0, yo ,'?'], [1]).
template([yo, soy, s(_),'.'], [porque, eres, tu, 0, '?'], [2]).

% pregunta algo que le gusta a nagata
template([te, gustan, las, s(_), _], [flagLike], [3]).
template([te, gustan, los, s(_), _], [flagLike], [3]).
template([te, gusta, s(_), _], [flagLike], [2]).
template([te, gusta, s(_), _], [flagLike], [2]).


% pregunta algo que hace nagata
template([tu, eres, s(_), _], [flagDo], [2]).
template([tu, haces, s(_), _], [flagDo], [2]).
template([puedes, s(_), _], [flagDo], [2]).

% pregunta algo que es nagata
template([que, eres, tu, s(_)], [flagIs], [2]).
template([eres, s(_), '?'], [flagIs], [2]).

template([yo, pienso, que, _], [bueno, esa, es, tu, opinion], []).
template([porque, _], [esa, no, es, una, buena, razon, '.'], []).
template([yo, tengo, s(_), con, s(_), '.'], ['Tu', tienes, que, lidear, con, tu, 0, y, tu, 1, de, una, forma, madura, '.'], [2, 4]).
template([yo, s(_),  _], [yo, puedo, recomendarte, un, libro, acerca, de, ese,  problema], []).
template([por, favor, s(_), _], ['No', yo, no, puedo, ayudar, ',', yo, solo, soy, una, maquina], []). 
		 template([di, me, un, s(_), _], ['No', yo, no , puedo, ',', soy, mala, para, eso], []).

	  
template(_, ['Una', disculpa, desconozco, de, que, hablas, ni, te, topo,'.'], []). 

% Lo que le gusta a nagata : flagLike
nagataLikes(X, R):- likes(X), R = ['Claro', a, mi, me, gusta, los, X].
nagataLikes(X, R):- \+likes(X), R = ['No', a, mi, no, me, gusta, los, X].
likes(manzanas).
likes(ponis).
likes(zombies).
likes(computadoras).
likes(carros).
likes(toyota).
likes(honda).
likes(chevrolet).
likes(dodge).
likes(bmw).

% lo que hace nagata: flagDo
nagataDoes(X, R):- does(X), R = ['Si', yo, se, X].
nagataDoes(X, R):- \+does(X), R = ['No', yo, no,  X ,'.', eso, es, muy, dificil, para, mi].
does(nagata).
does(responder).
does(platicar).
does(hablar).
does(fiebre).
does(fiebres).
does(fiebre_tifoidea).



% lo que es nagata: flagIs
nagataIs(X, R):- is0(X), R = ['Si', yo, soy, X].
nagataIs(X, R):- \+is0(X), R = ['No', yo, no, soy, X].
is0(tonto).
is0(raro).
is0(lindo).
is0(bien).
is0(feliz).
is0(redundante).

match([],[]).
match([], _):- true.

match([S|Stim],[I|Input]) :-
	atom(S), 
	% si I es un s(X) devuelve falso
	S == I,
	match(Stim, Input),!.

match([S|Stim],[_|Input]) :-
% I es un s(X), lo ignoro y continúo con el resto de la lista
	\+atom(S),
	match(Stim, Input),!.

replace0([], _, _, Resp, R):- append(Resp, [], R),!.

% nagata likes:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagLike,
	nagataLikes(Atom, R).

%//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
% los motroes que esta en las marcas de coches: flagmotor
esmotor(X, R):- motorenuso(X, Y), R = ['Los', coches, con, motor, X, son,':', Y].

%es un elemento:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagmotor,
	esmotor(Atom, R).

% los motroes que esta en las marcas de coches: flagmotor
esnacionalidad(X, R):- nacion(X, Y), R = ['Los', coches, con, nacionalidad, X, son,': ', Y].

%es un elemento:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagnacion,
	esnacionalidad(Atom, R).

% los motroes que usan algo en especifico: flageselemtoesp
eselemtoesp(X, R):- elemtoesp(X, Y), R = ['Los', motores, que, usan, X, son,': ', Y].

%es un elemento:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flageselemtoesp,
	eselemtoesp(Atom, R).

% los motroes que usan induccion forzada: flaginduccion
esinduccion(X, R):- induccion(X, Y), R = ['Los', motores, que, usan, X, son,': ', Y].

%es un elemento:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flaginduccion,
	esinduccion(Atom, R).

% los elementos de un motor: flagelemento
eselemento(X, R):- elemento(X), R = ['Si', X, es, un, elemento, de, un, motor].
eselemento(X, R):- \+elemento(X), R = ['No', X, no, es, un, elemento, de, un, motor].

%es un elemento:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagelemento,
	eselemento(Atom, R).

% sintomas de la fiebre tifoidea
essintomas(X, R):- sintomas(X, Y), R = ['Los', sintomas, de, la, X, son,':', Y].

%es un elemento:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagsintomas,
	essintomas(Atom, R).

% causas de la fiebre tifoidea
escausas(X, R):- causas(X, Y), R = ['Las', causas, de, la, X, son,':', Y].

%es un elemento:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagcausas,
	escausas(Atom, R).

% tratamientos de la fiebre tifoidea
estratamientos(X, R):- tratamientos(X, Y), R = ['Los', tratamientos, de, la, X, son,':', Y].

%es un elemento:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagtratamientos,
	estratamientos(Atom, R).
	
%//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

% no mover

% nagata does:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagDo,
	nagataDoes(Atom, R).

% nagata is:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagIs,
	nagataIs(Atom, R).

replace0([I|Index], Input, N, Resp, R):-
	length(Index, M), M =:= 0,
	nth0(I, Input, Atom),
	select(N, Resp, Atom, R1), append(R1, [], R),!.

replace0([I|Index], Input, N, Resp, R):-
	nth0(I, Input, Atom),
	length(Index, M), M > 0,
	select(N, Resp, Atom, R1),
	N1 is N + 1,
	replace0(Index, Input, N1, R1, R),!.
```