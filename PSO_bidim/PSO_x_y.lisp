;; Declaro una variable global, para ser utilizada en la función eval 
(defvar x)
(defvar y)


;;Función Rosenbrock 
;(setq fitness '(+ (expt (- 1 x) 2) (* 100 (expt (-  y (expt  x 2)) 2))))


;;Función Rastrigin
(setq fitness '(- (+ 20 x y  ) (* 10 (cos (* 2 PI x))) (* 10 ( cos (* 2 PI y)))))

;Defino función que genera partículas con su respectiva posición y velocidad de manera aleatoria
;Parámetros: 
;			*númeroPartículas: Número de partículas a generar. (Entero)
;			*listaLímites: Una lista que contiene los límites del espacio de búsqueda.
(defun Genera_VectorAleatorio (númeroPartículas  listaLímites)
	(let ((listaPosiciones nil)
		 (listaVelocidades nil)
		 (Delta nil))
  
	(setq Delta (-(second listaLímites) (first listaLímites)))
	(dotimes (i númeroPartículas)
		(setf listaPosiciones (append (list (+ (random Delta) (first listaLímites))) listaPosiciones))
		(setf listaVelocidades (append (list (+ (random Delta) (first listaLímites))) listaVelocidades)) )
	(list listaPosiciones listaVelocidades)))




(defun Genera_Partículas (númeroPartículas listaLímites)
	(let ((posición_velocidad_x nil) 
		  (posición_velocidad_y nil))

	(setq posición_velocidad_x (Genera_VectorAleatorio númeroPartículas  (first listaLímites)))
	(setq posición_velocidad_y (Genera_VectorAleatorio númeroPartículas (second listaLímites)))

	(list (mapcar #'list  (first posición_velocidad_x) (first posición_velocidad_y ))
		   (mapcar #'list (second posición_velocidad_x) (second posición_velocidad_y )))))





;Defino función que obtiene mejor fitness de acuerdo a las partículas evaluadas.
;Parámetros:
;		*funciónFitness: Cualquier función de formato lisp que dependa de la variable x.
;		*listaPosiciones: Lista de posiciones en x de cada partícula.
;		*Optimización: Parametro que se refiere a que tipo de optimización se desea, ya sea maximización o minimización.
;Ésta función regresa una lista con la posición de la partícula con el mejor fitness y su fitness de acuerdo a la optimización deseada.

(defun Mejor_Fitness (funciónFitness listaPosiciones Optimización)
	(let ((fitness nil)
		 (mejorFitness nil)
		 (posiciónMejor_fitness nil))

	(setq fitness (mapcar #'(lambda (A) (setf x (first A ) ) (setf y (second A)) (eval funciónFitness)) listaPosiciones))
	(setq mejorFitness (apply Optimización fitness))
	(setq posiciónMejor_fitness (position mejorFitness fitness))
	(setq posiciónMejor_fitness (nth posiciónMejor_fitness listaPosiciones))
	(list posiciónMejor_fitness mejorFitness)))





;Defino función donde se evaluan las velocidades de acuerdo a la siguiente fórmula de PSO:
; Vi(t+1)= w Vi(t) + C1 r1 [x' (t) - xi(t)] + C2 r2 [g(t) - xi (t)] xi (t+1) = xi(t)+ Vi(t+1)
;Parámetros:
;		* W : Factor de inercia. (Constante)
;		* C1 y C2: Constantes de peso con respecto al mejor local y al mejor global.
;		* listaPosiciones: Lista de posiciones en x de cada partícula
;		*listaVelocidades: Lista de velocidades en x de cada partícula.
; La función regresa una lista con las nuevas velocidades de cada partícula.

(defun Evaluación_Velocidades (W C1 C2 listaPosiciones listaVelocidades
							 MejorPartículaActual MejorPartículaGlobal)



	(mapcar #'(lambda (X V) (+ (* W V) (* C1 (random 1.0) (- MejorPartículaActual X)) (* C2 (random 1.0) (- MejorPartículaGlobal X)) )) 
		listaPosiciones listaVelocidades))


;;;;;;;;;;;;;;;;;;;;

(defun Velocidades (W C1 C2 listaPosiciones listaVelocidades MejorPartículaActual MejorPartículaGlobal)
	(let ((velocidades_x nil)
		 (velocidades_y nil))

	(setq velocidades_x (Evaluación_Velocidades (first W) (first C1) (first C2) (mapcar #'first listaPosiciones)
						(mapcar #'first listaVelocidades) (first MejorPartículaActual) (first MejorPartículaGlobal)))
n
	(setq velocidades_y (Evaluación_Velocidades (second W) (second C1) (second C2) (mapcar #'second listaPosiciones)
						(mapcar #'second listaVelocidades) (second MejorPartículaActual) (second MejorPartículaGlobal)))

	(mapcar #'list velocidades_x velocidades_y)))






;Defino función que actualiza las posiciones de cada partícula, en cada iteración realizada.
;Parámetros:
;		* listaPosiciones: Lista de posiciones en x de cada partícula
;		*listaVelocidades: Lista de velocidades en x de cada partícula

(defun Nuevas_Posiciones (listaPosiciones listaVelocidades)
	(mapcar #'(lambda ( posición velocidad ) (mapcar #'+ posición velocidad )) listaPosiciones listaVelocidades))



;Defino la función principal, la cual lleva acabo la realización del algoritmo PSO, para maximización o minimización de una función.
;Ejemplo de como inicializar el programa:
; (PSO '(SIN x) 0.371312 0.693147 0.693147 15 'max '(0 6.28) 30)    
;Usando los valores de la versión estándar, se tomaron los valores de w= 1/(2+ln2) y C1=C2 = 0.5 + ln2



(defun PSO (Fitness W C1 C2 N_Partículas Optimización LimiteDe_EspacioBúsqueda Nmáximo_Iteraciones)
	(let ((Posiciones nil) ;;Posiciones de las Partículas 
		 (Velocidades nil)
		 (MejorGlobal nil)
		 (MejorActual nil)
		 (NuevasVelocidades nil)
		 (Iteraciones nil)
		 (NuevasPosiciones nil)
		 (Posiciones_Velocidades nil))

	(setq Posiciones_Velocidades (Genera_Partículas N_Partículas LimiteDe_EspacioBúsqueda))
	(setq Posiciones (first Posiciones_Velocidades))
	(setq Velocidades (second Posiciones_Velocidades))

	(dotimes (i Nmáximo_Iteraciones)

		

	(setq MejorActual (Mejor_Fitness Fitness Posiciones Optimización))

	(if (null MejorGlobal)
		(setq MejorGlobal MejorActual)

		(if (equal (second MejorActual) (apply Optimización (list (second MejorActual) (second  MejorGlobal)))) 
			(setq MejorGlobal MejorActual))
	)

	(setq NuevasVelocidades (Velocidades W C1 C2 Posiciones Velocidades (first MejorActual) (first MejorGlobal)))
	(setq NuevasPosiciones (Nuevas_Posiciones Posiciones NuevasVelocidades))
	(setq Posiciones NuevasPosiciones)
	(setq Velocidades NuevasVelocidades)
	(if (equal 1 (length (remove-duplicates Posiciones )))
		(progn 

		(setq Iteraciones i)
		(return i)))
		(setq Iteraciones i))

	(print (first MejorGlobal))
	(print Iteraciones)))
