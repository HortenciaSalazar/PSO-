
(defun Ackley (partícula)
	(let ((Planos nil)
		(Fintness nil)
		(Sumatoria1 nil)
		(Sumatoria2 nil)
		(c (* 2 PI))
		(a 20)
		(b 0.2))

	(setq Planos (first partícula ))
	(setq Sumatoria1 (apply #'+ (mapcar #'(lambda (x) (expt x 2 )) Planos)))
	(setq Sumatoria2 (apply #'+ (mapcar #'(lambda (x) (cos (* c x ))) Planos)))

	(setq Fitness (+ (* (- a) (exp(* (- b) (sqrt (/  Sumatoria1 (length Planos)))) ))
		(- (exp (/ Sumatoria2 (length Planos)))) a (exp 1)))))

;;;;;Defino función que genera partículas con su respectiva posición y velocidad de manera aleatoria
;Parámetros: 
;			*númeroPartículas: Número de partículas a generar. (Entero)
;			*listaLímites: Una lista que contiene los límites del espacio de búsqueda.


(defun Genera_Partículas (númeroPartículas Límites númeroDimensiones )
	(let ((ListaPartículas nil)
		 (partículaGenerada nil)
		 (VelocidadGenerada nil )
		 (listaVelocidades nil)
		 (Delta nil))
  
	(setq Delta (-(second Límites) (first Límites)))
	(dotimes (i númeroPartículas) ; "i" es el número de partícula actual

		(setq partículaGenerada nil)
		(setq VelocidadGenerada nil)

		(dotimes (d númeroDimensiones) ;; "d" es el número de dimensión actual.

		(setf partículaGenerada (append (list (+ (random Delta) (first Límites))) partículaGenerada))
		(setf VelocidadGenerada (append (list (+ (random Delta) (first Límites))) VelocidadGenerada)) )

		(setf ListaPartículas (append ListaPartículas (list partículaGenerada )))
		
		(setf ListaVelocidades  (append ListaVelocidades  (list VelocidadGenerada )))
		)

	(list ListaPartículas ListaVelocidades)


	))
 







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

	(setq fitness (mapcar #'(lambda (A)  (funcall funciónFitness A)) listaPosiciones))
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



(defun Limita_Posición (Posiciones Límites)

	(let ((Límite_SuperiorX nil)
		(Límite_SuperiorY nil)
		(Límite_InferiorX nil)
		(Límite_InferiorY nil))

	(setq Límite_SuperiorX (second (first Límites)))
	(setq Límite_InferiorX (first (first Límites)))
	(setq Límite_SuperiorY (second (second Límites)))
	(setq Límite_InferiorY (first (second Límites)))


	(mapcar #'(lambda (Posición) 
				(if (< (first Posición) Límite_InferiorX) (setf (first Posición) Límite_InferiorX)
					(if (> (first Posición) Límite_SuperiorX) (setf (first Posición) Límite_SuperiorX)))

				(if (< (second Posición) Límite_InferiorY) (setf (second Posición) Límite_InferiorY)
					(if (> (second Posición) Límite_SuperiorY) (setf (second Posición) Límite_SuperiorY)))
				Posición) Posiciones)))


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
	(setq Posiciones (Limita_Posición Posiciones LimiteDe_EspacioBúsqueda))
	(setq Velocidades NuevasVelocidades)
	(if (equal 1 (length (remove-duplicates Posiciones )))
		(progn 

		(setq Iteraciones i)
		(return i)))
		(setq Iteraciones i))

	(print (first MejorGlobal))
	(print Iteraciones)
	MejorGlobal))



;;============================================================================================================================================
;;==========================================================================================================================================
(defun inserta (resultados_ordenados Convergencia_Actual)
	(if (null resultados_ordenados)
		(list Convergencia_Actual)
		(if (< (second Convergencia_Actual) (second (first resultados_ordenados)))
			(append (list Convergencia_Actual) resultados_ordenados)
			(append (list (first resultados_ordenados)) (inserta (rest resultados_ordenados) Convergencia_Actual)))
		)
	)





(defun Calcula_Media (resultados_ordenados Numero_Veces)
  (let ((media nil))
  	(setq media '((0 0) 0))
  	(dolist (resultado resultados_ordenados)
  		(setf (first (first media)) (+ (first (first media)) (first (first resultado))))
  		(setf (second (first media)) (+ (second (first media)) (second (first resultado))))
  		(setf (second media) (+ (second media) (second resultado)))
  	)
  	(setf (first (first media)) (/ (first (first media)) Numero_Veces))
  	(setf (second (first media)) (/ (second (first media)) Numero_Veces))
  	(setf (second media) (/ (second media) Numero_Veces))
  	media
  	))


(defun Obtiene_Mediana (resultados_ordenados Numero_Veces)
	(nth (- (floor (/ Numero_Veces 2)) 1) resultados_ordenados)
)



(defun Calcula_Varianza (media resultados_ordenados Numero_Veces)
	(let ((varianza nil))
		
		(setq varianza '((0 0) 0))
		(dolist (resultado resultados_ordenados)
			(setf (first (first varianza)) (+ (first (first varianza)) (expt (- (first (first resultado)) (first (first media))) 2)))
			
			(setf (second (first varianza)) (+ (second (first varianza)) (expt (- (second (first resultado)) (second (first media))) 2)))
		
			(setf (second varianza) (+ (second varianza) (expt (- (second resultado) (second media )) 2 ) ) )
			
		)

		(setf (first (first varianza)) (/ (first (first varianza)) Numero_Veces ))
		(setf (second (first varianza)) (/ (second  (first varianza)) Numero_Veces))
		(setf (second varianza) (/ (second varianza) Numero_Veces))
		varianza
))



(defun obtener_desviación (varianza)
	(let ((desviación nil))
		(setq desviación '((0 0) 0))

			(setf (first (first desviación)) (sqrt (first ( first varianza))))
			(setf (second (first desviación)) (sqrt (second (first varianza))))
			(setf (second desviación) (sqrt (second varianza)))
			desviación
	))

 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Prueba_PSO (Numero_Veces Fitness W C1 C2 N_Partículas Optimización LimiteDe_EspacioBúsqueda Nmáximo_Iteraciones) 
;;Función con prioridad más alta que PSO para ejecutar PSO con los mismos parametros las N veces (30 como te dijeron)
	(let ((stream nil) (Convergencia_Actual nil) (resultados_ordenados nil)
		   (posición nil) (fitnes nil) (media nil) (mediana nil) (desviación nil) (varianza nil))




		(with-open-file (stream "C:\\Users\\Diana\\Desktop\\Pruebas_TT\\Ackley\\Ackley.txt" :direction :output  ;; ponle el nombre y ruta
                                   :if-exists :supersede)
	
			(setq media 0); inicializas la variable para sumarle todos los resultados y al final sacar la media
			(dotimes (i Numero_Veces)

				(setq Convergencia_Actual (PSO Fitness W C1 C2 N_Partículas Optimización LimiteDe_EspacioBúsqueda Nmáximo_Iteraciones) )
				(setq posición (first Convergencia_Actual))
				(setq fitnes (second Convergencia_Actual))
				(format stream "~a ~T ~a" posición fitnes);; guardas cada dato en la archivo asi "(x y)   fitnes"separados por tabulador ~T
				;;(format stream "~a" fitnes)  ;; si sólo quieres guardar los fitnes y no las posiciones
				(terpri stream)
				;;; con esto ya se ejecuta el PSO las N veces que le digas y guarda los resultados en el archivo
				;;; ahora con esto mismo podemos agregar el calculo de la media, mediana y desviación estandar
				(setq resultados_ordenados (inserta resultados_ordenados Convergencia_Actual)) ; una función que te tiene el historial de los resultados ordenados e inserta el nuevo de acuerdo a su valor
				;; ésta función al final te dejara los resultados ordenados y sólo escoges el de en medio para la mediana
				;; al final te dejará una lista con todos  los resultados así: '(((x1 y1) fitnes1) ((x2 y2) fitnes2) ... ((xn yn) fitnesn)) EN ESE ORDEN DE menor a mayor en fitnes
			)
		(setq media (Calcula_Media resultados_ordenados Numero_Veces) )
		(setq mediana (Obtiene_Mediana resultados_ordenados Numero_Veces))
		(setq varianza (Calcula_Varianza media resultados_ordenados Numero_Veces))
		(setq desviación (obtener_desviación varianza))	
		;; guardas en el archivo los resultados al final
		(terpri stream)
		(format stream "Media=~a ~T ~a" (first media) (second media))
		(terpri stream)
		(format stream "Mediana=~a ~T ~a" (first mediana) (second mediana))
		(terpri stream)
		(format stream "Desviación estandar=~a ~T ~a" (first desviación) (second desviación))
		(terpri stream)
		(print resultados_ordenados)
		(terpri stream)

		)  ;;; hasta aqui se termina y tienes todo en el archivo y en la lista de historial con todos los resultados ordenados de acuerdo al fitnes, aquí se cierra el archivo
		
))

