(defun Genera_Partículas (númeroPartículas Límites númeroDimensiones )
	(let ((ListaPartículas nil)
		 (partículaGenerada nil)
		 (VelocidadGenerada nil )
		 (Delta nil))
  
	(setq Delta (-(second Límites) (first Límites)))
	(dotimes (i númeroPartículas) ; "i" es el número de partícula actual

		(setq partículaGenerada nil)
		(setq VelocidadGenerada nil)

		(dotimes (d númeroDimensiones) ;; "d" es el número de dimensión actual.

		(setf partículaGenerada (append (list (+ (random Delta) (first Límites))) partículaGenerada))
		(setf VelocidadGenerada (append (list (+ (random Delta) (first Límites))) VelocidadGenerada)) )

		(setf ListaPartículas (append ListaPartículas (list (list partículaGenerada VelocidadGenerada)))))
		ListaPartículas))