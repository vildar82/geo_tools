(defun calculation-coor-of-raw (structure-list / new-point st-list st-list-of-catalog)
  ;вычисляет структурированные RAW данные вычисленные точки добавляются в каталог координат
  (foreach x (get-item-by-name-in-list structure-list "observations")
    (setq st-list (car x))
    (setq st-list-of-catalog (get-atribut-in-list-be-value(get-item-by-name-in-list structure-list "catalog-coord") "name" (get-item-by-name-in-list st-list "station-name")))
    (setq x (cdr x))
    (foreach y x
      (setq new-point
	     (calculation-geodesic-straight-job
	       (list
		 (list "station-point" (list(get-item-by-name-in-list st-list-of-catalog "N")(get-item-by-name-in-list st-list-of-catalog "E")(get-item-by-name-in-list st-list-of-catalog "Z")))
		 (list "base-azimuth" (get-item-by-name-in-list st-list "base-azimuth"))
		 (list "base-horizon-angle" (get-item-by-name-in-list st-list "base-horizon-angle"))
		 (list "oblique-distance" (get-item-by-name-in-list y "oblique-distance"))
		 (list "horizon-angle" (get-item-by-name-in-list y "horizon-angle"))
		 (list "vertical-angle" (get-item-by-name-in-list y "vertical-angle"))
		 (list "h-instrument" (get-item-by-name-in-list st-list "h-instrument"))
		 (list "h-target" (get-item-by-name-in-list y "h-target"))
		 )
	       )
	    )
      (setq structure-list
	     (put-item-by-name-in-list structure-list "catalog-coord"
	       (append
		 (get-item-by-name-in-list structure-list "catalog-coord")
		 (list
		   (list
		     (list "genesis" "calculation")
		     (list "name" (get-item-by-name-in-list y "name"))
		     (list "N" (car new-point))
		     (list "E" (cadr new-point))
		     (list "Z" (caddr new-point))
		     (list "h-target" (get-item-by-name-in-list y "h-target"))
		     (list "temp" (get-item-by-name-in-list y "temp"))
		     (list "press" (get-item-by-name-in-list y "press"))
		     (list "prism" (get-item-by-name-in-list y "prism"))
		     (list "data" (get-item-by-name-in-list y "data"))
		     (list "time" (get-item-by-name-in-list y "time"))
		     (list "code" (get-item-by-name-in-list y "code"))
		     ))))
	    )
      )
    )
  structure-list
  )