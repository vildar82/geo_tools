(defun calculation-geodesic-straight-job (input-list / azimuth north east)
  ; вычисление прямой геодезической задачи вместе с тригонометрическим нивелированием
  ;|
  (calculation-geodesic-straight-job
    (list
      (list "station-point" (list 100.0 100.0 100.0))
      (list "base-azimuth" 0.0)
      (list "base-horizon-angle" 0.0)
      (list "oblique-distance" 72.284)
      (list "horizon-angle" 45.0)
      (list "vertical-angle" 11.97675296)
      (list "h-instrument" 0.0)
      (list "h-target" 0.0)
      )
    )
  |;
  (setq azimuth (get-item-by-name-in-list input-list "horizon-angle"))
  (setq north (+
	       (car(get-item-by-name-in-list input-list "station-point"))
	       (* (distance-to-horizon
		    (get-item-by-name-in-list input-list "oblique-distance")
		    (get-item-by-name-in-list input-list "vertical-angle"))
		  (cos (convert-grad-rad azimuth))
		  )
	       ))
  (setq east (+
		(cadr(get-item-by-name-in-list input-list "station-point"))
		(* (distance-to-horizon
		     (get-item-by-name-in-list input-list "oblique-distance")
		     (get-item-by-name-in-list input-list "vertical-angle"))
		   (sin (convert-grad-rad azimuth))
		   )
		))
  (setq elevation (+
		    (caddr(get-item-by-name-in-list input-list "station-point"))
		    (calculation-h-trig-nivelir
		      (get-item-by-name-in-list input-list "oblique-distance")
		      (get-item-by-name-in-list input-list "vertical-angle")
		      (get-item-by-name-in-list input-list "h-instrument")
		      (get-item-by-name-in-list input-list "h-target"))
		    ))
  (list north east elevation)
  )