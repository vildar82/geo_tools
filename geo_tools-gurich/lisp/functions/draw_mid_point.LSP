(defun C:draw_mid_point (/ list-coordinates list-points ssnabor vla-obj c3d_point flag length-list mid-point point-obj)
  ;чертит среднюю из выбранных точек
  (if (setq ssnabor (ssget '((0 . "POINT,AECC_COGO_POINT"))))
    (progn
      (setq list-points (convert_ss_to_list ssnabor))
      (foreach x list-points
	(setq vla-obj (vlax-ename->vla-object x))
	(if (=(vla-get-ObjectName vla-obj) "AcDbPoint")
	  (setq list-coordinates (append list-coordinates(list(vlax-safearray->list(vlax-variant-value(vla-get-Coordinates vla-obj))))))
	  (if (get-civil-or-not)
	    (progn
	      (setq C3D_point (vlax-invoke C3D_points 'find (vlax-get vla-obj 'Number)))
	      (setq list-coordinates
		     (append list-coordinates
			     (list(list(vlax-get C3D_point 'easting)(vlax-get C3D_point 'northing)(vlax-get C3D_point 'elevation)))))
	      (setq flag T)
	      )
	    )
	  )
	)
      (setq length-list (length list-coordinates))
      (setq mid-point (list
			(/(apply '+ (mapcar ' car list-coordinates)) length-list)
			(/(apply '+ (mapcar ' cadr list-coordinates)) length-list)
			(/(apply '+ (mapcar ' caddr list-coordinates)) length-list)
			)
	    )
      (if (and(get-civil-or-not)flag)
	(progn
	  (setq point-obj (vla-add C3D_points(vlax-3d-point mid-point)))
	  (vlax-put-property point-obj 'Description "MID_POINT")
	  )
	(entmake (list '(0 . "POINT")(cons 10 mid-point)))
	)
      )
    )
  (princ)
  )