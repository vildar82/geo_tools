(defun check-point-to-drawning (point / flag nn obj_vba points-corrent-drawning points_list ss x)
  ;проверка наличия точки в текущем чертеже
  (setq ss (ssget "X" '((0 . "AECC_COGO_POINT"))))
  (if ss
    (progn
      (setq points-corrent-drawning (convert_ss_to_list ss))
      (mapcar '(lambda (x)
		 (setq obj_vba(vlax-ename->vla-object x))
		 (setq points_list
			(append points_list
				(list (list
					(vlax-get-property  obj_vba "NORTHING")
					(vlax-get-property  obj_vba "EASTING")
					(vlax-get-property  obj_vba "ELEVATION")
					))))
		 )
	      points-corrent-drawning)
      (setq nn 0)
      (while (not(eq nn (length points_list)))
	(bump)
	(if (equal point
		   (nth nn points_list))
	  (progn
	    (setq nn (length points_list))
	    (setq flag T)
	    )
	  (setq nn (1+ nn))
	  )
	)
      )
    )
  flag
  )