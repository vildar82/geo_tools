(defun C:export_cogo_points_to_gsi
       (/ list_num_points putTXTfile putSDR33file putTXTfile SDR33file C3D_point number-item string)
  (geo_tools-help "export_cogo_points_to_SDR33file")
  (load_global_variable)
  (if
    C3D_active_document
    (progn
      (setq list_num_points (vlax-invoke C3D_active_document 'SelectPoints))
      (setq putTXTfile(strcat
			(getvar "DWGPREFIX")
			(substr (getvar "DWGNAME") 1 (-(strlen (getvar "DWGNAME"))4))
			"_"
			(vlax-get (vlax-invoke C3D_points 'find (car list_num_points)) 'Layer)
			)
	    )
      (setq putSDR33file (getfiled "Имя сохраняемого файла..." putTXTfile "gsi" 1))
      (setq SDR33file (open putSDR33file "w"))
      (setq number-item 1)
      (foreach num_point list_num_points
	(setq C3D_point (vlax-invoke C3D_points 'find num_point))
	(if (setq string (convert-point-to-gsi-string
			   (list
			     (cons "number-item" number-item)
			     (cons "number" (vlax-get C3D_point 'RawDescription))
			     (cons "N" (vlax-get C3D_point 'northing))
			     (cons "E" (vlax-get C3D_point 'easting))
			     (cons "Z" (vlax-get C3D_point 'elevation))
			     (cons "code" ""))))
	  (write-line string SDR33file)
	  )
	(setq number-item (1+ number-item))
	)
      (close SDR33file)
      )
    (alert "Выполнение комманды предполагает ее запуск в AutoCAD Civil 3D!\nТекущий AutoCAD им не является или представляет собой не поддерживаемую(временно?) версию.")
    )
  (princ)
  )