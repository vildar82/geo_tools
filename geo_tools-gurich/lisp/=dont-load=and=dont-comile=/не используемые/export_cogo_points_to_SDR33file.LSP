(defun C:export_cogo_points_to_SDR33file
       (/ list_num_points putTXTfile putSDR33file putTXTfile SDR33file C3D_point)
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
      (setq putSDR33file (getfiled "Имя сохраняемого файла..." putTXTfile "sdr" 1))
      (setq SDR33file (open putSDR33file "w"))
      (foreach num_point list_num_points
	(setq C3D_point (vlax-invoke C3D_points 'find num_point))
	(write-line (strcat
		      "08TP"
		      (edit_str_adding_probel(edit_str_trim_for_nunber_symbols(edit_translit_str(vlax-get C3D_point 'RawDescription))15 nil) 16 T)
		      (edit_str_adding_probel (rtos (vlax-get C3D_point 'northing) 2 3) 16 nil)
		      (edit_str_adding_probel (rtos (vlax-get C3D_point 'easting) 2 3) 16 nil)
		      (edit_str_adding_probel (rtos (vlax-get C3D_point 'elevation) 2 3) 16 nil)
		      (edit_str_adding_probel " " 16 nil)
		      ) SDR33file)
	)
      (close SDR33file)
      )
    (alert "Выполнение комманды предполагает ее запуск в AutoCAD Civil 3D!\nТекущий AutoCAD им не является или представляет собой не поддерживаемую(временно?) версию.")
    )
  (princ)
  )
