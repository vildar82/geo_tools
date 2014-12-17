(defun C:export_cogo_points_to_be_file ( / DESCRIPTION-STRING KATALOG-LIST LIST-DIALOG LIST_NUM_POINTS NUMBER-STRING POINT-LIST PUT-INPUT-FILE)
  (load_global_variable)
  (geo_tools-help "export_cogo_points_to_be_file")
  (if
    C3D_active_document
    (progn
      
      (if (not(vl-catch-all-error-p(setq list_num_points (vl-catch-all-apply 'vlax-invoke (list C3D_active_document 'SelectPoints)))))
	(progn
	  (foreach num_point list_num_points
	    (setq C3D_point (vlax-invoke C3D_points 'find num_point))
	    (setq point-list
		   (append point-list
			   (list
			     (list
			       (cons "name"(vlax-get C3D_point 'name))
			       (cons "number"(itoa(vlax-get C3D_point 'number)))
			       (cons "N"(vlax-get C3D_point 'northing))
			       (cons "E"(vlax-get C3D_point 'easting))
			       (cons "Z"(vlax-get C3D_point 'elevation))
			       (cons "description"(vlax-get C3D_point 'RawDescription))
			       )))))
	  (setq put-input-file
		 (strcat
		   (getvar "DWGPREFIX")
		   (substr (getvar "DWGNAME") 1 (-(strlen (getvar "DWGNAME"))4))
		   "_"
		   (vlax-get (vlax-invoke C3D_points 'find (car list_num_points)) 'Layer)
		   )
		)
	  
	  (if(setq list-dialog (dialog-export-cogo-points))
	    (if(setq put-input-file (getfiled "Имя сохраняемого файла..." put-input-file (cdr(assoc "format-file" list-dialog)) 1))
	      (progn
		
		
		(setq description-string(cdr(assoc "description" list-dialog)))
		(setq number-string(cdr(assoc "number" list-dialog)))
		(foreach x point-list
		  (setq katalog-list (append katalog-list
					     (list
					       (list
						 (cons "number" (edit-string-replase-substr
								  (edit-string-replase-substr
								    (edit-string-replase-substr number-string
								      "[D]"(cdr(assoc "description" x)))
								    "[P]" (cdr(assoc "number" x)))
								  "[N]"(cdr(assoc "name" x))))
						 (cons "N" (cdr(assoc "N" x)))
						 (cons "E" (cdr(assoc "E" x)))
						 (cons "Z" (cdr(assoc "Z" x)))
						 (cons "description" (edit-string-replase-substr
								       (edit-string-replase-substr
									 (edit-string-replase-substr description-string
									   "[D]"(cdr(assoc "description" x)))
									 "[P]" (cdr(assoc "number" x)))
								       "[N]"(cdr(assoc "name" x)))))
					       )))
		  )
		(write-katalog-coor-in-file katalog-list put-input-file)
		)))
	  )
	)
      )
    (alert "Выполнение комманды предполагает ее запуск в AutoCAD Civil 3D!\nТекущий AutoCAD им не является или представляет собой не поддерживаемую(временно?) версию.")
    )
  (princ)
  )

(defun dialog-export-cogo-points (/ dcl_id last-selekt result last-selekt list-dialog-export-cogo-points last-selekt)
  (setq dcl_id (geo-tools-load-dialog "d_export_cogo_points"))
  (setq list-dialog-export-cogo-points (vlax-ldata-get "geo_tools-data-dialogs" "dialog-export-cogo-points"))
  (setq last-selekt "description")
  (setq format-file-list (list (list "Nikon PNEZD" "lst")
			       (list "Sokkia SDR33" "sdr")
			       (list "Leica GSI" "gsi")))
			       
			       
  (new_dialog "d_export_cogo_points" dcl_id)
  (start_list "format-file")
  (foreach x format-file-list (add_list (car x)))
  (end_list)
  (if list-dialog-export-cogo-points
    (foreach x list-dialog-export-cogo-points
      (if (= (car x) "format-file")
	(set_tile "format-file" (itoa(analysis-number-item-in-list (cdr x) (mapcar 'cadr format-file-list))))
	(set_tile (car x) (cdr x))
	)
      )
    (set_tile "format-file" "0")
    )
  (action_tile "number" "(setq last-selekt \"number\"))")
  (action_tile "description" "(setq last-selekt \"description\"))")
  (action_tile "number-button"
    (vl-prin1-to-string
      '(progn
	(set_tile last-selekt (strcat(get_tile last-selekt)"[P]"))
	)))
  (action_tile "name-button"
    (vl-prin1-to-string
      '(progn
	(set_tile last-selekt (strcat(get_tile last-selekt)"[N]"))
	)))
  (action_tile "description-button"
    (vl-prin1-to-string
      '(progn
	(set_tile last-selekt (strcat(get_tile last-selekt)"[D]"))
	)))
  (action_tile "accept"
    (vl-prin1-to-string
      '(progn
	(setq result(list
		     (cons "number"(get_tile "number"))
		     (cons "description"(get_tile "description"))
		     (cons "format-file"(cadr(nth (atoi(get_tile "format-file"))format-file-list)))
		     ))
	(done_dialog 1)
	)))
  (start_dialog)
  (unload_dialog dcl_id)
  (if result(vlax-ldata-put "geo_tools-data-dialogs" "dialog-export-cogo-points" result))
  result
  )

(defun write-katalog-coor-in-file (katalog-list put-input-file / number-item input-file string)
  (setq number-item 1)
  (setq input-file (open put-input-file "w"))
  (cond
    ((or(=(vl-filename-extension put-input-file) ".txt")(=(vl-filename-extension put-input-file) ".lst"))
     (foreach point katalog-list
       (write-line (strcat
		     (if(=(cdr(assoc"number"point))"")(itoa number-item)(cdr(assoc"number"point)))
		     ","
		     (rtos (cdr(assoc "N" point)) 2 3)
		     ","
		     (rtos (cdr(assoc "E" point)) 2 3)
		     ","
		     (if (= -1.000E+20 (cdr(assoc "Z" point)))""(rtos (cdr(assoc "Z" point)) 2 3))
		     ","
		     (cdr(assoc "description" point))
		     ) input-file)
       (setq number-item (1+ number-item))
       ))
    ((=(vl-filename-extension put-input-file) ".sdr")
     (foreach point katalog-list
       (write-line (strcat
		     "08TP"
		     (edit_str_adding_probel (edit_str_trim_for_nunber_symbols
					       (edit_translit_str
						 (if(=(cdr(assoc"number"point))"")(itoa number-item)(cdr(assoc"number"point))))
					       15 nil) 16 T)
		     (edit_str_adding_probel (rtos (cdr(assoc "N" point)) 2 3) 16 nil)
		     (edit_str_adding_probel (rtos (cdr(assoc "E" point)) 2 3) 16 nil)
		     (edit_str_adding_probel (if (= -1.000E+20 (cdr(assoc "Z" point)))""(rtos (cdr(assoc "Z" point)) 2 3)) 16 nil)
		     (edit_str_adding_probel (edit_str_trim_for_nunber_symbols(edit_translit_str (cdr(assoc "description" point)))15 nil) 16 nil)
		     ) input-file)
       (setq number-item (1+ number-item))
       ))
    ((=(vl-filename-extension put-input-file) ".gsi")
     (foreach point katalog-list
       (if (setq string (convert-point-to-gsi-string
			  (list
			    (cons "number-item" number-item)
			    (if(=(cdr(assoc"number"point))"")(assoc"number"point)(cons "number" (itoa number-item)))
			    (assoc "N" point)
			    (assoc "E" point)
			    (if (= -1.000E+20 (cdr(assoc "Z" point)))0(assoc "Z" point))
			    (assoc "description" point))))
	 (write-line string input-file)
	 )
       (setq number-item (1+ number-item))
       )
     )
    )
  (close input-file)
  (princ (strcat "\nЭкспортировано " (itoa number-item) " точек."))
  )