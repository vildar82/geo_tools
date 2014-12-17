(defun C:inport_file_to_be_cogo_points( / filename file_points_list flag genesis insert-statist list-dialog no-insert-statist point-obj put-file)
  ;Импортирует точки из файла, форматов Nikon PNEZD,CDR33,GSI
  (load_global_variable)
  (geo_tools-help "inport_file_to_be_cogo_points")
  (if
    C3D_active_document
    (progn
      (terpri)
      (setq list-dialog (dialog-import-cogo-points))
      (setq insert-statist 0)
      (setq no-insert-statist 0)
      (if list-dialog
	(progn
	  (cond
	    ((= (get-item-by-name-in-list list-dialog "format") "0")
	      (if(setq put-file (getfiled "Выбор файла координат" (getvar "DWGPREFIX")  "txt;raw;*" 0))
	      (setq file_points_list(get-NikonRAW-katalog-coor put-file)))
	      )
	    ((= (get-item-by-name-in-list list-dialog "format") "1")
	      (if(setq put-file (getfiled "Выбор файла координат" (getvar "DWGPREFIX")  "txt;*" 0))
	      (setq file_points_list(get-NikonPENZD-katalog-coor put-file)))
	      )
	    ((= (get-item-by-name-in-list list-dialog "format") "2")
	      (if(setq put-file (getfiled "Выбор файла координат" (getvar "DWGPREFIX")  "txt;sdr;*" 0))
	      (setq file_points_list(get-SDR33-katalog-coor put-file)))
	      )
	    ((= (get-item-by-name-in-list list-dialog "format") "3")
	      (if(setq put-file (getfiled "Выбор файла координат" (getvar "DWGPREFIX")  "gsi;*" 0))
	      (setq file_points_list(get-GSI-katalog-coor put-file)))
	      )
	    )
	  (if put-file
	    (if file_points_list
	      (progn
		(vla-StartUndoMark active_document)
		(setq FileName(vl-string-translate "," "."(vl-filename-base put-file)))
		(foreach x file_points_list
;;;		  (terpri)
;;;		  (princ x)
		  (if(not(setq genesis(get-item-by-name-in-list x "genesis")))(setq genesis "unknown"))
		  (if
		    (if
		      (or
			(and (= genesis "upload")(= (get-item-by-name-in-list list-dialog "format") "0")(= (get-item-by-name-in-list list-dialog "upload") "1"))
			(and (= genesis "input")(= (get-item-by-name-in-list list-dialog "format") "0")(= (get-item-by-name-in-list list-dialog "input") "1"))
			(and (= genesis "calculation")(= (get-item-by-name-in-list list-dialog "format") "0")(= (get-item-by-name-in-list list-dialog "calculation") "1"))
			(and (= genesis "unknown")(= (get-item-by-name-in-list list-dialog "format") "0")(= (get-item-by-name-in-list list-dialog "unknown") "1"))
			(and (/= (get-item-by-name-in-list list-dialog "format") "0")(= genesis "unknown"))
			)
		      (if
			(not (check-point-to-drawning (list (get-item-by-name-in-list list-dialog "N")(get-item-by-name-in-list list-dialog "E")(get-item-by-name-in-list list-dialog "Z"))))
			(progn
			  (if (and (= (get-item-by-name-in-list list-dialog "format") "0")(= (get-item-by-name-in-list list-dialog "layer-name") "1"))
			    (create-or-sel-layer (get-item-by-name-in-list x "data"))
			    (create-or-sel-layer FileName)
			    )
			  (setq point-obj (vla-add C3D_points
						   (vlax-3d-point
						     (get-item-by-name-in-list x "E")
						     (get-item-by-name-in-list x "N")
						     (get-item-by-name-in-list x "Z"))))
			  (setq flag (if (/= (get-item-by-name-in-list x "code")"")T nil))
			  (vlax-put-property point-obj 'Description (strcat
								      (get-item-by-name-in-list x "code")
								      (if flag "[" "")
								      (get-item-by-name-in-list x "name")
								      (if flag "]" "")
								      ))
			  (foreach y (list
				       "name"
				       "genesis"
				       "h-target"
				       ;"temp"
				       ;"press"
				       "prism"
				       "data"
				       "time"
				       "code")
			    (if (get-item-by-name-in-list x y)
			      (vlax-ldata-put point-obj y (get-item-by-name-in-list x y)))
			    )
			  T
			  )
			nil
			)
		      )
		    (setq insert-statist (1+ insert-statist))
		    (setq no-insert-statist (1+ no-insert-statist))
		    )
		  (vla-EndUndoMark active_document)
		  )
		(princ "\nВставлено ")(princ insert-statist)(princ " точек")
		(princ "\nПропущено ")(princ no-insert-statist)(princ " точек")
		)
	      (alert "В выбраном файле координат не обнаружено!")
	      ))
	  )
	)
      )
    (alert "Выполнение комманды предполагает ее запуск в AutoCAD Civil 3D!\nТекущий AutoCAD им не является или представляет собой не поддерживаемую(временно?) версию.")
    )
  (princ)
  )