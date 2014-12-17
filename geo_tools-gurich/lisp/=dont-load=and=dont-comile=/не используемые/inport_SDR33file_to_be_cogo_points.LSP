(defun C:inport_SDR33file_to_be_cogo_points ( / c3d_points filename file_points_list mode osn putsdr33file sdr33file str x insert-statist no-insert-statist)
  (load_global_variable)
  (geo_tools-help "inport_SDR33file_to_be_cogo_points")
  (if
    C3D_active_document
    (progn
      (terpri)
      (setq mode (dialog-import-sdr-mode))
      (setq insert-statist 0)
      (setq no-insert-statist 0)
      (if mode
	(progn
	  (load_global_variable)
	  (setq osn (getvar "OSMODE"))
	  (setvar "OSMODE" 0)
	  (setq putSDR33file (getfiled "Выбор файла SDR33" (getvar "DWGPREFIX") "txt;sdr" 0))
	  (setq FileName(vl-string-translate "," "."(substr putSDR33file (+(vl-string-position 92 putSDR33file 0 T)2) (-(strlen putSDR33file)(+(vl-string-position 92 putSDR33file 0 T)2)3))))
	  (setq SDR33file (open putSDR33file"r"))
	  (setq str (read-line SDR33file))
	  (while (not(or(eq str nil)(eq (substr str 1 4)"000")))
	    (if (or(eq (substr str 1 4) "08TP") (eq (substr str 1 4) "08KI") (eq (substr str 1 4) "02TP"))
	      (progn
		(setq file_points_list
		       (append file_points_list
			       (list (list
				       (vl-string-trim" "(substr str 5 16)); номер
				       (atof(vl-string-trim" "(substr str 21 16))); X
				       (atof(vl-string-trim" "(substr str 37 16))); Y
				       (atof(vl-string-trim" "(substr str 53 16))); H
				       (vl-string-trim" "(substr str 69 16)); описание
				       ))))))
	    (setq str (read-line SDR33file))
	    )
	  (close SDR33file)
	  (if file_points_list
	    (progn
	      (setvar "nomutt" 1)
	      (setvar "cmdecho" 0)
	      (if
		(eq(tblsearch "LAYER" FileName)nil)
		(command "_-LAYER" "с" FileName "")
		(command "_-LAYER" "_S" FileName "")
		)
	      (setvar "cmdecho" 1)
	      (setvar "nomutt" 0)
	      (mapcar '(lambda (x)
			 (cond
			   ((eq mode 0)
			    (if (not (check-point-to-drawning (list (nth 1 x)(nth 2 x)(nth 3 x)(nth 0 x))))
			      (progn
				(vlax-put-property(vla-add C3D_points (vlax-3d-point (nth 2 x)(nth 1 x)(nth 3 x)))'Description (nth 0 x))
				(setq insert-statist (1+ insert-statist))
				)
			      (setq no-insert-statist (1+ no-insert-statist))
			      )
			    )
			   ((eq mode 1)
			    (if (not (check-point-to-drawning (list (nth 1 x)(nth 2 x)(nth 3 x)(nth 4 x))))
			      (progn
				(vlax-put-property(vla-add C3D_points (vlax-3d-point (nth 2 x)(nth 1 x)(nth 3 x)))'Description (nth 4 x))
				(setq insert-statist (1+ insert-statist))
				)
			      (setq no-insert-statist (1+ no-insert-statist))
			      )
			    )
			   ((eq mode 2)
			    (if (not (check-point-to-drawning (list (nth 1 x)(nth 2 x)(nth 3 x)(strcat (nth 0 x)"/"(nth 4 x)))))
			      (progn
				(vlax-put-property(vla-add C3D_points (vlax-3d-point (nth 2 x)(nth 1 x)(nth 3 x)))'Description (strcat (nth 0 x)"/"(nth 4 x)))
				(setq insert-statist (1+ insert-statist))
				)
			      (setq no-insert-statist (1+ no-insert-statist))
			      )
			    )
			   )
			 )
		      file_points_list)
	      (princ "\nВставлено ")
	      (princ insert-statist)
	      (princ " точек")
	      (princ "\nПропущено ")
	      (princ no-insert-statist)
	      (princ " точек")
	      )
	    (alert "В выбраном файле координат не обнаружено!")
	    )
	  (setvar "OSMODE" osn)
	  )
	)
      )
    (alert "Выполнение комманды предполагает ее запуск в AutoCAD Civil 3D!\nТекущий AutoCAD им не является или представляет собой не поддерживаемую(временно?) версию.")
    )
  (princ)
  )

(defun check-point-to-drawning (point / flag nn obj_vba points-corrent-drawning points_list ss x)
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
;;;					(vlax-get-property  obj_vba "FullDescription")
					))))
		 )
	      points-corrent-drawning)
      (setq nn 0)
      (while (not(eq nn (length points_list)))
	(bump)
	(if (equal (list (nth 0 point)(nth 1 point)(nth 2 point)(nth 3 point))
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



(defun dialog-import-sdr-mode (/ dcl_id mode)
  (setq dcl_id (load_dialog "Диалоговые окна.dcl"))
  (setq mode (vlax-ldata-get "geo_tools-data-dialogs" "dialog-import-sdr-mode"))
  (if (not mode)(setq mode 0))
  (new_dialog "d_import_sdr_mode" dcl_id)
  (set_tile "mode" (itoa mode))
  (start_list "mode")
  (mapcar 'add_list (list "Номер точки => Описание" "Описание => Описание" "Номер точки/Описание => Описание"))
  (end_list)
  (action_tile "mode" "(setq mode (atoi $value)))")
  (if(eq(start_dialog)0)(setq mode nil))
  (unload_dialog dcl_id)
  (if mode(vlax-ldata-put "geo_tools-data-dialogs" "dialog-import-sdr-mode" mode))
  mode
  )