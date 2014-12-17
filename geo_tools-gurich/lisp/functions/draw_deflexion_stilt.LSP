(defun C:draw_deflexion_stilt (/ t1 t2 t3 object dx dy z text_x text_y text_z num_svai_xy
			       z_xy dx_xy dy_xy ar_x_xy ar_y_xy put dxf41 dxf42 dxf43 dxf2 dxf50 z_real os
			       ar_x ar_y chek_num chek_xy chek_z dcl_id ei_xy ei_z h num num_svai sel_z z_pr z_pr_)
  (geo_tools-help "draw_deflexion_stilt")
  (if (eq num_svai nil)(setq num_svai 1))
  (setq chek_num 1)
  (setq chek_xy 1)
  (setq chek_z 1)
  (setq ei_xy 0)
  (setq ei_z 0)
  (setq sel_z 0)
  (setq dcl_id (geo-tools-load-dialog "d_draw_deflexion_stilt"))
  (setq text-style (getvar "TEXTSTYLE"))
  (new_dialog "d_draw_deflexion_stilt" dcl_id)
  (set_tile "num_1" (rtos num_svai 2 0))
  (action_tile "num_1" "(setq num (get_tile \"num_1\"))")
  (action_tile "chek_num"
    "(if
    (eq chek_num 1)
    (progn
    (mode_tile \"num_1\" 1)
    (setq chek_num 0)
    )
    (progn
    (mode_tile \"num_1\" 0)
    (setq chek_num 1)
    )
    )"
    )
  (action_tile "chek_xy"
    "(if
    (eq chek_xy 1)
    (progn
    (mode_tile \"ei_xy\" 1)
    (setq chek_xy 0)
    )
    (progn
    (mode_tile \"ei_xy\" 0)
    (setq chek_xy 1)
    )
    )"
    )
  (action_tile "chek_z"
    "(if
    (eq chek_z 1)
    (progn
    (mode_tile \"ei_z\" 1)
    (mode_tile \"sel_z\" 1)
    (setq chek_z 0)
    )
    (progn
    (mode_tile \"ei_z\" 0)
    (mode_tile \"sel_z\" 0)
    (setq chek_z 1)
    )
    )"
    )
  (action_tile "ei_xy"
    "(cond
    ((= $value \"0\")
    (setq ei_xy 0)
    )
   ((= $value \"1\")
    (setq ei_xy 1)
    )
    ((= $value \"2\")
    (setq ei_xy 2)
    )
    )"
    )
  (action_tile "ei_z"
    "(cond
    ((= $value \"0\")
    (setq ei_z 0)
    )
   ((= $value \"1\")
    (setq ei_z 1)
    )
    ((= $value \"2\")
    (setq ei_z 2)
    )
    )"
    )
  (action_tile "sel_z"
    "(cond
    ((= $value \"0\")
    (setq sel_z 0)
    )
   ((= $value \"1\")
    (setq sel_z 1)
    )
    )"
    )
  (if (eq (start_dialog) 0) (progn (unload_dialog dcl_id)(exit))(unload_dialog dcl_id))
  (if
    (not (or (eq (type num_svai) 'INT)(eq (type num_svai) 'REAL)))
    (setq num_svai (atof num_svai))
    )
  (initget)(setq os (getangle "\n”кажи главную ось<Enter - горизонтально>: "))
  (if(eq os nil)(setq os (* pi 1.5)))
  (if (eq chek_z 1)
    (progn
      (initget 1)(setq z_pr (getreal "\n¬веди проектную отметку: "))
      )
    )
  (initget)(setq t1(getpoint "\n”кажи проектное положение: "))
  (while (not (eq t1 nil))
    (initget)(setq t2(getpoint "\n”кажи измеренное расположение: "))
    (if (eq sel_z 0)
      (setq z (nth 2 t2))
      (progn
	(initget 1)(setq z (getreal "\n¬веди измеренную отметку: "))
	)
      )
    (initget)(setq t3(getpoint "\n”кажи куда вставить текст: "))
    (setq t1(list (nth 0 t1)(nth 1 t1) 0)
	  t2 (list (nth 0 t2)(nth 1 t2) 0)
	  )
;;;    (setq dx (- (nth 0 t2) (nth 0 t1)))
;;;    (setq dy (- (nth 1 t2) (nth 1 t1)))
;;;    (setq dx (*(distance t1 t2) (cos (- (angle t1 t2)os))))
;;;    (setq dy (*(distance t1 t2) (sin (- (angle t1 t2)os))))
    (setq dx (*(distance t1 t2) (cos ( - pi (- (- (angle t1 t2)os)(/ pi 2))))))
    (setq dy (*(distance t1 t2) (sin ( - pi (- (- (angle t1 t2)os)(/ pi 2))))))
    
    (setq text_x (cond
		   ((eq ei_xy 0)
		    (rtos (*  (abs dx) 1000) 2 0))
		   ((eq ei_xy 1)
		    (rtos (*  (abs dx) 100) 2 1))
		   ((eq ei_xy 2)
		    (rtos (*  (abs dx) 1) 2 3))
		   )
	  )
    (setq text_y (cond
		   ((eq ei_xy 0)
		    (rtos (*  (abs dy) 1000) 2 0))
		   ((eq ei_xy 1)
		    (rtos (*  (abs dy) 100) 2 1))
		   ((eq ei_xy 2)
		    (rtos (*  (abs dy) 1) 2 3))
		   )
	  )
    (if (= chek_z 1)
      (progn
	(setq h (- z z_pr))
	(if (not (minusp h))
	  (setq text_z (strcat "+"
			       (cond
				 ((eq ei_xy 0)
				  (rtos (*  h 1000) 2 0))
				 ((eq ei_xy 1)
				  (rtos (*  h 100) 2 1))
				 ((eq ei_xy 2)
				  (rtos (*  h 1) 2 3))
				 )
			       )
		)
	  (setq text_z (cond
			 ((eq ei_xy 0)
			  (rtos (*  h 1000) 2 0))
			 ((eq ei_xy 1)
			  (rtos (*  h 100) 2 1))
			 ((eq ei_xy 2)
			  (rtos (*  h 1) 2 3))
			 )
		)
	  )
	))
    (cond
      ((not (minusp dx))(setq ar_x "стрелка в лево"))
      ((minusp dx) (setq ar_x "стрелка в право"))
      )
    (cond
      ((minusp dy) (setq ar_y "стрелка в низ"))
      ((not (minusp dy)) (setq ar_y "стрелка в верх"))
      )
    
    (setq num_svai_xy (polar t3 (+ os pi) (/ (/ 7.5 (format_scale_kof)) 1)))
    (setq z_xy t3)
    (setq dx_xy (polar (polar t3 (+ os pi) (/ (/ 2.5 (if(= (getvar "CTAB")"Model")(getvar "CANNOSCALEVALUE")1)) 1)) (+ os (/ pi 2)) (/ (/ 3 (format_scale_kof)) 1)))
    (setq dy_xy (polar (polar t3 (+ os pi) (/ (/ 5 (if(= (getvar "CTAB")"Model")(getvar "CANNOSCALEVALUE")1)) 1)) (+ os (/ pi 2)) (/ (/ 3 (format_scale_kof)) 1)))
    (setq ar_x_xy (polar t3 (+ os pi) (/ (/ 2.5 (format_scale_kof)) 1)))
    (setq ar_y_xy (polar t3 (+ os pi) (/ (/ 5 (format_scale_kof)) 1)))
    (if
      (not (tblobjname "block" ar_x))
      (progn
	(command "_insert" ar_x "0,0,0" "1" "1" "0")
	(entdel (entlast))
	)
      )
    (if
      (not (tblobjname "block" ar_y))
      (progn
	(command "_insert" ar_y "0,0,0" "1" "1" "0")
	(entdel (entlast))
	)
      )
    (setq dxf41 (vl-list* 41 (/ 1 (getvar "CANNOSCALEVALUE"))))
    (setq dxf42 (vl-list* 42 (/ 1 (getvar "CANNOSCALEVALUE"))))
    (setq dxf43 (vl-list* 43 (/ 1 (getvar "CANNOSCALEVALUE"))))
    (setq dxf2 (vl-list* 2 ar_x))
    (setq dxf50 (vl-list* 50 (+ os (/ pi 2))))
    (if (= chek_xy 1)
      (progn
	(if
	  (not (eq text_x "0"))
	  (entmake (list '(0 . "INSERT") dxf2 (append '(10) ar_x_xy) dxf41 dxf42 dxf43 dxf50))
	  )
	(setq dxf2 (vl-list* 2 ar_y))
	(if
	  (not (eq text_y "0"))
	  (entmake (list '(0 . "INSERT") dxf2 (append '(10) ar_y_xy) dxf41 dxf42 dxf43 dxf50))
	  )
	))
    (if (= chek_num 1)
      (draw-text-vla-by-list-property
	     (list
	       (cons "spece" model_spece)
	       (cons "TextString" (strcat "є" (rtos num_svai 2 0)))
	       (cons "InsertionPoint" num_svai_xy)
	       (cons "Height" (/ (/ 2 (format_scale_kof)) 1))
	       (cons "Rotation" (+ os (/ pi 2)))
	       (cons "Alignment" 0)
	       (cons "StyleName" text-style)
	       ))
      )
    (if (eq chek_xy 1)
      (progn
	(draw-text-vla-by-list-property
	     (list
	       (cons "spece" model_spece)
	       (cons "TextString" text_x)
	       (cons "InsertionPoint" dx_xy)
	       (cons "Height" (/ (/ 2 (format_scale_kof)) 1))
	       (cons "Rotation" (+ os (/ pi 2)))
	       (cons "Alignment" 0)
	       (cons "StyleName" text-style)
	       ))
	(draw-text-vla-by-list-property
	     (list
	       (cons "spece" model_spece)
	       (cons "TextString" text_y)
	       (cons "InsertionPoint" dy_xy)
	       (cons "Height" (/ (/ 2 (format_scale_kof)) 1))
	       (cons "Rotation" (+ os (/ pi 2)))
	       (cons "Alignment" 0)
	       (cons "StyleName" text-style)
	       ))
	)
      )
    (if (= chek_z 1)
      (draw-text-vla-by-list-property
	     (list
	       (cons "spece" model_spece)
	       (cons "TextString" text_z)
	       (cons "InsertionPoint" z_xy)
	       (cons "Height" (/ (/ 2 (format_scale_kof)) 1))
	       (cons "Rotation" (+ os (/ pi 2)))
	       (cons "Alignment" 0)
	       (cons "StyleName" text-style)
	       ))
      )
    (setq z_pr_ z_pr)
    (if
      (not(setq z_pr
	     (if (eq chek_z 1)
	       (progn
		 (initget)
		 (getreal (strcat "\n¬веди проектную отметку <" (rtos z_pr 2 3) ">: ")
			  )
		 )
	       )
	    )
	  )
      (setq z_pr z_pr_)
      )
    (initget)(setq t1(getpoint "\n”кажи проектное положение: "))
    (setq num_svai (1+ num_svai))
    )
  (princ)
  )
