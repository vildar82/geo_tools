(defun C:draw_number_in_text ( / ) ; програмка "Нумератор"
  (geo_tools-help "draw_number_in_text")
  (setq data-dailog(dialog-draw-number-in-text))
  (setq mode (get-item-by-name-in-list data-dailog "mode"))
  (cond
    ((= mode 0); создать
     (create-number-in-text data-dailog)
     )
    ((or (= mode 1)(= mode 2)); редактировать или заменить
     (edit-or-replase-number-in-text data-dailog)
     )
    ((= mode 3); полилиния
     (draw-number-by-poly data-dailog)
     )
    )
  (princ)
  )

(defun dialog-draw-number-in-text (  / data-dailog dcl_id)
  (if (not(setq data-dailog(vlax-ldata-get "geo_tools-data-dialogs" "dialog-draw-number-in-text")))
    (setq data-dailog
	   (list
	     (list "mode" 0)
	     (list "create-type" 0)
	     (list "edit-type" 1)
	     (list "mleader-mode" 0)
	     (list "h-text" 2.5)
	     (list "perfix" "")
	     (list "number-first" 1)
	     (list "suffix" "")
	     )
	  )
    )
  (setq dcl_id (geo-tools-load-dialog "draw_number_in_text"))
  (new_dialog "draw_number_in_text" dcl_id)
  (set-and-mode-tile-dialog-draw-number-in-text data-dailog)
  (action_tile "mode"
    (vl-prin1-to-string
      '(progn
	(setq data-dailog (put-item-by-name-in-list data-dailog "mode" (atoi $value)))
	(set-and-mode-tile-dialog-draw-number-in-text data-dailog)
	))
    )
  (action_tile "create-type"
    (vl-prin1-to-string
      '(progn
	(setq data-dailog (put-item-by-name-in-list data-dailog "create-type" (atoi $value)))
	(set-and-mode-tile-dialog-draw-number-in-text data-dailog)
	))
    )
  (action_tile "create-mode"
    (vl-prin1-to-string
      '(progn
	(if (=(get-item-by-name-in-list data-dailog "mode")1)
	 (setq data-dailog (put-item-by-name-in-list data-dailog "edit-type" (atoi $value)))
	 (setq data-dailog (put-item-by-name-in-list data-dailog "mleader-mode" (atoi $value)))
	 )
	(set-and-mode-tile-dialog-draw-number-in-text data-dailog)
	))
    )
  (action_tile "perfix"
    (vl-prin1-to-string
      '(progn
	(setq data-dailog (put-item-by-name-in-list data-dailog "perfix" $value))
	(set-and-mode-tile-dialog-draw-number-in-text data-dailog)
	))
    )
  (action_tile "number-first"
    (vl-prin1-to-string
      '(progn
	(setq data-dailog (put-item-by-name-in-list data-dailog "number-first" (atof $value)))
	(set-and-mode-tile-dialog-draw-number-in-text data-dailog)
	))
    )
  (action_tile "suffix"
    (vl-prin1-to-string
      '(progn
	(setq data-dailog (put-item-by-name-in-list data-dailog "suffix" $value))
	(set-and-mode-tile-dialog-draw-number-in-text data-dailog)
	))
    )
  (action_tile "h-text"
    (vl-prin1-to-string
      '(progn
	(setq data-dailog (put-item-by-name-in-list data-dailog "h-text" (atof $value)))
	(set-and-mode-tile-dialog-draw-number-in-text data-dailog)
	))
    )
  
  (if (/= (start_dialog) 0)
    (vlax-ldata-put "geo_tools-data-dialogs" "dialog-draw-number-in-text" data-dailog)
    )
  (unload_dialog dcl_id)
  data-dailog
  )

(defun set-and-mode-tile-dialog-draw-number-in-text (data-dailog /)
  (setq create-mode-mleader (list "Вторая точка Авто" "Вторая точка Вручную"))
  (setq create-mode-edit (list "Вставить в начало" "Вставить в конец"))
  (set_tile "mode" (itoa(get-item-by-name-in-list data-dailog "mode")))
  (set_tile "h-text" (rtos(get-item-by-name-in-list data-dailog "h-text")2 2))
  (set_tile "perfix" (get-item-by-name-in-list data-dailog "perfix"))
  (set_tile "number-first" (rtos(get-item-by-name-in-list data-dailog "number-first")2 0))
  (set_tile "suffix" (get-item-by-name-in-list data-dailog "suffix"))
  (set_tile "create-type" (itoa(get-item-by-name-in-list data-dailog "create-type")))
  (cond
    ((=(get-item-by-name-in-list data-dailog "mode")0); создать
     (mode_tile "create-type" 0)
     (mode_tile "create-mode" 0)
     (mode_tile "h-text" 0)
     (cond
       ((=(get-item-by-name-in-list data-dailog "create-type")2)
	(mode_tile "create-mode" 0)
	(start_list "create-mode")
	(foreach x create-mode-mleader(add_list x))
	(end_list)
	(set_tile "create-mode" (itoa(get-item-by-name-in-list data-dailog "mleader-mode")))
	)
       (T
	(mode_tile "create-mode" 1)
	)
       )
     )
    ((=(get-item-by-name-in-list data-dailog "mode")1); редактировать
     (mode_tile "create-type" 1)
     (mode_tile "create-mode" 0)
     (mode_tile "h-text" 1)
     (start_list "create-mode")
     (foreach x create-mode-edit(add_list x))
     (end_list)
     (set_tile "create-mode" (itoa(get-item-by-name-in-list data-dailog "edit-type")))
     )
    ((=(get-item-by-name-in-list data-dailog "mode")2); заменить
     (mode_tile "create-type" 1)
     (mode_tile "create-mode" 1)
     (mode_tile "h-text" 1)
     )
    ((=(get-item-by-name-in-list data-dailog "mode")3); полилиния
     (mode_tile "create-type" 0)
     (mode_tile "create-mode" 1)
     (mode_tile "h-text" 0)
     )
    )
  )

(defun create-number-in-text (data-dailog / t1 t2 string flag); создание объекта и помещение в его содержимое номера
  
  (while (progn(initget "Настроить")(setq t1(getpoint (if (= (get-item-by-name-in-list data-dailog "create-type") 2)"\nУкажи точку вставки[Настроить режим авто]: " "\nУкажи точку вставки: " ))))
    
    (setq string (strcat (get-item-by-name-in-list data-dailog "perfix")
			 (rtos (get-item-by-name-in-list data-dailog "number-first") 2 0)
			 (get-item-by-name-in-list data-dailog "suffix")))
    
    (cond
      ((= t1 "Настроить")
       (if(setq point1(getpoint "\nУкажи первую точку: "))
	 (if(setq point2(getpoint point1 "\nУкажи вторую точку: "))
	   (progn
	     (setq *delta-leader-in-draw_number_in_text*(mapcar '- point2 point1 ))
	     (setq flag nil)
	     )
	   )
	 )
       )
      ((= (get-item-by-name-in-list data-dailog "create-type") 0); Текст
       (draw-text-vla-by-list-property
	 (list
	   (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	   (cons "TextString" string)
	   (cons "InsertionPoint" t1)
	   (cons "Height" (/ (get-item-by-name-in-list data-dailog "h-text") (getvar "CANNOSCALEVALUE")))
	   (cons "Rotation" 0)
	   (cons "Alignment" 4)
	   ))
       (setq flag T)
       )
      ((= (get-item-by-name-in-list data-dailog "create-type") 1); Мтекст
       (draw-mtext-vla-by-list-property
	 (list
	   (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	   (cons "TextString" string)
	   (cons "InsertionPoint" t1)
	   (cons "Width" (/ (*(get-item-by-name-in-list data-dailog "h-text")5.0) (getvar "CANNOSCALEVALUE")))
	   (cons "Height" (/ (get-item-by-name-in-list data-dailog "h-text") (getvar "CANNOSCALEVALUE")))
	   (cons "Rotation" 0)
	   (cons "AttachmentPoint" 5)
	   )
	 )
       (setq flag T)
       )
      ((= (get-item-by-name-in-list data-dailog "create-type") 2); Мвыноска
       (if (not *delta-leader-in-draw_number_in_text*)
	 (setq *delta-leader-in-draw_number_in_text* (list(/ 20.0 (getvar "CANNOSCALEVALUE")) (/ 20.0 (getvar "CANNOSCALEVALUE")))))
       (if (=(get-item-by-name-in-list data-dailog "mleader-mode")1)
	 (if (not(setq t2 (getpoint t1 "\nУкажи точку вставки выноски<Авто>: ")))
	   (setq t2 (mapcar '+ t1 *delta-leader-in-draw_number_in_text*))
	   )
	 (setq t2 (mapcar '+ t1 *delta-leader-in-draw_number_in_text*))
	 )
       (create-mleader-style "Подписи")
       (draw-mleader-vla-by-list-property
	 (list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "LeaderLineCoor" (list t1 t2))
	      (cons "StyleName" "Подписи")
	      (cons "TextHeight" (/ (get-item-by-name-in-list data-dailog "h-text") (getvar "CANNOSCALEVALUE")))
	      (cons "TextString" string)
	      ))
       (setq flag T)
       )
      )
    (if flag
      (progn
	(setq data-dailog(put-item-by-name-in-list data-dailog "number-first" (1+(get-item-by-name-in-list data-dailog "number-first"))))
	(vlax-ldata-put "geo_tools-data-dialogs" "dialog-draw-number-in-text" data-dailog)
	)
      )
    )
  )

(defun edit-or-replase-number-in-text (data-dailog / string obj entget-obj); помещение номера в содержимое указанного объекта
  (while (setq obj(car(entsel "\nВыбери Текст, Мтекст или Мвыноску: ")))
    (setq string (strcat (get-item-by-name-in-list data-dailog "perfix")
			 (rtos (get-item-by-name-in-list data-dailog "number-first") 2 0)
			 (get-item-by-name-in-list data-dailog "suffix")))
    (setq entget-obj (entget obj))
    (cond
      ((or (eq (cdr(assoc 0 entget-obj)) "TEXT")(eq (cdr(assoc 0 entget-obj)) "MTEXT"))
       (entmod (subst (cons 1
			    (if (=(get-item-by-name-in-list data-dailog "mode")1)
			      (if(=(get-item-by-name-in-list data-dailog "edit-type")0)
				(strcat string(cdr(assoc 1 entget-obj)))
				(strcat (cdr(assoc 1 entget-obj))string)
				)
			      string
			      )
			    )
		      (assoc 1 entget-obj) entget-obj)
	       )
       )
      ((eq (cdr(assoc 0 entget-obj)) "MULTILEADER")
       (if (=(vla-get-ContentBlockName (vlax-ename->vla-object obj))"")
	 (entmod (subst (cons 304
			      (if (=(get-item-by-name-in-list data-dailog "mode")1)
				(if(=(get-item-by-name-in-list data-dailog "edit-type")0)
				  (strcat string(cdr(assoc 304 entget-obj)))
				  (strcat (cdr(assoc 304 entget-obj))string)
				  )
				string
				)
			      )
			(assoc 304 entget-obj) entget-obj)
		 )
	 (princ "\nВыбран объект не поддерживаемого типа.")
	 )
       )
      (t
       (princ "\nВыбран объект не поддерживаемого типа.")
       )
      )
    (setq data-dailog(put-item-by-name-in-list data-dailog "number-first" (1+(get-item-by-name-in-list data-dailog "number-first"))))
    (vlax-ldata-put "geo_tools-data-dialogs" "dialog-draw-number-in-text" data-dailog)
    )
  )

(defun draw-number-by-poly (data-dailog / h-text list-points obj string p1 p3 t2); нумерация вершин полилинии
  
  (if (setq obj(car(entsel "\nВыбери полилинию: ")))
    (progn
      (cond
	((= (cdr (assoc 0 (entget obj)))"LWPOLYLINE")
	 (setq list-points(list-coordinates-lwpolyline obj))
	 )
	((= (cdr (assoc 0 (entget obj)))"POLYLINE")
	 (setq list-points(list-coordinates-polyline obj)))
	)
      (setq h-text (/(get-item-by-name-in-list data-dailog "h-text")(getvar "CANNOSCALEVALUE")))
      
      (foreach x list-points
	(setq string (strcat (get-item-by-name-in-list data-dailog "perfix")
			     (rtos (get-item-by-name-in-list data-dailog "number-first") 2 0)
			     (get-item-by-name-in-list data-dailog "suffix")))
	(cond
	  ((= (get-item-by-name-in-list data-dailog "create-type") 0); Текст
	   (draw-text-vla-by-list-property
	     (list
	       (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	       (cons "TextString" string)
	       (cons "InsertionPoint" (insertion-point-from-draw-number-by-poly list-points x h-text 2.0))
	       (cons "Height" h-text)
	       (cons "Rotation" 0)
	       (cons "Alignment" 4)
	       ))
	   )
	  ((= (get-item-by-name-in-list data-dailog "create-type") 1); Мтекст
	   (draw-mtext-vla-by-list-property
	     (list
	       (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	       (cons "TextString" string)
	       (cons "InsertionPoint" (insertion-point-from-draw-number-by-poly list-points x h-text 1.5))
	       (cons "Width" (/ (*(get-item-by-name-in-list data-dailog "h-text")5.0) (getvar "CANNOSCALEVALUE")))
	       (cons "Height" h-text)
	       (cons "Rotation" 0)
	       (cons "AttachmentPoint" 5)
	       )
	     )
	   )
	  ((= (get-item-by-name-in-list data-dailog "create-type") 2); Мвыноска
	   (setq t2 (insertion-point-from-draw-number-by-poly list-points x h-text 5.0))
	   (create-mleader-style "Подписи")
	   (draw-mleader-vla-by-list-property
	     (list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
		  (cons "LeaderLineCoor" (list x t2))
		  (cons "StyleName" "Подписи")
		  (cons "TextHeight" h-text)
		  (cons "TextString" string)
		  ))
	   )
	  )
	(setq data-dailog(put-item-by-name-in-list data-dailog "number-first" (1+(get-item-by-name-in-list data-dailog "number-first"))))
	)
      (vlax-ldata-put "geo_tools-data-dialogs" "dialog-draw-number-in-text" data-dailog)
      )
    )
  )

(defun insertion-point-from-draw-number-by-poly (list-points x h-text length- /)
  (cond
    ((eq (vl-position x list-points) 0) ; первая точка в списке
     (polar x (angle (nth (1+(vl-position x list-points))list-points) x)(/ (* length- h-text) (getvar "CANNOSCALEVALUE")))
     )
    ((eq (vl-position x list-points) (1-(length list-points))); последняя точка в списке
     (polar x (angle (nth (1-(vl-position x list-points))list-points) x)(/ (* length- h-text) (getvar "CANNOSCALEVALUE")))
     )
    (T ; все осальные точки списка
     (setq p1(nth(1-(vl-position x list-points))list-points))
     (setq p3(nth(1+(vl-position x list-points))list-points))
     (cond
       ((> (calculation_angle_be_3points p1 x p3) (calculation_angle_be_3points p3 x p1))
	(polar x (+(angle x p3) (/(calculation_angle_be_3points p1 x p3)2.0)) (/ (* length- h-text) (getvar "CANNOSCALEVALUE"))))
       ((< (calculation_angle_be_3points p1 x p3) (calculation_angle_be_3points p3 x p1))
	(polar x (+(angle x p1) (/(calculation_angle_be_3points p3 x p1)2.0)) (/ (* length- h-text) (getvar "CANNOSCALEVALUE"))))
       ((= (calculation_angle_be_3points p1 x p3) (calculation_angle_be_3points p3 x p1))
	(polar x (+(angle x p3) (/ pi 2.0)) (/ (* length- h-text) (getvar "CANNOSCALEVALUE")))))
     )
    )
  )