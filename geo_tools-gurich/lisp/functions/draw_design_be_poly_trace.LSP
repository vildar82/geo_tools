(defun C:draw_design_be_poly_trace ; оформляет как трассу выбраную полилинию
       (/ 2dpline 2dpline_vla data-dialog g ht p1 p2 p3 spisokv2d styl dec angle-string flag-3D current-picket dist-tressa flag picket-first)
  (geo_tools-help "draw_design_be_poly_trace")
  (load_global_variable)
  (initget)(setq 2Dpline (car (entsel "\nВыберите 2D полилинию оси трассы: ")))
  (cond
    ((eq 2Dpline nil)
     (alert "Ничего не выбрано!"))
    ((and (not(eq (cdr(assoc 0 (entget 2Dpline))) "LWPOLYLINE"))(not(eq (cdr(assoc 0 (entget 2Dpline))) "POLYLINE")))
     (alert "Выбрана не полилиния!"))
    (T
     (if (eq (cdr(assoc 0 (entget 2Dpline))) "POLYLINE")
       (progn
	 (setq flag-3D T)
	 (setq 2Dpline_vla(draw_2dPoly_be_3dpoly 2Dpline))
	 (setq 2Dpline (vlax-vla-object->ename 2Dpline_vla))
	 )
       (setq 2Dpline_vla (vlax-ename->vla-object 2Dpline))
       )
     (setq spisokv2D (extract_coord_lwpolyline (entget 2Dpline)))
     (setq ht (/ 2.5 (getvar "CANNOSCALEVALUE")))
     (setq styl (getvar "TEXTSTYLE"))
     (setq data-dialog (dialog-draw_design_be_2dpoly_trace 2Dpline flag-3D))
     (vla-StartUndoMark active_document)
     (if (eq (cadr (assoc "draw-mleader" data-dialog)) 1)
       (progn
	 (setq p1 (car spisokv2D))
	 (if (=(get-item-by-name-in-list data-dialog "draw-start-end-picket-angle")1)
	   (draw-mleader-be-design-be-2dpoly-trace p1 "Начало трассы\\PПК" data-dialog 2Dpline_vla))
	 (repeat (- (length spisokv2D) 2)
	   (setq p1 (car spisokv2D))
	   (setq p2 (cadr spisokv2D))
	   (setq p3 (caddr spisokv2D))
	   (setq spisokv2D (cdr spisokv2D))
	   (setq g (calculation_angle_be_3points p1 p2 p3))
	   (setq dec (abs(convert-rad-grad (- g pi))))
	   (setq angle-string (convert-dec-to-gms-str dec (get-item-by-name-in-list data-dialog "accuracy-angle")))
	   (if (not(< dec (get-item-by-name-in-list data-dialog "min-angle")))
	   (draw-mleader-be-design-be-2dpoly-trace p2 (strcat (get-item-by-name-in-list data-dialog "perfix-angle") angle-string "\\PПК")data-dialog 2Dpline_vla)
	     )
	   )
	 (if (=(get-item-by-name-in-list data-dialog "draw-start-end-picket-angle")1)
	   (draw-mleader-be-design-be-2dpoly-trace (cadr spisokv2D) "Конец трассы\\PПК" data-dialog 2Dpline_vla))
	 )
       )
     (if (eq (get-item-by-name-in-list data-dialog "draw-picket") 1)
       (progn
	 (setq picket-first (get-item-by-name-in-list data-dialog "picket-first"))
	 (setq dist-tressa (vla-get-Length 2Dpline_vla))
	 (setq current-picket(+(*(fix(/ picket-first 100.00))100.00)100.00))
	 (setq flag (if (>(- current-picket picket-first)dist-tressa)T))
	 (while (not flag)
	   (draw-design-be-poly-trace-draw-picket picket-first current-picket 2Dpline_vla  ht styl nil nil)
	   (setq current-picket(+ current-picket 100.00))
	   (setq flag (if (>(- current-picket picket-first)dist-tressa)T))
	   )
	 (if (=(get-item-by-name-in-list data-dialog "draw-start-end-picket")1)
	   (progn
	     (draw-design-be-poly-trace-draw-picket picket-first picket-first 2Dpline_vla  ht styl T (get-item-by-name-in-list data-dialog "accuracy-picket"))
	     (draw-design-be-poly-trace-draw-picket picket-first (+ picket-first (vla-get-Length 2Dpline_vla)) 2Dpline_vla  ht styl nil (get-item-by-name-in-list data-dialog "accuracy-picket"))
	     ))
	 )
       )
     (if (and flag-3D (=(get-item-by-name-in-list data-dialog "del-bak-2poly") 1))(vla-delete 2Dpline_vla))
     (vla-put-ConstantWidth 2Dpline_vla (/ (get-item-by-name-in-list data-dialog "width-poly") (getvar "CANNOSCALEVALUE")))
     (vla-EndUndoMark active_document)
     )
    )
  (princ)
  )

(defun draw-design-be-poly-trace-draw-picket (picket-first current-picket 2Dpline_vla ht styl flag-first-piket accuracy-picket / point point-prev)
  (setq point (vlax-curve-getPointAtDist 2Dpline_vla (- current-picket picket-first)))
  (if flag-first-piket
    (setq point-prev (vlax-curve-getPointAtParam 2Dpline_vla(float(1+(fix(vlax-curve-getParamAtPoint 2Dpline_vla point))))))
    (if accuracy-picket
      (setq point-prev (vlax-curve-getPointAtParam 2Dpline_vla(float(1-(fix(vlax-curve-getParamAtPoint 2Dpline_vla point))))))
      (setq point-prev (vlax-curve-getPointAtParam 2Dpline_vla(float(fix(vlax-curve-getParamAtPoint 2Dpline_vla point))))); точка в ближайщей задней вершине полилинии
      )
    )
  
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
		 '(90 . 2)
		 (cons 43 (/ 0.15 (getvar "CANNOSCALEVALUE")))
		 (cons 10 (polar point (+ (angle point-prev point) (/ pi 2))(/ 1.25 (getvar "CANNOSCALEVALUE"))))
		 (cons 10 (polar point (- (angle point-prev point) (/ pi 2))(/ 1.25 (getvar "CANNOSCALEVALUE"))))
		 ))
  (draw-text-vla-by-list-property
    (list
      (cons "spece" model_spece)
      (cons "TextString" (strcat "ПК" (if (or accuracy-picket flag-first-piket)
					(convert-distance-to-pk-string current-picket accuracy-picket T)
					(rtos (/ current-picket 100.0)2 0))
				 ))
      (cons "InsertionPoint" (polar point (+ (analysis_angle_be_long_distance(angle point-prev point)) (/ pi 2))(* ht 1.3)))
      (cons "Height" ht)
      (cons "Rotation" (analysis_angle_be_long_distance(angle point-prev point)))
      (cons "Alignment" 4)
      (cons "StyleName" styl)
      ))
  )

(defun dialog-draw_design_be_2dpoly_trace(2Dpline flag-3D / list-dialog dcl_id)
(setq list-dialog(get-list-dialog-design-trace))
  (setq dcl_id (geo-tools-load-dialog "d_trassa"))
  (new_dialog "d_trassa" dcl_id)
  (mode-tile-in-dialog-draw-design-be-2dpoly-trace list-dialog)
  (foreach x (cdr list-dialog)
    (set_tile (car x)
	      (cond
		((="picket-first"(car x))
		 (convert-distance-to-pk-string (cadr x) (cadr (assoc "accuracy-picket" list-dialog)) nil))
		((="min-angle"(car x))
		 (convert-dec-to-gms-str (cadr x) (cadr (assoc "accuracy-angle" list-dialog))))
		((="numbers-coor"(car x))
		 (if(>(cadr x)0)
		   (itoa (-(cadr x)2))
		   (itoa (cadr x))))
		((="perfix-angle"(car x))
		 (cadr x)
		 )
		((or(="version"(car x))(="width-poly"(car x)))
		 (rtos(cadr x)2 2)
		 )
		(T
		 (itoa (cadr x)))
		)
	      )
    )
  (if flag-3D
    (mode_tile "del-bak-2poly" 0)
    (mode_tile "del-bak-2poly" 1))
  (action_tile "draw-picket"
    (vl-prin1-to-string
      '(progn
	(setq list-dialog(subst (list "draw-picket" (atoi $value))(assoc "draw-picket" list-dialog) list-dialog))
	(mode-tile-in-dialog-draw-design-be-2dpoly-trace list-dialog)
	)))
  (action_tile "accuracy-picket"(vl-prin1-to-string'(setq list-dialog(subst (list "accuracy-picket" (atoi $value))(assoc "accuracy-picket" list-dialog) list-dialog))))
  (action_tile "draw-start-end-picket"(vl-prin1-to-string'(setq list-dialog(subst (list "draw-start-end-picket" (atoi $value))(assoc "draw-start-end-picket" list-dialog) list-dialog))))
  
  
  (action_tile "draw-mleader"
    (vl-prin1-to-string
      '(progn
	(setq list-dialog(subst (list "draw-mleader" (atoi $value))(assoc "draw-mleader" list-dialog) list-dialog))
	(mode-tile-in-dialog-draw-design-be-2dpoly-trace list-dialog)
	)))
  (action_tile "accuracy-angle"
    (vl-prin1-to-string
      '(progn
	(setq list-dialog(subst (list "accuracy-angle" (atoi $value))(assoc "accuracy-angle" list-dialog) list-dialog))
	 (set_tile "min-angle"(convert-dec-to-gms-str (cadr (assoc "min-angle" list-dialog)) (cadr (assoc "accuracy-angle" list-dialog))))
	)))
  (action_tile "perfix-angle"(vl-prin1-to-string'(setq list-dialog(subst (list "perfix-angle" $value)(assoc "perfix-angle" list-dialog) list-dialog))))
  (action_tile "accuracy-picket-angle"(vl-prin1-to-string'(setq list-dialog(subst (list "accuracy-picket-angle" (atoi $value))(assoc "accuracy-picket-angle" list-dialog) list-dialog))))
  (action_tile "draw-start-end-picket-angle"(vl-prin1-to-string'(setq list-dialog(subst (list "draw-start-end-picket-angle" (atoi $value))(assoc "draw-start-end-picket-angle" list-dialog) list-dialog))))
  (action_tile "min-angle"(vl-prin1-to-string'(setq list-dialog(subst (list "min-angle" (convert-str-gms-to-dec $value))(assoc "min-angle" list-dialog) list-dialog))))
  
  (action_tile "draw-coor"
    (vl-prin1-to-string
      '(progn
	(setq list-dialog(subst (list "draw-coor" (atoi $value))(assoc "draw-coor" list-dialog) list-dialog))
	(mode-tile-in-dialog-draw-design-be-2dpoly-trace list-dialog)
	)))
  (action_tile "numbers-coor"; выбор количества символов до запятой
    (vl-prin1-to-string
      '(if
	(> (atoi $value) 0)
	(setq list-dialog(subst (list "numbers-coor" (+ (atoi $value)2))(assoc "numbers-coor" list-dialog) list-dialog))
	(setq list-dialog(subst (list "numbers-coor" 0)(assoc "numbers-coor" list-dialog) list-dialog))
	)
      )
    )
  (action_tile "accuracy-coor"(vl-prin1-to-string'(setq list-dialog(subst (list "accuracy-coor" (atoi $value))(assoc "accuracy-coor" list-dialog) list-dialog))))
  
  (action_tile "picket-first"(vl-prin1-to-string'(setq list-dialog(subst (list "picket-first" (convert-pk-string-to-distance $value))(assoc "picket-first" list-dialog) list-dialog))))
  (action_tile "width-poly"(vl-prin1-to-string'(setq list-dialog(subst (list "width-poly" (atof $value))(assoc "width-poly" list-dialog) list-dialog))))
  (action_tile "del-bak-2poly"(vl-prin1-to-string'(setq list-dialog(subst (list "del-bak-2poly" (atoi $value))(assoc "del-bak-2poly" list-dialog) list-dialog))))
  
  (if(eq(start_dialog)0)
    (setq list-dialog nil)
    (vlax-ldata-put "geo_tools_dictionary" "dialog-design-trace" list-dialog)
    )
  (unload_dialog dcl_id)
  list-dialog
  )

(defun mode-tile-in-dialog-draw-design-be-2dpoly-trace (list-dialog / )
  (if (=(cadr (assoc "draw-picket" list-dialog))0)
    (foreach x (list "accuracy-picket" "draw-start-end-picket" )(mode_tile x 1))
    (foreach x (list "accuracy-picket" "draw-start-end-picket" )(mode_tile x 0))
    )
  (if
    (=(cadr (assoc "draw-mleader" list-dialog))0)
    (foreach x (list "accuracy-angle" "perfix-angle" "accuracy-picket-angle" "draw-start-end-picket-angle" "min-angle"
		     "draw-coor" "numbers-coor" "accuracy-coor")(mode_tile x 1))
    (progn
      (foreach x (list "accuracy-angle" "perfix-angle" "accuracy-picket-angle" "draw-start-end-picket-angle" "min-angle"
		     "draw-coor" "numbers-coor" "accuracy-coor")(mode_tile x 0))
      (if
	(=(cadr (assoc "draw-coor" list-dialog))0)
	(foreach x (list "numbers-coor" "accuracy-coor" )(mode_tile x 1))
	(foreach x (list "numbers-coor" "accuracy-coor" )(mode_tile x 0))
	)
      )
    )
  )

(defun draw-mleader-be-design-be-2dpoly-trace ( point string data-dialog 2Dpline_vla / ht styl)
  (setq ht (/ 2.5 (getvar "CANNOSCALEVALUE")))
  (setq styl (getvar "TEXTSTYLE"))
  (draw-mleader-vla-by-list-property
    (list(cons "StyleName" "Подписи")
	 (cons "TextStyleName" styl)
	 (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	 (cons "LeaderLineCoor" (list point(mapcar '+ point (list(/ -10 (getvar "CANNOSCALEVALUE")) (/ 10 (getvar "CANNOSCALEVALUE"))))))
	 
	 (cons "TextHeight" ht)
	 (cons "TextString" (strcat string
					    (convert-distance-to-pk-string
					      (+(vlax-curve-getDistAtPoint 2Dpline_vla point)(cadr(assoc"picket-first"data-dialog)))
					      (cadr(assoc"accuracy-picket-angle"data-dialog))T)
					    (if (eq(cadr(assoc"draw-coor"data-dialog)) 1)
					      (if (eq(cadr(assoc"numbers-coor"data-dialog))0)
						(strcat "\\PX=" (rtos(nth 1 point)2(cadr(assoc"accuracy-coor"data-dialog)))
							"\\PY="(rtos(nth 0 point)2(cadr(assoc"accuracy-coor"data-dialog))))
						(strcat "\\PX=" (rtos(rem(nth 1 point)(expt 10 (cadr(assoc"numbers-coor"data-dialog)))) 2 (cadr(assoc"accuracy-coor"data-dialog)))
							"\\PY="(rtos(rem(nth 0 point)(expt 10 (cadr(assoc"numbers-coor"data-dialog)))) 2 (cadr(assoc"accuracy-coor"data-dialog))))
						) "" )
					    )
	       )
;;;	 (cons "TextLineSpacingStyle" 2)
	 )
    )
  )

(defun convert-dec-to-gms-str (dec accuracy / mm ss)
  ; переводит десятичные градусы в текстовую строку градусы минуты секунды 180d30'15"
  ;с точностью
  ;0 = 180d
  ;1 = 180d30'
  ;2 = 180d30'15"
  (setq mm (fix(*(- dec(fix dec))60)))
  (setq ss (fix(*(-(*(- dec(fix dec))60)(fix(*(- dec(fix dec))60)))60)))
  (strcat (itoa(fix dec))"d"
	  (if (> accuracy 0)(strcat(if (< mm 10.)"0" "")(itoa mm)"'")"")
	  (if (> accuracy 1)(strcat(if (< ss 10.)"0" "")(itoa ss)"\"")"")
	  )
  )

(defun convert-str-gms-to-dec (string / grad minu pos-d pos-m pos-s sek)
  ; переводит текстовую строку градусы минуты секунды 180d30'15" в десятичные градусы
  (setq pos-d (vl-string-position (ascii "d") string))
  (setq pos-m (vl-string-position (ascii "'") string))
  (setq pos-s (vl-string-position (ascii "\"") string))
  (princ (setq grad (if pos-d (atof(substr string 1 pos-d))(atof string))))(terpri)
  (princ (setq minu (if (and pos-d pos-m)(atof(substr string (+ 2 pos-d) (- pos-m pos-d 1)))0.0)))(terpri)
  (princ (setq sek (if (and pos-m pos-s)(atof(substr string (+ 2 pos-m) (- pos-s pos-m 1)))0.0)))(terpri)
  (+(float grad)(/ (+ (float minu)(/ (float sek)60.0))60.0))
  )