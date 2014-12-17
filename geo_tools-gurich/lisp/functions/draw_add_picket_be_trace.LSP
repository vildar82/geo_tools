(defun C:draw_add_picket_be_trace ; вычисляет пикет траассы в указаной точке
       (/ list-dialog 2Dpline ht styl 2Dpline_vla spisokv2D list_points pt_array mleader_obj p1)
  (geo_tools-help "draw_add_picket_be_trace")
  (load_global_variable)
  (initget)(setq 2Dpline (car (entsel "\nВыберите 2D полилинию оси трассы: ")))
  (setq ht (/ 2.5 (getvar "CANNOSCALEVALUE")))
  (setq styl (getvar "TEXTSTYLE"))
  (cond
    ((eq 2Dpline nil)
     (alert "Ничего не выбрано!"))
    ((not(eq (cdr(assoc 0 (entget 2Dpline))) "LWPOLYLINE"))
     (alert "Выбрана не 2D полилиния!"))
    ((eq (cdr(assoc 0 (entget 2Dpline))) "LWPOLYLINE")
     (setq 2Dpline_vla (vlax-ename->vla-object 2Dpline))
     (setq spisokv2D (extract_coord_lwpolyline (entget 2Dpline)))
     (initget)(setq p1(getpoint "\nУкажи определяемую точку:"))
     (setq list-dialog (dialog-draw-add-picket-be-trace))
     
     (setq list_points (list p1(mapcar '+ p1 '(-10 10))))
     (setq pt_array (convert_coords_list_from_safearray_mleader list_points))
     (vla-StartUndoMark active_document)
     (setq mleader_obj (vla-addmleader model_spece pt_array 0))
     (vla-put-StyleName mleader_obj "Подписи")
     (vla-put-TextStyleName mleader_obj styl)
     (vla-put-textstring mleader_obj (strcat "\\PПК"
					     (convert-distance-to-pk-string(+(vlax-curve-getDistAtPoint 2Dpline p1)(cadr(assoc"picket-first"list-dialog))) 2 T)
					     (if (eq (cadr(assoc"draw-coor"list-dialog)) 1)
					       (if (eq (cadr(assoc"numbers-coor"list-dialog)) 0)
						 (strcat "\\PX=" (rtos(nth 1 p1)2 (cadr(assoc"accuracy-coor"list-dialog)))"\\PY="(rtos(nth 0 p1)2 (cadr(assoc"accuracy-coor"list-dialog))))
						 (strcat "\\PX=" (rtos(rem(nth 1 p1)(expt 10 (cadr(assoc"numbers-coor"list-dialog)))) 2 (cadr(assoc"accuracy-coor"list-dialog)))
							 "\\PY="(rtos(rem(nth 0 p1)(expt 10 (cadr(assoc"numbers-coor"list-dialog)))) 2 (cadr(assoc"accuracy-coor"list-dialog))))
						 )
					       ""
					       )
					     )
       )
     (vla-put-TextHeight mleader_obj ht)
     (vla-EndUndoMark active_document)
     )
    )
  (princ)
  )


(defun dialog-draw-add-picket-be-trace (/ dcl_id list-dialog)
  
  (setq list-dialog(get-list-dialog-design-trace))
  (setq dcl_id (geo-tools-load-dialog "d_add_piket"))
  
  (new_dialog "d_add_piket" dcl_id)
  (if
    (=(cadr (assoc "draw-coor" list-dialog))0)
    (foreach x (list "numbers-coor" "accuracy-coor" )(mode_tile x 1))
    (foreach x (list "numbers-coor" "accuracy-coor" )(mode_tile x 0))
    )
  
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
  (action_tile "draw-coor"
    (vl-prin1-to-string
      '(progn
	(setq list-dialog(subst (list "draw-coor" (atoi $value))(assoc "draw-coor" list-dialog) list-dialog))
	(if
	 (= (atoi $value) 0)
	 (foreach x (list "numbers-coor" "accuracy-coor" )(mode_tile x 1))
	 (foreach x (list "numbers-coor" "accuracy-coor" )(mode_tile x 0))
	 )
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
  (if(eq(start_dialog)0)
    (setq list-dialog nil)
    (vlax-ldata-put "geo_tools_dictionary" "dialog-design-trace" list-dialog)
    )
  (unload_dialog dcl_id)
  list-dialog
  )

