(defun C:draw_elevation_contour_line (/ contour-obj h-text insert-point mtext pik-point temp-list angle-text-final temp flag)
  ; чертит отметку горизонтали
  (defun *ERROR* (msg)
    (draw_osnap_on)
    (princ)
    )
  (initget 1)(setq temp-list (entsel "\nВыбери горизонталь (не Civil): "))
  (setq contour-obj (car temp-list))
  (setq pik-point (cadr temp-list))
  (setq h-text (/ 2.0(format_scale_kof)))
  (setq text-style "OTI")
  (if contour-obj
    (progn
      (setq insert-point (vlax-curve-getClosestPointTo contour-obj pik-point))
      (setq dialog-data(get-list-dialog-draw-elevation-contour-line))
      (while (not flag)
	(draw_osnap_off)
	(initget "Настройки")(setq angle-point (getpoint insert-point "\nУкажи точку вверх по склону[Настройки]: "))
	(draw_osnap_on)
	(cond
	  ((= angle-point "Настройки")
	   (if (not(setq dialog-data (dialog_draw_elevation_contour_line)))
	     (setq dialog-data(get-list-dialog-draw-elevation-contour-line)))
	   )
	  (T
	   (vla-StartUndoMark active_document)
	   (setq angle-text(angle '(0 0 0) (vlax-curve-getFirstDeriv contour-obj (vlax-curve-getParamAtPoint contour-obj insert-point))))
	   (setq angle-input (angle insert-point angle-point))
	   (if(minusp (setq temp (- angle-text angle-input)))
	     (setq temp (+ (+ pi pi) (- angle-text angle-input)))
	     )
	   (if (< temp pi)
	     (setq angle-text (+ angle-text pi))
	     )
	   (cond
	     ((=(get-item-by-name-in-list dialog-data "input-mode")0)
	      (setq elevation-contour-line
		     (if (= (caddr insert-point)0.0)
		       (getreal"\nВведи отметку горизонтали: ")
		       (caddr insert-point)))
	      )
	     ((=(get-item-by-name-in-list dialog-data "input-mode")1); всегда
	      (setq elevation-contour-line (getreal"\nВведи отметку горизонтали: "))
	      )
	     ((=(get-item-by-name-in-list dialog-data "input-mode")2); никогда
	      (setq elevation-contour-line (caddr insert-point))
	      )
	     )
	   (if (=(get-item-by-name-in-list dialog-data "smart-accuracy-elevation")1)
	     (progn
	       (setq temp-text-contour-line (rtos elevation-contour-line 2 3))
	       (setq temp-text-contour-line (substr temp-text-contour-line (+ (vl-string-position (ascii ".") temp-text-contour-line)2)))
	       (setq text-contour-line (rtos elevation-contour-line 2
					     (cond
					       ((= temp-text-contour-line "000")
						0
						)
					       ((= temp-text-contour-line "500")
						1
						)
					       ((or(= temp-text-contour-line "250")(= temp-text-contour-line "750"))
						2
						)
					       (T
						3
						)
					       )
					     ))
	       )
	     (setq text-contour-line (rtos elevation-contour-line 2 (get-item-by-name-in-list dialog-data "accuracy-elevation")))
	     )
	   (setq mtext
		  (draw-mtext-vla-by-list-property(list
						    (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
						    (cons "TextString" text-contour-line)
						    (cons "InsertionPoint" insert-point)
						    (cons "Height" h-text)
						    (cons "Width" (calculation_str_long_be_h_stile text-contour-line h-text text-style))
						    (cons "AttachmentPoint" 5)
						    (cons "StyleName" text-style)
						    (cons "Rotation" angle-text)
						    (cons "BackgroundFill" :vlax-true)
						    ))
		 )
	   (vla-EndUndoMark active_document)
	   (setq flag T)
	   )
	  )
	)
      )
    )
  (princ)
  )

(defun dialog_draw_elevation_contour_line ()
  (setq list-dialog(get-list-dialog-draw-elevation-contour-line))
  (setq dcl_id (geo-tools-load-dialog "dialog_draw_elevation_contour_line"))
  (new_dialog "dialog_draw_elevation_contour_line" dcl_id)
  (foreach x (cdr list-dialog)
    (set_tile (car x)
	      (cond
		((="version"(car x))
		 (rtos(cadr x)2 2)
		 )
		(T
		 (itoa (cadr x)))
		)
	      )
    )
  (if (=(get-item-by-name-in-list list-dialog "smart-accuracy-elevation")1)
	 (mode_tile "accuracy-elevation" 1)
	 (mode_tile "accuracy-elevation" 0)
	 )
  (action_tile "input-mode"(vl-prin1-to-string'(setq list-dialog(subst (list "input-mode" (atoi $value))(assoc "input-mode" list-dialog) list-dialog))))
  (action_tile "accuracy-elevation"(vl-prin1-to-string'(setq list-dialog(subst (list "accuracy-elevation" (atoi $value))(assoc "accuracy-elevation" list-dialog) list-dialog))))
  (action_tile "smart-accuracy-elevation"
    (vl-prin1-to-string
      '(progn
	(setq list-dialog(subst (list "smart-accuracy-elevation" (atoi $value))(assoc "smart-accuracy-elevation" list-dialog) list-dialog))
	(if (=(get-item-by-name-in-list list-dialog "smart-accuracy-elevation")1)
	 (mode_tile "accuracy-elevation" 1)
	 (mode_tile "accuracy-elevation" 0)
	 )
	
	)
      )
    )
  
    (if(eq(start_dialog)0)
    (setq list-dialog nil)
    (vlax-ldata-put "geo_tools_dictionary" "dialog-draw-elevation-contour-line" list-dialog)
    )
  (unload_dialog dcl_id)
  list-dialog
  
  )


(defun get-list-dialog-draw-elevation-contour-line ( / list-dialog)
  (setq list-dialog(vlax-ldata-get "geo_tools_dictionary" "dialog-draw-elevation-contour-line"))
  (if (<(get-item-by-name-in-list list-dialog "version") 2.0)
    (setq list-dialog (list
			(list "version" 2.0)
			(list "input-mode" 0)
			(list "smart-accuracy-elevation" 1)
			(list "accuracy-elevation" 0)
			)
	  )
    )
  list-dialog
  )