(defun C:draw_arc_demention ( / ARC-OBJ H-TEXT MTEXT P1 P2 P3 *ERROR*)
  (geo_tools-help "draw_arc_demention")
  (load_global_variable)
  
  (defun *ERROR* (msg)
    (draw_osnap_on)
    (princ)
    )
  
  (if(setq p1 (getpoint "\nУкажи первую точку измеряемой линии<Выход>: "))
    (if(setq p2 (getpoint p1 "\nУкажи вторую точку измеряемой линии: "))
      (progn
	(draw_osnap_off)
	(if(=(setq p3 (getpoint "\nУкажи куда положить подпись<Прямая>: "))nil)
	  (progn
	    (setq p3 (polar p1 (angle p1 p2) (/(distance p1 p2)2.0)))
	    (progn
	      (vla-StartUndoMark active_document)
	      (setq h-text 2.5)
	      
	      (setvar "cmdecho" 0)
	      (draw-line-vla-by-list-property
		(list
		  (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
		  (cons "StartPoint" p1)
		  (cons "EndPoint" p2)
		  ))
	      
	      (draw_osnap_on)
	      
	      (setvar "cmdecho" 1)
	      
	      (setq mtext
		     (draw-mtext-vla-by-list-property(list
						       (cons "spece" model_spece)
						       (cons "TextString" (rtos (distance p1 p2) 2 2))
						       (cons "InsertionPoint" p3)
						       (cons "Height" (/ h-text(format_scale_kof)))
						       (cons "Width" (calculation_str_long_be_h_stile (rtos (distance p1 p2) 2 2) (/ h-text(format_scale_kof)) (getvar "TEXTSTYLE")))
						       (cons "AttachmentPoint" 5)
						       (cons "StyleName" (getvar "TEXTSTYLE"))
						       (cons "Rotation" (analysis_angle_be_long_distance (angle p1 p2)))
						       (cons "BackgroundFill" :vlax-true)
						       ))
		    )
	      (entmod (subst (cons 45 1.2) (assoc 45 (entget (entlast))) (entget (entlast)) ))
	      (vla-EndUndoMark active_document)
	      )
	    )
	  (progn
	    (vla-StartUndoMark active_document)
	    (setq h-text 2.5)
	    
	    (setvar "cmdecho" 0)
	    
	    (command "_arc" p1 p3 p2)
	    (draw_osnap_on)
	    (setq arc-obj (vlax-ename->vla-object (entlast)))
	    (setvar "cmdecho" 1)
	    (setq p3 (vlax-curve-getClosestPointTo arc-obj p3))
	    (setq mtext
		   (draw-mtext-vla-by-list-property(list
						     (cons "spece" model_spece)
						     (cons "TextString" (rtos (distance p1 p2) 2 2))
						     (cons "InsertionPoint" p3)
						     (cons "Height" (/ h-text(format_scale_kof)))
						     (cons "Width" (calculation_str_long_be_h_stile (rtos (distance p1 p2) 2 2) (/ h-text(format_scale_kof)) (getvar "TEXTSTYLE")))
						     (cons "AttachmentPoint" 5)
						     (cons "StyleName" (getvar "TEXTSTYLE"))
						     (cons "Rotation" (analysis_angle_be_long_distance(angle '(0 0 0) (vlax-curve-getFirstDeriv arc-obj (vlax-curve-getParamAtPoint arc-obj p3)))))
						     (cons "BackgroundFill" :vlax-true)
						     ))
		  )
	    (entmod (subst (cons 45 1.2) (assoc 45 (entget (entlast))) (entget (entlast)) ))
	    (vla-EndUndoMark active_document)
	    )
	  ))))
  (princ)
  )