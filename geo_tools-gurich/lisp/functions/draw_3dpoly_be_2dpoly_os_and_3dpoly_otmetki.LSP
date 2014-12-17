(defun C:draw_3dpoly_be_2dpoly_os_and_3dpoly_otmetki ( / 2dpline 3dpline flag2 h h1 h2 n n0 n00 p1 p2 s1 s2 spisokvall y_n)
  ; По осевой 2D полилинии и 3D полилинии (или поверхности) с отметками строится полилиния оси с отметками.
  (geo_tools-help "draw_3dpoly_be_2dpoly_os_and_3dpoly_otmetki")
  (load_global_variable)
  (initget)(setq 2Dpline (car (entsel "\nВыберите 2D полилинию оси трассы: ")))
  (cond
    ((eq 2Dpline nil)
     (alert "Ничего не выбрано!"))
    ((not(eq (cdr(assoc 0 (entget 2Dpline))) "LWPOLYLINE"))
     (alert "Выбрана не 2D полилиния!"))
    ((eq (cdr(assoc 0 (entget 2Dpline))) "LWPOLYLINE")
     (initget)(setq 3Dpline (car (entsel "\nВыберите 3D полилинию или поверхность: ")))
     (cond
       ((eq 3Dpline nil)
	(alert "Ничего не выбрано!"))
       ((not(or (eq (cdr(assoc 0 (entget 3Dpline))) "POLYLINE")(eq (cdr(assoc 0 (entget 3Dpline))) "AECC_TIN_SURFACE")))
	(alert "Выбран не допустимый объект!"))
       ((or (eq (cdr(assoc 0 (entget 3Dpline))) "POLYLINE")(eq (cdr(assoc 0 (entget 3Dpline))) "AECC_TIN_SURFACE"))
	(initget "Да Нет")(setq Y_N (getkword "\nИнтерполировать точки без отметок? [Да/Нет] <Нет>: "))
	(if(eq Y_N nil)(setq Y_N "Нет"))
	(vla-StartUndoMark active_document)
	(setq spisokvALL
	       (if(eq (vla-get-ObjectName (vlax-ename->vla-object 3Dpline)) "AcDb3dPolyline")
		    (analisis-3d-and-2d-poly-by-general-3dpoly (vlax-ename->vla-object 2Dpline) (vlax-ename->vla-object 3Dpline))
		    (analisis-surfese-and-2d-poly 3Dpline 2Dpline)
		    )
	      )

	;                        ИНТЕРПОЛЯЦИЯ ТОЧЕК БЕЗ ОТМЕТОК
	(if
	  (eq Y_N "Да")
	  (progn
	    (setq n (length spisokvALL))
	    (setq n0 0)
	    (repeat n
	      (cond
		((and (= n0 0) (= (nth 2 (nth n0 spisokvALL)) 0.0)); первая
		 (setq flag2 nil)
		 (setq n00 (1+ n0))
		 (while (not flag2)
		   (if (= (nth 2 (nth n00 spisokvALL)) 0.0)
		     (setq n00 (1+ n00))
		     (progn
		       (setq spisokvALL
			      (subst (list (nth 0 (nth n0 spisokvALL)) (nth 1 (nth n0 spisokvALL)) (nth 2(nth n00 spisokvALL)))
				     (nth n0 spisokvALL)
				     spisokvALL)
			     )
		       (setq flag2 T)
		       )
		     ); if
		   )
		 )
		((and (= n0 (1- n)) (= (nth 2 (nth n0 spisokvALL)) 0.0)); последняя
		 (setq flag2 nil)
		 (setq n00 (1- n0))
		 (while (not flag2)
		   (if (= (nth 2 (nth n00 spisokvALL)) 0.0)
		     (setq n00 (1- n00))
		     (progn
		       (setq spisokvALL
			      (subst (list (nth 0 (nth n0 spisokvALL)) (nth 1 (nth n0 spisokvALL)) (nth 2(nth n00 spisokvALL)))
				     (nth n0 spisokvALL)
				     spisokvALL)
			     )
		       (setq flag2 T)
		       )
		     ); if
		   )
		 )
		((and (and (> n0 0) (< n0 (1- n)))(= (nth 2 (nth n0 spisokvALL)) 0.0)); серединые
		 (setq p1 (nth (1- n0) spisokvALL))
		 (setq H1 (nth 2 p1))
		 (setq s1 (distance (nth n0 spisokvALL) p1))
		 (setq n00 (1+ n0))
		 (setq s2 (distance (list(nth 0(nth n0 spisokvALL))(nth 1(nth n0 spisokvALL)))
				    (list(nth 0(nth n00 spisokvALL))(nth 1(nth n00 spisokvALL)))
				    ))
		 (while (= (nth 2 (nth n00 spisokvALL)) 0.0)
		   (setq s2 (+ s2 (distance (list(nth 0(nth n00 spisokvALL))(nth 1(nth n00 spisokvALL)))
					    (list(nth 0(nth (1+ n00) spisokvALL))(nth 1(nth (1+ n00) spisokvALL))))))
		   (setq n00 (1+ n00))
		   (if (= n00 n)
		     (setq n00 (1- n0)))
		   )
		 (setq p2 (nth n00 spisokvALL))
		 (setq H2 (nth 2 p2))
		 (setq H (+(*(/(- H2 H1)(+ s1 s2))s1)H1))
		 (setq spisokvALL
			(subst (list (nth 0 (nth n0 spisokvALL)) (nth 1 (nth n0 spisokvALL)) H)
			       (nth n0 spisokvALL)
			       spisokvALL)
		       )
		 )
		)
	      (setq n0 (1+ n0))
	      )
	    )
	  )
;;;	(draw_pline_entmake spisokvALL nil T 0 0)
	(vla-add3dPoly model_spece
	  (vlax-safearray-fill
	    (vlax-make-safearray vlax-vbDouble (cons 0 (1-(*(length spisokvALL)3))))
	    (apply 'append (mapcar 'list (mapcar 'car spisokvALL)
				   (mapcar 'cadr spisokvALL)
				   (mapcar 'caddr spisokvALL)))))
	(vla-EndUndoMark active_document)
	(princ)
	)
       )
     )
    )
  )
