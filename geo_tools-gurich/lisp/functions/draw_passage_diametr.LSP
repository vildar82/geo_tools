(defun C:draw_passage_diametr (/ DIAMETR LENGTH-PEREHOD LIST-OBJ-1 LIST-OBJ-2 NEW-DIAMETR OBJ-1 OBJ-2 P1 P2 PEREHOD PIL1 PIL2 PROTIV)
  (load_global_variable)
  (setq list-obj-1 (entsel "\nВыбери первую сторону трубы: "))
  (cond
    ((eq list-obj-1 nil)
     (alert "Ничего не выбрано!"))
    ((not(or (eq (cdr(assoc 0 (entget (car list-obj-1)))) "LINE")(eq (cdr(assoc 0 (entget (car list-obj-1)))) "LWPOLYLINE")))
     (alert "Ошибка!\nВыбран не допустимый объект!\nВыбирать можно Отрезки и 2Dполилинии."))
    ((or (eq (cdr(assoc 0 (entget (car list-obj-1)))) "LINE")(eq (cdr(assoc 0 (entget (car list-obj-1)))) "LWPOLYLINE"))
     (setq list-obj-2 (entsel "\nВыбери вторую сторону трубы: "))
     (cond
       ((eq list-obj-2 nil)
	(alert "Ничего не выбрано!"))
       ((not(or (eq (cdr(assoc 0 (entget (car list-obj-2)))) "LINE")(eq (cdr(assoc 0 (entget (car list-obj-2)))) "LWPOLYLINE")))
	(alert "Ошибка!\nВыбран не допустимый объект!\nВыбирать можно Отрезки и 2Dполилинии."))
       ((or (eq (cdr(assoc 0 (entget (car list-obj-2)))) "LINE")(eq (cdr(assoc 0 (entget (car list-obj-2)))) "LWPOLYLINE"))
	
	(setq obj-1(vlax-ename->vla-object(car list-obj-1))
	      p1(cadr list-obj-1)
	      pil1 (vlax-curve-getClosestPointTo obj-1 p1))
	(setq obj-2(vlax-ename->vla-object(car list-obj-2))
	      p2(cadr list-obj-2)
	      pil2(vlax-curve-getClosestPointTo obj-2 p2))
	(if T ; здесь потом можно будет сделать контроль на паралельность труб
	  (progn
	    (setq protiv (vlax-curve-getClosestPointTo obj-2 pil1))
	    (setq diametr (distance protiv pil1))
	    (initget 6)(setq new-diametr(getreal(strcat "\nУкажи новый диаметр <" (rtos diametr 2 3) ">:")))
	    
	    (if (/= new-diametr nil)
	      (progn
		(setq perehod (distance protiv pil2))
		(initget 6)(setq length-perehod(getreal(strcat "\nУкажи длинну перехода <" (rtos perehod 2 3) ">:")))
		(if (= length-perehod nil)(setq length-perehod perehod))
		(vla-StartUndoMark active_document)
		(draw-lwpolyline-vla-by-list-property
		  (list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
		       (cons "Coordinates" (list
					     pil1
					     protiv
					     (polar (polar (polar pil1(angle pil1 protiv)(/ diametr 2))(angle protiv pil2)length-perehod)(angle pil1 protiv )(/ new-diametr 2))
					     (polar (polar (polar pil1(angle pil1 protiv)(/ diametr 2))(angle protiv pil2)length-perehod)(angle protiv pil1)(/ new-diametr 2))
					     )
			     )
		       (cons "Closed" 1)
		       ))
		(if (> diametr new-diametr)
		  (draw-lwpolyline-vla-by-list-property
		    (list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
			 (cons "Coordinates" (list
					       (polar (polar (polar pil1(angle pil1 protiv)(/ diametr 2))(angle protiv pil2)length-perehod)(angle pil1 protiv )(/ diametr 2))
					       (polar (polar (polar pil1(angle pil1 protiv)(/ diametr 2))(angle protiv pil2)length-perehod)(angle protiv pil1)(/ diametr 2))
					       )
			       )
			 ))
		  )
		(vla-EndUndoMark active_document)
		)
	      )
	    );progn
	  );if
	)
       )
     
     
     )
    )
  (princ)
  )