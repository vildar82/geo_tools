(defun C:draw_delta_os (/ list-points put-text-file string temp insert-point insert-point-list angle-list new-point height-text insert-text-point p1 p3 otstup-text)
  ; Читает из файла и чертит отклонения оси башни полилинией и среднее направление отклонений линией.
  (setq height-text (/ 2.5(getvar "CANNOSCALEVALUE")))
  (setq otstup-text (* height-text 2))
  (if(setq put-text-file (getfiled "Выбор файла" (getvar "DWGPREFIX") "txt" 0))
    (progn
      (foreach x (file_read_to_list put-text-file)
	(setq string (vl-string-trim " \t" x))
	(if (/= string "")
	  (progn
	    (setq temp (convert-string-to-list-be-delimiter string "\t"))
	    (if temp
	      (setq list-points (append list-points (list(mapcar 'atof temp))))
	      )
	    )
	  )
	)
      (setq insert-point(getpoint "\nУкажи точку:"))
      (foreach y list-points
	(setq new-point(mapcar '+ insert-point (list (nth 1 y)(nth 0 y))))
	(setq insert-point-list (append insert-point-list (list new-point)))
	(setq angle-list (append angle-list (list (angle insert-point new-point))))
	)
      (vla-StartUndoMark active_document)
      (draw-line-vla-by-list-property
	(list
	  (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	  (cons "StartPoint" insert-point)
	  (cons "EndPoint" (polar insert-point (/(apply '+ (cdr angle-list))(1-(length angle-list))) (distance insert-point (last insert-point-list))))
	  (cons "Color" 1)
	  ))
      (draw-lwpolyline-vla-by-list-property
	(list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	     (cons "Coordinates" insert-point-list)
	     ))
      (foreach x insert-point-list
	(cond
	  ((eq (vl-position x insert-point-list) 0) ; первая точка в списке
	   (setq insert-text-point (polar x (angle (nth (1+(vl-position x insert-point-list))insert-point-list) x)otstup-text))
	   )
	  ((eq (vl-position x insert-point-list) (1-(length insert-point-list))); последняя точка в списке
	   (setq insert-text-point (polar x (angle (nth (1-(vl-position x insert-point-list))insert-point-list) x)otstup-text))
	   )
	  (T ; все осальные точки списка
	   (setq p1(nth(1-(vl-position x insert-point-list))insert-point-list))
	   (setq p3(nth(1+(vl-position x insert-point-list))insert-point-list))
	   (cond
	     ((> (calculation_angle_be_3points p1 x p3) (calculation_angle_be_3points p3 x p1))
	      (setq insert-text-point (polar x (+(angle x p3) (/(calculation_angle_be_3points p1 x p3)2)) otstup-text)))
	     ((< (calculation_angle_be_3points p1 x p3) (calculation_angle_be_3points p3 x p1))
	      (setq insert-text-point (polar x (+(angle x p1) (/(calculation_angle_be_3points p3 x p1)2)) otstup-text)))
	     ((= (calculation_angle_be_3points p1 x p3) (calculation_angle_be_3points p3 x p1))
	      (setq insert-text-point (polar x (+(angle x p3) (/ pi 2)) otstup-text))))
	   )
	  )
	(draw-text-vla-by-list-property
	  (list
	    (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	    (cons "TextString" (itoa(vl-position x insert-point-list)))
	    (cons "InsertionPoint" insert-text-point)
	    (cons "Height" height-text)
	    (cons "Rotation" 0)
	    ;(cons "Color" 1)
	    (cons "Alignment" 4)
	    ))
	(draw-circle-vla-by-list-property
	  (list
	    (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	    (cons "InsertionPoint" x)
	    (cons "Radius" (/ height-text 3))
	    
	    ))
	)
      (vla-EndUndoMark active_document)
      )
    )
  (princ)
  )