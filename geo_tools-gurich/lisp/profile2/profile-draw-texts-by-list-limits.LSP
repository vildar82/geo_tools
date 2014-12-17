(defun profile-draw-texts-by-list-limits
       (input-list
	limit-list
	down-y-coor-in-curent-string
	up-y-coor-in-curent-string
	height-text
	accuracy
	horizontal-scale
	vertical-scale
	curent-text-style /
	curent-point height-curent-text height-string limit-left limit-right mid-y-coor-in-curent-string number obj-list old-point scale-coefficient text-string x-coor-line-text)
  (setq height-string (- up-y-coor-in-curent-string down-y-coor-in-curent-string))
  (setq mid-y-coor-in-curent-string (+ down-y-coor-in-curent-string (/ height-string 2.0)))
  (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
  (setq number 0)
  (while (>(length input-list)number)
    (setq curent-point(caadr(nth number input-list)))
    (setq limit-left (caar limit-list))
    (setq limit-right (cadar limit-list))
    (if(not(and(> curent-point limit-left)(< curent-point limit-right)))
      ; отрисовка одиночек
      (progn
	
	(setq text-string
	       (if (=(type (car(nth number input-list)))'STR)
		 (car(nth number input-list))
		 (rtos(car(nth number input-list))2 accuracy))
	      )
	(if (eq number 0)
	  (setq height-curent-text height-text)
	  (cond
	    ((> (*(- curent-point old-point)0.6) height-text)
	     (setq height-curent-text height-text))
	    ((< (*(- curent-point old-point)0.6) (* 1.6 scale-coefficient))
	     (setq height-curent-text (* 1.6 scale-coefficient)))
	    (T
	     (setq height-curent-text (*(- curent-point old-point)0.6)))
	    )
	  )
	(setq obj-list
	       (append obj-list
		       (list
			 (draw-text-vla-by-list-property
			   (list(cons "spece" model_spece)
				(cons "TextString" text-string)
				(cons "InsertionPoint" (list curent-point mid-y-coor-in-curent-string))
				(cons "Height" height-curent-text)
				(cons "Rotation" (/ pi 2))
				(cons "Alignment" 13)
				(cons "StyleName" curent-text-style)
				(cons "Linetype" "Continuous")
				))
			 )))
	(setq obj-list
	       (append obj-list
		       (list
			 (draw-line-vla-by-list-property
			   (list
			     (cons "spece" model_spece)
			     (cons "StartPoint" (list curent-point down-y-coor-in-curent-string))
			     (cons "EndPoint" (list curent-point up-y-coor-in-curent-string))
			     (cons "Linetype" "Continuous")
			     ))
			 )))
	(setq old-point curent-point)
	(setq number (1+ number))
	)
      ; отрисовка входящих в границы
      (progn
	(setq x-coor-line-text(+ limit-left(/(*(* 2(/ 4.0 3.0))scale-coefficient)2)))
	(repeat (analisis-numbers-texts-in-limit limit-left limit-right (apply 'list (mapcar 'cadr input-list))horizontal-scale)
	  (setq text-string
	       (if (=(type (car(nth number input-list)))'STR)
		 (car(nth number input-list))
		 (rtos(car(nth number input-list))2 accuracy))
	      )
	  (setq obj-list
		 (append obj-list
			 (list
			   (draw-text-vla-by-list-property
			     (list(cons "spece" model_spece)
				  (cons "TextString" text-string)
				  (cons "InsertionPoint" (list x-coor-line-text mid-y-coor-in-curent-string))
				  (cons "Height" (* 2.0 scale-coefficient))
				  (cons "Rotation" (/ pi 2))
				  (cons "Alignment" 4)
				  (cons "StyleName" curent-text-style)
				  (cons "Linetype" "Continuous")
				  ))
			   )))
	  (setq long-arrow-pline(/(- height-string(+(cadr(calculation-text-xyz-syze (last obj-list)))(* 0.5 scale-coefficient)))2))
	  (setq obj-list
		 (append obj-list
			 (list
			   (draw-lwpolyline-vla-by-list-property
			     (list(cons "spece" model_spece)
				  (cons "Coordinates" (list
							(list curent-point up-y-coor-in-curent-string)
							(list curent-point (- up-y-coor-in-curent-string (* 1.0 scale-coefficient)))
							(list x-coor-line-text (- up-y-coor-in-curent-string (* long-arrow-pline (/ 2.0 3.0))))
							(list x-coor-line-text (- up-y-coor-in-curent-string long-arrow-pline))
							))
				  (cons "Linetype" "Continuous")
				  ))
			   )))
	  (setq obj-list
		 (append obj-list
			 (list
			   (draw-lwpolyline-vla-by-list-property
			     (list(cons "spece" model_spece)
				  (cons "Coordinates" (list
							(list x-coor-line-text (+ down-y-coor-in-curent-string long-arrow-pline))
							(list x-coor-line-text (+ down-y-coor-in-curent-string (* long-arrow-pline (/ 2.0 3.0))))
							(list curent-point (+ down-y-coor-in-curent-string (* 1.0 scale-coefficient)))
							(list curent-point down-y-coor-in-curent-string)
							))
				  (cons "Linetype" "Continuous")
				  ))
			   )))
	  (setq x-coor-line-text (+ x-coor-line-text (* (* 2.0 (/ 4.0 3.0)) scale-coefficient)))
	  (setq number (1+ number))
	  (setq curent-point(caadr(nth number input-list)))
	  )
	(setq old-point limit-right)
	(setq limit-list (cdr limit-list))
	)
      )
    )
  obj-list
  )