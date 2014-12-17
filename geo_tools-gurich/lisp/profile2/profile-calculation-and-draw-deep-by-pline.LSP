(defun profile-calculation-and-draw-deep-by-pline (block-profile-obj 2Dpline list-select-string /
						   curent-text-style down-y-coor-in-curent-string height-text horizontal-scale list-inters inter
						   list-points list-vertex max-y min-y number-string point profile-parameters profile-string-list
						   scale-coefficient second-list-vertex second-pline-obj start-point up-y-coor-in-curent-string vertical-scale accuracy
						   elevation-1 elevation-2 flag text-string
						   )
  (if (/=(type 2Dpline) 'list)
    (setq second-pline-obj(car(entsel"\nВыбери полилинию на профиле до которой будем мерить глубину: ")))
    )

  (if(=(vlax-ldata-get "geo_tools_dictionary" "profile-mode")1)
    (progn
      (setq up-y-coor-in-curent-string (nth 1 list-select-string))
      (setq down-y-coor-in-curent-string(nth 2 list-select-string))
      (setq number-string(nth 0 list-select-string))
      (setq profile-string-list(vlax-ldata-get block-profile-obj"profile-string-list"))
      (setq list-select-string(nth number-string profile-string-list))
      (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
      (setq vertical-scale (vlax-ldata-get block-profile-obj"vertical-scale"))
      (setq profile-parameters(vlax-ldata-get block-profile-obj"profile-parameters"))
      (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
      (setq start-point(vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj))))
      (setq curent-text-style (nth 1 profile-parameters))
      (if(or(eq curent-text-style "***Текущий***")(not(member curent-text-style(get-text-style-list))))
	(setq curent-text-style(getvar "TEXTSTYLE")))
      (setq height-text (*(atof(nth 6 list-select-string))scale-coefficient))
      (setq accuracy (atoi(nth 4 list-select-string)))
      (setq flag T)
      )
    (progn
      (if(setq down-y-coor-in-curent-string(cadr(getpoint"\nУкажи точку на нижней линии строки: ")))
	(if(setq up-y-coor-in-curent-string(cadr(getpoint "\nУкажи точку на верхней линии строки: ")))
	  (progn
	    (setq horizontal-scale (cadr(assoc "horizontal-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	    (setq vertical-scale (cadr(assoc "vertical-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	    (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
	    (setq curent-text-style(getvar "TEXTSTYLE"))
	    (setq height-text (*(cadr(assoc "height-text" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list")))
				scale-coefficient))
	    (setq accuracy (cadr(assoc "accuracy" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	    (setq flag T)
	    )
	  ))
      )
    )
  (if (=(type 2Dpline) 'list)
    (progn
      (if (or(= (nth 1 2Dpline) T)(and (> (nth 1(nth 0 2Dpline))down-y-coor-in-curent-string)(< (nth 1(nth 0 2Dpline))up-y-coor-in-curent-string)))
	(if(setq elevation-1 (get-elevation-be-text-obj (car(entsel "\nВыбери текст с первой отметкой: "))))
	  (if(setq elevation-2 (get-elevation-be-text-obj (car(entsel "\nВыбери текст со второй отметкой: "))))
	    (setq text-string (rtos (abs(- elevation-1 elevation-2)) 2 accuracy))
	    )
	  )
	(setq text-string (rtos(abs(*(-
				       (get-item-deep-list (list 0 1) 2Dpline)
				       (get-item-deep-list (list 1 1) 2Dpline)
				       )
				     (/ (* vertical-scale (/ 1000.0 horizontal-scale)) 1000.0))
				   )
			       2 accuracy))
	)
      (draw-text-vla-by-list-property
	(list(cons "spece" model_spece)
	     (cons "TextString" text-string)
	     (cons "InsertionPoint" (list (caar 2Dpline) (+ down-y-coor-in-curent-string (/ (- up-y-coor-in-curent-string down-y-coor-in-curent-string) 2.0))))
	     (cons "Height" height-text)
	     (cons "Rotation" (/ pi 2))
	     (cons "Alignment" 13)
	     (cons "StyleName" curent-text-style)
	     (cons "Linetype" "Continuous")
	     ))
      (draw-line-vla-by-list-property
	(list
	  (cons "spece" model_spece)
	  (cons "StartPoint" (list (caar 2Dpline) down-y-coor-in-curent-string))
	  (cons "EndPoint" (list (caar 2Dpline) up-y-coor-in-curent-string))
	  (cons "Linetype" "Continuous")
	  ))
      )
    (cond
      ((eq second-pline-obj nil)
       (alert "Ничего не выбрано!"))
      ((not(eq (cdr(assoc 0 (entget second-pline-obj))) "LWPOLYLINE"))
       (alert "Выбрана не 2D полилиния!"))
      ((eq (cdr(assoc 0 (entget second-pline-obj))) "LWPOLYLINE")
       (if flag
	 (progn
	   (setq list-vertex(extract_coord_lwpolyline (entget 2Dpline)))
	   (setq second-list-vertex (extract_coord_lwpolyline (entget second-pline-obj)))
	   (setq max-y (+(max (apply 'max (mapcar 'cadr list-vertex))(apply 'max (mapcar 'cadr second-list-vertex)))10))
	   (setq min-y (-(min (apply 'min (mapcar 'cadr list-vertex))(apply 'min (mapcar 'cadr second-list-vertex)))10))
	   (repeat (length list-vertex)
	     (setq point (car list-vertex))
	     (setq list-vertex (cdr list-vertex))
	     (setq inter (car(inters-2point-by-list-points (list (nth 0 point) max-y) (list (nth 0 point) min-y) second-list-vertex)))
	     (if inter
	       (progn
		 (setq list-inters(append list-inters(list (list (abs(* (-(nth 1 inter)(nth 1 point)) (/ (* vertical-scale (/ 1000.0 horizontal-scale)) 1000.0))) inter))))
		 )
	       )
	     )
	   (vla-StartUndoMark active_document)
	   (profile-draw-texts-by-list-limits
	     list-inters
	     (profile-analisis-limits (apply 'list (mapcar 'cadr list-inters))horizontal-scale)
	     down-y-coor-in-curent-string
	     up-y-coor-in-curent-string
	     height-text
	     accuracy
	     horizontal-scale
	     vertical-scale
	     curent-text-style)
	   (vla-EndUndoMark active_document)
	   )
	 )
       )
      )
    )
  
  (princ)
  )

(defun inters-2point-by-list-points (point-1 point-2 list-points / centr result-list t1 t2)
  (repeat (1- (length list-points))
    (setq t1 (car list-points))
    (setq list-points (cdr list-points))
    (setq t2 (car list-points))
    (setq centr (inters (list (nth 0 point-1)(nth 1 point-1))(list (nth 0 point-2)(nth 1 point-2))
			(list (nth 0 t1) (nth 1 t1))(list (nth 0 t2) (nth 1 t2))))
    (if centr
      (setq result-list (append result-list(list centr)))
      )
    )
  result-list
  )

(defun get-elevation-be-text-obj (text-obj / LIST-ENTGET)
  (if text-obj
    (progn
      (setq list-entget (entget text-obj))
      (if (=(cdr(assoc 0 list-entget))"TEXT")
	(atof(edit_text_clean_for_number (cdr(assoc 1 list-entget))))
	)
      )
    )
  )