(defun C:draw_punkts_by_square ( / insert-square list-square ne-point nw-point point punkt-point se-point square square/2 sw-point o-doc
				collection-square list-square-coll n nn nnn punkt-recorg squares summ type-list flag filter-list)
  ;наносит пункты каталога координат попадающие в квадрат заданный центром и длинной стороны или
  (geo_tools-help "draw_punkts_by_square")
  (if (setq o-doc(XML-Get-XMLObject (get-patch-to-katalog)))
    (progn
      (setq filter-list (get-current-filter-list (get-filter-katalog-punkts) (get-list-tipe-punkts-in-katalog o-doc)))
      (setq temp-filter-list filter-list)
      (while (not flag)
	(initget "Весь Фильтр")(setq point (getpoint "\nУкажи точку в центре квадрата или [Весь каталог/Фильтр]<Enter-Выход>: "))
	(if (= point "Фильтр")
	  (if(not(setq filter-list (dialog-filter-draw-punkts-by-square o-doc filter-list)))
	    (setq filter-list temp-filter-list)
	    )
	  (setq flag T)
	  )
	)
      (setq filter-mode (get-item-by-name-in-list filter-list "filter-mode"))
      (if point
	(progn
	  (setq nnn 0)
	  (if
	    (/= point "Весь")
	    (progn
	      (if(not(setq insert-square (getreal "\nВведи длинну стороны квадрата выборки в км <2.0>: ")))
		(setq insert-square 2.0))
	      (setq squares(XML-Get-Child (XML-Get-Child o-doc nil "puncts") nil "squares"))
	      (setq type-list(get-punct-type-list-in-katalog o-doc))
	      (add-mggt-punct-types-in-drawning)
	      (setq square (* insert-square 1000.0))
	      (setq square/2 (/ square 2.0))
	      (setq nw-point (list (-(nth 0 point)square/2) (+(nth 1 point)square/2)(nth 2 point)))
	      (setq ne-point (list (+(nth 0 point)square/2) (+(nth 1 point)square/2)(nth 2 point)))
	      (setq sw-point (list (-(nth 0 point)square/2) (-(nth 1 point)square/2)(nth 2 point)))
	      (setq se-point (list (+(nth 0 point)square/2) (-(nth 1 point)square/2)(nth 2 point)))
	      ;;;	    (draw-lwpolyline-vla-by-list-property
	      ;;;	      (list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      ;;;		   (cons "Coordinates" (list nw-point ne-point se-point sw-point))
	      ;;;		   (cons "Closed" -1)
	      ;;;		   ))
	      (setq list-square (edit-list-del-duplicate-item
				  (list (analisis-point-square nw-point)
					(analisis-point-square ne-point)
					(analisis-point-square sw-point)
					(analisis-point-square se-point)
					)))
	      (foreach square list-square
		(if (setq square (XML-Get-Child-ByAttribute squares nil "name" square))
		  (progn
		    (foreach x (XML-Get-ChildList square)
		      (if (equal (list(nth 0 point)(nth 1 point))
				 (extract-coor-of-punkt-recorg x)
				 square/2)
			(progn
			  (if (= filter-mode 1)
			    (progn
			      (draw-punkt-by-record x type-list)
			      (setq nnn (1+ nnn))
			      )
			    (if (check-draw-record-punkt-by-filter x filter-list)
			      (progn
				(draw-punkt-by-record x type-list)
				(setq nnn (1+ nnn))
				)
			      )
			    )
			  )
			);if
		      );foreach
		    (vlax-release-object square)
		    )
		  );if
		)
	      );progn
	    (progn
	      (setq squares(XML-Get-Children(XML-Get-Child (XML-Get-Child o-doc nil "puncts") nil "squares")nil))	      
	      (setq summ 0)
	      (setq n 0)
	      (setq type-list(get-punct-type-list-in-katalog o-doc))
	      (add-mggt-punct-types-in-drawning)
	      (while (setq square (vlax-get-property squares 'item n))
		(setq collection-square(XML-Get-Children square nil))
		(setq list-square-coll (append list-square-coll (list collection-square)))
		(setq summ (+ summ (vlax-get-property collection-square 'length)))
		(setq n (1+ n))
		)
	      (setq n 1)
	      (foreach collection list-square-coll
		(setq nn 0)
		(while (setq punkt-recorg (vlax-get-property collection 'item nn))
		  (if (=(rem nnn 100)0)(grtext -1 (strcat(itoa nnn)"/"(itoa summ))))
		  (if (= filter-mode 1)
		    (progn
		      (draw-punkt-by-record punkt-recorg type-list)
		      
		      )
		    (if (check-draw-record-punkt-by-filter punkt-recorg filter-list)
		      (progn
			(draw-punkt-by-record punkt-recorg type-list)
			
			)
		      )
		    )
		  (setq nn (1+ nn))
		  (setq nnn (1+ nnn))
		  (vlax-release-object punkt-recorg)
		  )
		(setq n (1+ n))
		(vlax-release-object collection)
		)
	      )
	    )
	  (vlax-release-object squares)
	  (princ (strcat "\nНанесено пунктов: " (itoa nnn)))
	  (grtext -1 "")
	  )
	)
      
      (vlax-release-object o-doc)
      )
    (alert "Файл каталога координат пунктов не обнаружен!")
    )
  (princ)
  )

(defun analisis-point-square (point / )
  (strcat (itoa(analisis-number-square(nth 0 point)))
	  " "
	  (itoa(analisis-number-square(nth 1 point)))
	  )
  )

(defun analisis-number-square (number / )
  ;определение принадлежности числа квадрату стороной 10км
  (setq square 5000)
  (setq x (atoi(rtos(/ number square)2 0)))
  (/
    (if (minusp number)
      (if (even-odd x)
	(-(* square x)square)
	(* square x)
	)
      (if (even-odd x)
	(+(* square x)square)
	(* square x)
	)
      )
    1000)
  )

(defun extract-coor-of-punkt-recorg (punkt-recorg / )
  (list (atof (XML-Get-Attribute punkt-recorg "E" ""))
	(atof (XML-Get-Attribute punkt-recorg "N" ""))
	)
  )

(defun draw-punkt-by-record (punkt-recorg type-list / ASS-LIST BLOCK-NAME COLOR ATT-LIST)
  (setq ass-list (convert-punkt-recorg-to-ass-list punkt-recorg))
  (if (not(setq block-name(cadr(assoc(cadr(assoc"type"ass-list))type-list))))
    (setq block-name"МГГТ тип знака не определен"))
  (setq color
	 (cond
	   ((cadr(assoc"year"ass-list))1)
	   ((cadr(assoc"Z"ass-list))82)
	   (T 5)
	   )
	)
  (foreach x ass-list
    (setq att-list
	   (append att-list
		   (list
		     (cond
		       ((= (car x) "N")(cons "N" (strcat "N=" (cadr x))))
		       ((= (car x) "E")(cons "E" (strcat "E=" (cadr x))))
		       ((= (car x) "Z")(cons "Z" (strcat "Z=" (cadr x))))
		       ((= (car x) "year")(cons "year" (strcat  (cadr x)"г.")))
		       (T (cons (car x)(cadr x)))
		       )
		     )
		   ))
    )
  
  (edit-block-set-attributes-by-list
    (draw-insertblock-vla-by-list-property
      (list(cons "spece" model_spece)
	   (cons "InsertionPoint" (extract-coor-of-punkt-recorg punkt-recorg))
	   (cons "EffectiveName" block-name)
	   (cons "XScaleFactor" (/ 1.0 (getvar "CANNOSCALEVALUE")))
	   (cons "YScaleFactor" (/ 1.0 (getvar "CANNOSCALEVALUE")))
	   (cons "ZScaleFactor" (/ 1.0 (getvar "CANNOSCALEVALUE")))
	   (cons "Rotation" 0.0)
	   (cons "Color" color)
	   ))
    att-list
    )
  (princ)
  )