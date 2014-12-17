(defun add-in-blocks-mggt-attributs ( / Blocks)
  (setq Blocks(vla-get-Blocks active_document))
  (foreach x (list
	       "МГГТ вековой репер"
	       "МГГТ грунтовые знаки"
	       "МГГТ местный предмет"
	       "МГГТ надстройка"
	       "МГГТ пирамида"
	       "МГГТ репер стенной"
	       "МГГТ сигнал"
	       "МГГТ стенная марка"
	       "МГГТ стенной знак штанга"
	       "МГГТ тип знака не определен"
	       )
    (add-attributs-to-mggt-punkt (vla-Item Blocks x))
    )
  )
(defun add-blocks-mggt ( / lwpolyline block)
  (setq Blocks(vla-get-Blocks active_document))
  (if(vl-catch-all-error-p(vl-catch-all-apply 'vla-Item(list Blocks"МГГТ репер стенной")))
    (progn
      (setq block (vla-Add Blocks (vlax-3D-point '(0 0 0)) "МГГТ репер стенной"))
      (add-hatch-to-closed-obj
	block
	(setq lwpolyline
	       (draw-lwpolyline-vla-by-list-property
		 (list(cons "spece" block)
		      (cons "Coordinates" (list '(0 1.5 0)'(1.299 -0.750 0)'(-1.299 -0.750 0)))
		      (cons "Closed" 1)
		      (cons "Layer" "0")
		      (cons "Color" 0)
		      (cons "Lineweight" -2)
		      (cons "Linetype" "Continuous")
		      )))
	(list
	  (cons "Color" 0)
	  (cons "Layer" "0")
	  (cons "Lineweight" -2)
	  (cons "Linetype" "Continuous")
	  )
	)
      (add-attributs-to-mggt-punkt block)
      (vla-Delete lwpolyline)
      ))
  (if(vl-catch-all-error-p(vl-catch-all-apply 'vla-Item(list Blocks"МГГТ пирамида")))
    (progn
      (setq block (vla-Add Blocks (vlax-3D-point '(0 0 0)) "МГГТ пирамида"))
      (add-hatch-to-closed-obj
	block
	(setq lwpolyline
	       (draw-lwpolyline-vla-by-list-property
		 (list(cons "spece" block)
		      (cons "Coordinates" (list '(0 3.0 0)'(2.598 -1.500 0)'(-2.598 -1.500 0)))
		      (cons "Closed" 1)
		      (cons "Layer" "0")
		      (cons "Color" 0)
		      (cons "Lineweight" -2)
		      (cons "Linetype" "Continuous")
		      )))
	(list
	  (cons "Color" 0)
	  (cons "Layer" "0")
	  (cons "Lineweight" -2)
	  (cons "Linetype" "Continuous")
	  )
	)
      (add-attributs-to-mggt-punkt block)
      (vla-Delete lwpolyline)
      ))
  (if(vl-catch-all-error-p(vl-catch-all-apply 'vla-Item(list Blocks"МГГТ надстройка")))
    (progn
      (setq block (vla-Add Blocks (vlax-3D-point '(0 0 0)) "МГГТ надстройка"))
      (add-hatch-to-closed-obj
	block
	(setq lwpolyline
	       (draw-lwpolyline-vla-by-list-property
		 (list(cons "spece" block)
		      (cons "Coordinates" (list '(2.25 -2.25 0)'(-2.25 -2.25 0)'(-2.25 2.25 0)'(2.25 2.25 0)))
		      (cons "Closed" 1)
		      (cons "Layer" "0")
		      (cons "Color" 0)
		      (cons "Lineweight" -2)
		      (cons "Linetype" "Continuous")
		      )))
	(list
	  (cons "Color" 0)
	  (cons "Layer" "0")
	  (cons "Lineweight" -2)
	  (cons "Linetype" "Continuous")
	  )
	)
      (add-attributs-to-mggt-punkt block)
      (vla-Delete lwpolyline)
      ))
  (if(vl-catch-all-error-p(vl-catch-all-apply 'vla-Item(list Blocks"МГГТ сигнал")))
    (progn
      (setq block (vla-Add Blocks (vlax-3D-point '(0 0 0)) "МГГТ сигнал"))
      (add-hatch-to-closed-obj
	block
	(setq lwpolyline
	       (draw-lwpolyline-vla-by-list-property
		 (list(cons "spece" block)
		      (cons "Coordinates" (list '(0 0.75 0)'(1.5 -1.50 0)'(-1.5 -1.5 0)))
		      (cons "Closed" 1)
		      (cons "Layer" "0")
		      (cons "Color" 0)
		      (cons "Lineweight" -2)
		      (cons "Linetype" "Continuous")
		      )))
	(list
	  (cons "Color" 0)
	  (cons "Layer" "0")
	  (cons "Lineweight" -2)
	  (cons "Linetype" "Continuous")
	  ))
      (draw-lwpolyline-vla-by-list-property
	(list(cons "spece" block)
	     (cons "Coordinates" (list '(1.5 -1.5 0)'(-1.5 -1.5 0)'(-1.5 1.5 0)'(1.5 1.5 0)))
	     (cons "Closed" 1)
	     (cons "Layer" "0")
	     (cons "Color" 0)
	     (cons "Lineweight" -2)
	     (cons "Linetype" "Continuous")
	     ))
      (add-attributs-to-mggt-punkt block)
      (vla-Delete lwpolyline)
      ))
  (if(vl-catch-all-error-p(vl-catch-all-apply 'vla-Item(list Blocks"МГГТ сигнал")))
    (progn
      (setq block (vla-Add Blocks (vlax-3D-point '(0 0 0)) "МГГТ сигнал"))
      (add-hatch-to-closed-obj
	block
	(setq lwpolyline
	       (draw-lwpolyline-vla-by-list-property
		 (list(cons "spece" block)
		      (cons "Coordinates" (list '(0 0.75 0)'(1.5 -1.50 0)'(-1.5 -1.5 0)))
		      (cons "Closed" 1)
		      (cons "Layer" "0")
		      (cons "Color" 0)
		      (cons "Lineweight" -2)
		      (cons "Linetype" "Continuous")
		      )))
	(list
	  (cons "Color" 0)
	  (cons "Layer" "0")
	  (cons "Lineweight" -2)
	  (cons "Linetype" "Continuous")
	  ))
      (draw-lwpolyline-vla-by-list-property
	(list(cons "spece" block)
	     (cons "Coordinates" (list '(1.5 -1.5 0)'(-1.5 -1.5 0)'(-1.5 1.5 0)'(1.5 1.5 0)))
	     (cons "Closed" 1)
	     (cons "Layer" "0")
	     (cons "Color" 0)
	     (cons "Lineweight" -2)
	     (cons "Linetype" "Continuous")
	     ))
      (add-attributs-to-mggt-punkt block)
      (vla-Delete lwpolyline)
      ))
  )

(defun add-attributs-to-mggt-punkt (block /)
  (foreach x (list (list "NAME" (list 2.5 3 0) "Имя, номер знака")
		   (list "TYPE" (list 2.5 0 0) "Тип знака")
		   (list "N" (list 2.5 -3 0)"Северная координата")
		   (list "E" (list 2.5 -6 0)"Восточная координата")
		   (list "Z" (list 2.5 -9 0)"Отметка")
		   (list "ADDRESS" (list 2.5 -12 0)"Местоположение, адрес")
		   (list "COMMENT" (list 2.5 -15 0)"Коментарий")
		   (list "YEAR" (list 2.5 -18 0)"Год получения данных"))
    (draw-attribute-vla-by-list-property
      (list(cons "spece" block)
	   (cons "Height" 2.0)
	   (cons "Layer" "0")
	   (cons "Color" 0)
	   (cons "Lineweight" -2)
	   (cons "Mode" acAttributeModeNormal)
	   (cons "Linetype" "Continuous")
	   (cons "PromptString" (caddr x))
	   (cons "InsertionPoint" (cadr x))
	   (cons "TagString" (car x))
	   (cons "TextString" "")
	   (cons "Alignment" 9)
	   (cons "StyleName" "OTI")
	   )))
  )

(defun add-hatch-to-closed-obj (spece closed-obj list-property / hatch)
  ;добавляет штриховку к ЗАМКНОТУМУ объекту (иначе ошибка)
  (setq hatch(vla-addHatch
	       spece
	       1
	       "solid"
	       :vlax-false
	       AcHatchObject
	       )
	)
  (vla-appendOuterLoop hatch
    (vlax-safearray-fill (vlax-make-safearray vlax-vbObject '(0 . 0))
      (list closed-obj)))
  (foreach x list-property
    (if (vlax-property-available-p hatch (car x) T)
      (eval(list(read(strcat "vla-put-" (car x)))hatch(cdr x)))
      )
    )
  (vla-update hatch)
  hatch
  )


;;;  (entget(vlax-vla-object->ename block))
;;;  (entget(vlax-vla-object->ename (vla-Item(vla-get-Blocks active_document)"Съемочная точка Долговременная")))
;;;  (vlax-dump-object(vla-Item(vla-get-Blocks active_document)"Съемочная точка Долговременная")T)