(defun C:profile_add_intersection_line_be_point_in_plan ( /
							 block-obj block-profile-obj buffer-zone curent-profile
							 elevation horizontal-scale insert-point
							 lightweightpolyline point reference-level
							 scale-coefficient start-point trace-2dpline vertical-scale
							 curent-text-style h-text intersection-str profile-parameters
							 profile-string-list profile-mode plan-point)
  (load_global_variable)
  (geo_tools-help "profile_add_intersection_line_be_point_in_plan")
  (setq profile-mode (vlax-ldata-get "geo_tools_dictionary" "profile-mode"))
  (if(if (not profile-mode)(profile-seleckt-curent-mode)T)
    (if (=(vlax-ldata-get "geo_tools_dictionary" "profile-mode")1)
      (progn
	(setq curent-profile(vlax-ldata-get "geo_tools_dictionary" "curent-profile"))
	(if curent-profile
	  (if (vlax-erased-p (cdr curent-profile))
	    (alert "Ошибка! Текущий профиль был удален.\nВыбрать другой текущий профиль можно с помощью команды \"Выбрать текущий профиль\"")
	    (progn
	      (princ (strcat"\nТекущий профиль: "(car curent-profile)))
	      (if (setq point (getpoint "\nУкажи точку на плане: "))
		(progn
		  (setq block-profile-obj (cdr curent-profile))
		  (setq trace-2Dpline (trace-create-2Dpline-by-block-profile block-profile-obj))
		  (setq block-obj (cdr(assoc "block"(vlax-ldata-get block-profile-obj"obj-list"))))
		  (setq vertical-scale (vlax-ldata-get block-profile-obj"vertical-scale"))
		  (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
		  (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
		  (setq reference-level (vlax-ldata-get block-profile-obj"reference-level"))
		  
		  (setq profile-string-list(vlax-ldata-get block-profile-obj"profile-string-list"))
		  (setq profile-parameters(vlax-ldata-get block-profile-obj"profile-parameters"))
		  
		  (setq buffer-zone(*(atof(nth 3 (vlax-ldata-get block-profile-obj"profile-parameters")))scale-coefficient))
		  (setq start-point(mapcar'+ (vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj)))(list buffer-zone 0)))
		  
		  (setq curent-text-style (nth 1 profile-parameters))
		  (if(or(eq curent-text-style "***Текущий***")(not(member curent-text-style(get-text-style-list))))
		    (setq curent-text-style(getvar "TEXTSTYLE")))
		  
		  (setq h-text (*(atof(nth 9 profile-parameters))scale-coefficient))
		  (setq plan-point (vlax-curve-getDistAtPoint trace-2Dpline (vlax-curve-getClosestPointTo trace-2Dpline (list (car point)(cadr point)0))))
		  (vla-erase trace-2Dpline)
		  (setq intersection-str (get-string-or-text))
		  (if (setq elevation (getreal"\nВведи отметку пересечки<без отметки>: "))
		    (progn
		      (vla-StartUndoMark active_document)
		      (setq insert-point
			     (mapcar '+ start-point
				     (list
				       plan-point
				       (/(- elevation reference-level)(/ (* vertical-scale (/ 1000.0 horizontal-scale)) 1000.0))
				       )))
		      (princ (strcat"\nОтметка определена как "(rtos elevation 2 3)))
		      
		      
		      (setq LightWeightPolyline (vla-addLightWeightPolyline model_spece
						  (convert-list-points-to-lwpolyline-safearray (list (mapcar '+ insert-point (list (* -1.25 scale-coefficient) (* -2.165 scale-coefficient)))
												     insert-point
												     (mapcar '+ insert-point (list (* 1.25 scale-coefficient) (* -2.165 scale-coefficient)))
												     ))))
		      (vla-put-Linetype LightWeightPolyline "Continuous")
		      (vla-put-Closed LightWeightPolyline -1)
		      (draw-line-vla model_spece insert-point (list (nth 0 insert-point) (nth 1 start-point)))
		      (if intersection-str
			(draw-text-vla-by-list-property(list
							 (cons "spece" model_spece)
							 (cons "TextString" intersection-str)
							 (cons "InsertionPoint" (list (nth 0 insert-point) (+(nth 1 start-point)(/ h-text 2))))
							 (cons "Height" h-text)
							 (cons "Rotation" (/ pi 2))
							 (cons "Alignment" 12)
							 (cons "StyleName" curent-text-style)
							 ))
			)
		      (vla-EndUndoMark active_document)
		      )
		    (progn
		      (vla-StartUndoMark active_document)
		      (setq insert-point (list(+(nth 0 start-point)plan-point)(nth 1 start-point)))
		      (draw-line-vla model_spece insert-point (mapcar '+ insert-point (list 0 100)))
		      (if intersection-str
			(draw-text-vla-by-list-property(list
							 (cons "spece" model_spece)
							 (cons "TextString" intersection-str)
							 (cons "InsertionPoint" (list (nth 0 insert-point) (+(nth 1 start-point)(/ h-text 2))))
							 (cons "Height" h-text)
							 (cons "Rotation" (/ pi 2))
							 (cons "Alignment" 12)
							 (cons "StyleName" curent-text-style)
							 ))
			)
		      (vla-EndUndoMark active_document)
		      )
		    )
		  
		  )
		)
	      )
	    )
	  (alert "Ошибка! Текущий профиль не определен.\nВыбрать текущий профиль можно с помощью команды \"Выбрать текущий профиль\"")
	  )
	)
      (alert "Данная команда не работает в режиме \"старый профиль\"!")
      )
    )
  (princ)
  )

(defun get-string-or-text(/ flag obj string)
  (load_global_variable)
  (setq flag T)
  (while flag
    (setq obj (car(entsel "\nУкажи объект типа текст<ввести>: ")))
    (cond
      ((eq obj nil)
       (setq string (getstring t "\nВведи текст пересечки: "))
       (setq flag nil)
       )
      ((eq (cdr(assoc 0 (entget obj))) "TEXT")
       (setq string(cdr(assoc 1(entget obj))))
       (setq flag nil)
       )
      (T
       (alert "Ошибка! Выбран не объект не допустимого типа. Допускаются однострочный текст и точки COGO. Попробуй снова.")
       )
      )
    )
  string
  )