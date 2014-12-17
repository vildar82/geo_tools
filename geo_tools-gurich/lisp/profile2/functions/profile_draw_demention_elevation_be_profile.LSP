(defun C:profile_draw_demention_elevation_be_profile (/ accuracy accuracy-mleader block-profile-obj
						      curent-profile curent-text-style dimension-point
						      down-y-coor-in-curent-string elivation flag height-text
						      horizontal-scale insert-point list-select-string list_points
						      mleader_obj model_spece number-string paper_spece profile-mode
						      profile-string-list reference-level scale-coefficient
						      start-point up-y-coor-in-curent-string vertical-scale)
  ; снятие отметки с профиля
  (load_global_variable)
  (geo_tools-help "profile_draw_demention_elevation_be_profile")
  (setq profile-mode (vlax-ldata-get "geo_tools_dictionary" "profile-mode"))
  (if(if(not profile-mode)(profile-seleckt-curent-mode)T)
    (progn
      (if(= profile-mode 1)
	(progn
	  (setq curent-profile(vlax-ldata-get "geo_tools_dictionary" "curent-profile"))
	  (setq flag (profile-check-curent-profile curent-profile))
	  (if flag
	    (progn
	      (princ(strcat"\nТекущий профиль: "(car curent-profile)))
	      (setq block-profile-obj (cdr curent-profile))
	      (setq vertical-scale (vlax-ldata-get block-profile-obj"vertical-scale"))
	      (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
	      (setq reference-level (vlax-ldata-get block-profile-obj"reference-level"))
	      (setq start-point(vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj))))
	      (setq curent-text-style (nth 1 (vlax-ldata-get block-profile-obj"profile-parameters")))
	      (if(or(eq curent-text-style "***Текущий***")(not(member curent-text-style(get-text-style-list))))
		(setq curent-text-style(getvar "TEXTSTYLE")))
	      (setq scale-coefficient (/ 1000.0 horizontal-scale))
	      (if(not(setq accuracy-mleader (vlax-ldata-get block-profile-obj"accuracy-mleader")))(setq accuracy-mleader 2))
	      ))
	  )
	(progn
	  (setq horizontal-scale (cadr(assoc "horizontal-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	  (setq vertical-scale (cadr(assoc "vertical-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	  (setq reference-level (cadr(assoc "reference-level" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	  (setq height-text (cadr(assoc "height-text" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	  (setq accuracy (cadr(assoc "accuracy" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	  (setq accuracy-mleader accuracy)
	  (setq curent-text-style(getvar "TEXTSTYLE"))
	  (princ(strcat
		  "\nМасштабВ 1:" (itoa horizontal-scale)
		  " МасштабГ 1:" (itoa vertical-scale)
		  " Отметка условного горизонта: " (rtos reference-level 2 2) "м"
		  ))
	  (setq scale-coefficient (/ 1000.0 horizontal-scale))
	  (setq flag T))
	)
      (if
	flag
	(progn
	  (if(= profile-mode 0)(setq start-point(profile-get-last-select-point)))
	  (while
	    (setq dimension-point (getpoint "\nУкажи измеряемую точку на профиле: "))
	    (setq elivation (+(*(- (nth 1 dimension-point) (nth 1 start-point))(/ (* vertical-scale (/ 1000.0 horizontal-scale)) 1000.0))reference-level))
	    (initget"Выбрать Пропустить")
	    (setq insert-point(getpoint
				(strcat "\nОтметка = "
					(rtos elivation 2 3)
					" укажи куда будем вставлять текст [Выбрать текст/Пропустить]<Пропустить>: ")))
	    (cond
	      ((= insert-point "Выбрать")
	       (setq text-obj(car(entsel "\nВыбери текст куда вставлять отметку<Пропустить>: ")))
	       (if text-obj
		 (progn
		   (setq entget-text (entget text-obj))
		   (if (= profile-mode 1)
		     (progn
		       (setq list-select-string(profile-calculation-string-by-point block-profile-obj
						 (cdr(assoc 10 entget-text))))
		       (setq number-string(nth 0 list-select-string))
		       (setq profile-string-list(vlax-ldata-get block-profile-obj"profile-string-list"))
		       (setq list-select-string(nth number-string profile-string-list))
		       (setq accuracy (atoi(nth 4 list-select-string)))
		       (if (not(eq (nth 1 list-select-string) "elevations"))
			 (progn(alert "Указанная строка не может содержать отметок.")(setq flag nil)))
		       )
		     )
		   (if flag
		     (progn
		       (entmod (list (cons -1 text-obj) (cons 1 (rtos elivation 2 accuracy))))
		       (entupd text-obj))
		     )
		   )
		 )
	       )
	      ((= (type insert-point) 'list)
	       (if
		 (< (nth 1 insert-point)(nth 1 start-point))
		 (progn
		   (if (= profile-mode 0)
		     (progn
		       (setq down-y-coor-in-curent-string (nth 1 insert-point))
		       (setq up-y-coor-in-curent-string(cadr(getpoint "\nУкажи точку на верхней линии строки: ")))
		       )
		     (progn
		       (setq list-select-string(profile-calculation-string-by-point block-profile-obj insert-point))
		       (setq up-y-coor-in-curent-string (nth 1 list-select-string))
		       (setq down-y-coor-in-curent-string(nth 2 list-select-string))
		       (setq number-string(nth 0 list-select-string))
		       (setq profile-string-list(vlax-ldata-get block-profile-obj"profile-string-list"))
		       (setq list-select-string(nth number-string profile-string-list))
		       (setq height-text (atof(nth 6 list-select-string)))
		       (setq accuracy (atoi(nth 4 list-select-string)))
		       (if (not(eq (nth 1 list-select-string) "elevations"))
			 (progn(alert "Указанная строка не может содержать отметок.")(setq flag nil)))
		       )
		     )
		   (if flag
		     (progn
		       (entmake (list '(0 . "TEXT") (cons 10 dimension-point)
				      '(72 . 1) '(73 . 1)
				      (cons 11 (list
						 (nth 0 dimension-point)
						 (+ down-y-coor-in-curent-string
						    (/(- up-y-coor-in-curent-string down-y-coor-in-curent-string )2))
						 0))
				      (cons 40 (/ height-text scale-coefficient))
				      (cons 50 (/ pi 2))
				      (cons 1 (rtos elivation 2 accuracy))
				      (cons 7 curent-text-style)))
		       (entmake (list '(0 . "LINE") '(6 . "Continuous")
				      (cons 10 (list (nth 0 dimension-point)down-y-coor-in-curent-string 0))
				      (cons 11 (list (nth 0 dimension-point)up-y-coor-in-curent-string 0))))
		       )
		     )
		   (setq flag T)
		   )
		 (progn
		   (if (= profile-mode 1)
		     (progn
		       (setq height-text (atof(nth 9(vlax-ldata-get block-profile-obj"profile-parameters"))))
		       (setq scale-coefficient (/ 1000.0 horizontal-scale))
		       )
		     )
		   (if
		     (< (nth 1 insert-point)(nth 1 dimension-point))
		     (if (< (distance (list 0(nth 1 insert-point))(list 0(nth 1 dimension-point))) (/ 4.0 scale-coefficient))
		       (setq list_points (list dimension-point(mapcar '- dimension-point (list 0 (/ 6.0 scale-coefficient)0))))
		       (setq list_points (list dimension-point(mapcar '- dimension-point (list 0 (distance (list 0(nth 1 insert-point))(list 0(nth 1 dimension-point)))0))))
		       ); вниз
		     (if (< (distance (list 0(nth 1 insert-point))(list 0(nth 1 dimension-point))) (/ 4.0 scale-coefficient))
		       (setq list_points (list dimension-point(mapcar '+ dimension-point (list 0 (/ 4.0 scale-coefficient)0))))
		       (setq list_points (list dimension-point(mapcar '+ dimension-point (list 0 (distance (list 0(nth 1 insert-point))(list 0(nth 1 dimension-point)))0))))
		       ); вверх
		     )
		   (setq mleader_obj
			  (draw-mleader-vla-by-list-property
			    (list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
				 (cons "LeaderLineCoor" list_points)
				 (cons "StyleName" "Подписи")
				 (cons "TextHeight"(/ height-text scale-coefficient))
				 (cons "TextString" (rtos elivation 2 (if (=(type accuracy-mleader)'STR) (atoi accuracy-mleader)accuracy-mleader)))
				 (cons "ArrowheadType" 9)
				 (cons "ArrowheadSize" (/ 2.0 scale-coefficient))
				 (cons "ArrowheadBlock" "_Open90")
				 ))
			 )
		   (if
		     (if (=(get-acad-ver)"2008")
		       (> (nth 0 insert-point)(nth 0 dimension-point))
		       (< (nth 0 insert-point)(nth 0 dimension-point)))
		     (progn
		       (vla-Mirror mleader_obj (vlax-3D-point dimension-point)(vlax-3D-point (list (nth 0 dimension-point) 1 0)))
		       (vla-Delete mleader_obj)
		       )
		     )
		   )
		 )
	       )
	      )
	    )
	  )
	)
      )
    )
  (princ)
  )