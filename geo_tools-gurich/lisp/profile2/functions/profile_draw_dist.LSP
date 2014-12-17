(defun C:profile_draw_dist ( / block-profile-obj curent-profile
					       list-select-string pline-obj
					       profile-string-list select-string-point
					       flag profile-mode)
  
  (load_global_variable)
  (geo_tools-help "profile_calculation_line_in_basement")
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
	      (setq profile-string-list(vlax-ldata-get block-profile-obj"profile-string-list"))
	      ))
	  )
	(progn
	  (princ(strcat
		  "\nМасштабВ 1:" (itoa(cadr(assoc "vertical-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
		  " МасштабГ 1:" (itoa(cadr(assoc "horizontal-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
		  " Отметка условного горизонта: " (rtos (cadr(assoc "reference-level" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))) 2 2) "м"
		  ))
	  (setq flag T))
	)
      (if
	flag
	(progn
	  (setq pline-obj(car(entsel"\nВыбери полилинию на профиле: ")))
	  (cond
	    ((eq pline-obj nil)
	     (alert "Ничего не выбрано!"))
	    ((not(eq(cdr(assoc 0(entget pline-obj)))"LWPOLYLINE"))
	     (alert "Выбрана не 2D полилиния!"))
	    ((eq(cdr(assoc 0(entget pline-obj)))"LWPOLYLINE")
	     (if (= profile-mode 1)
	       (progn
		 (draw_osnap_off)
		 (setq select-string-point(getpoint"\nУкажи заполняемую строку подвала: "))
		 (draw_osnap_on)
		 (setq list-select-string(profile-calculation-string-by-point block-profile-obj select-string-point))
		 )
	       (progn
		 (setq list-select-string (list nil nil nil))
		 )
	       )
	     (if list-select-string
	       (progn
		 (vla-StartUndoMark active_document)
		 (profile-calculation-and-draw-dist block-profile-obj pline-obj list-select-string)
		 (vla-EndUndoMark active_document)
		 )
	       (if(= profile-mode 1)(alert "Ошибка! Указаная точка не попадает в подвал текущего профиля!"))
	       )
	     )
	    )
	  )
	)
      )
    )
  (princ)
  )

(defun profile-calculation-and-draw-dist (block-profile-obj 2Dpline list-select-string /
					  accuracy curent-text-style
					  up-y-coor-in-curent-string
					  down-y-coor-in-curent-string
					  height-text horizontal-scale list-picket
					  list-vertex number-string point profile-parameters profile-string-list
					  reference-level scale-coefficient start-point vertical-scale picket-first flag
					  buffer-zone
					  )
  ;чертит пикеты на профиле
  (if(=(vlax-ldata-get "geo_tools_dictionary" "profile-mode")1)
    (progn
      (setq up-y-coor-in-curent-string (nth 1 list-select-string))
      (setq down-y-coor-in-curent-string(nth 2 list-select-string))
      (setq number-string(nth 0 list-select-string))
      (setq profile-string-list(vlax-ldata-get block-profile-obj"profile-string-list"))
      (setq list-select-string(nth number-string profile-string-list))
      (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
      (setq vertical-scale (vlax-ldata-get block-profile-obj"vertical-scale"))
      (setq reference-level (vlax-ldata-get block-profile-obj"reference-level"))
      (setq profile-parameters(vlax-ldata-get block-profile-obj"profile-parameters"))
      (setq picket-first(vlax-ldata-get block-profile-obj"picket-first"))
      (setq buffer-zone(*(atof(nth 3 (vlax-ldata-get block-profile-obj"profile-parameters")))(/ 1.0 (/ 1000.0 horizontal-scale))))
      (setq start-point(mapcar'+ (vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj)))(list buffer-zone 0)))
      (setq curent-text-style (nth 1 profile-parameters))
      (if(or(eq curent-text-style "***Текущий***")(not(member curent-text-style(get-text-style-list))))
	(setq curent-text-style(getvar "TEXTSTYLE")))
      (setq height-text (*(atof(nth 6 list-select-string))(/ 1.0 (/ 1000.0 horizontal-scale))))
      (setq accuracy (atoi(nth 4 list-select-string)))
      (setq flag T)
      )
    (progn
      (if(setq start-point(getpoint"\nУкажи точку начального пикета на условном горизонте: "))
	(if(setq down-y-coor-in-curent-string(cadr(getpoint"\nУкажи точку на нижней линии строки: ")))
	  (if(setq up-y-coor-in-curent-string (cadr(getpoint "\nУкажи точку на верхней линии строки: ")))
	    (progn
	      (setq horizontal-scale (cadr(assoc "horizontal-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	      (setq vertical-scale (cadr(assoc "vertical-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	      (setq reference-level (cadr(assoc "reference-level" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	      (setq picket-first (cadr(assoc "picket-first" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	      (setq curent-text-style(getvar "TEXTSTYLE"))
	      (setq height-text (*
				  (cadr(assoc "height-text" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list")))
				  (/ 1.0 (/ 1000.0 horizontal-scale))))
	      (setq accuracy (cadr(assoc "accuracy" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	      (setq flag T)
	      )
	    )))
      )
    )
  (if flag
    (progn
      (if (not height-text)
	(setq height-text (*(cadr(assoc "height-text" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list")))
			    (/ 1.0 (/ 1000.0 horizontal-scale))))
	)
      (if (not accuracy)
	(setq accuracy (cadr(assoc "accuracy" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	)
      (setq scale-coefficient (/ (* vertical-scale (/ 1000.0 horizontal-scale))1000.00))
      (setq list-vertex (extract_coord_lwpolyline (entget 2Dpline)))
      (repeat (length list-vertex)
	(setq point (car list-vertex))
	(setq list-vertex (cdr list-vertex))
	(setq list-picket(append list-picket(list (list (rtos (/(+(-(nth 0 point) (nth 0 start-point))picket-first)1000.0) 2 accuracy) point))))
	)
      
      (profile-draw-texts-by-list-limits
	list-picket
	(profile-analisis-limits (apply 'list (mapcar 'cadr list-picket))horizontal-scale)
	down-y-coor-in-curent-string
	up-y-coor-in-curent-string
	height-text
	accuracy
	horizontal-scale
	vertical-scale
	curent-text-style)
      ))
  )