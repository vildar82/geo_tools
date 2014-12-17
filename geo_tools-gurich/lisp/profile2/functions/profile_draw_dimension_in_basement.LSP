(defun C:profile_draw_dimension_in_basement ( / block-profile-obj curent-profile
					     list-select-string pline-obj
					     profile-string-list select-string-point
					     flag profile-mode)
  ;чертит одно измерение на профиль
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
	(if(setq p1 (getpoint "\nУкажи первую точку:"))
	  (progn
	    (if (= profile-mode 1)
	      (if (<(car(setq list-select-string(profile-calculation-string-by-point block-profile-obj p1)))0)
		(setq list-select-string nil)
		)
	      )
	    (if (and list-select-string (eq (nth 1(nth(car list-select-string)profile-string-list)) "deep"))
	      (setq p2 T)
	      (setq p2 (getpoint p1 "\nУкажи вторую точку:"))
	      )
	    (if p2
	      (progn
		(if (= profile-mode 1)
		  (if (not list-select-string)
		    (progn
		      (draw_osnap_off)
		      (setq select-string-point(getpoint"\nУкажи заполняемую строку подвала: "))
		      (draw_osnap_on)
		      (setq list-select-string(profile-calculation-string-by-point block-profile-obj select-string-point))
		      ))
		  (progn
		    (initget "Расстояния Глубина Длинна")
		    (setq list-select-string (getkword"\n[Расстояния/Глубина заложения/Длинна\\уклон]<Выход>: "))
		    )
		  )
		(if list-select-string
		  (progn
		    (vla-StartUndoMark active_document)
		    (cond
		      ((if(= profile-mode 1)(eq (nth 1(nth(car list-select-string)profile-string-list)) "distance")(= list-select-string "Расстояния"))
		       ;;;		    (princ "\nСтрока определена как РАССТОЯНИЯ.")
		       (profile-calculation-and-draw-length-by-pline block-profile-obj (list p1 p2) list-select-string)
		       )
		      ((if(= profile-mode 1)(eq (nth 1(nth(car list-select-string)profile-string-list)) "dist-downgrade")(= list-select-string "Длинна"))
		       ;;;		    (princ "\nСтрока определена как РАССТОЯНИЯ И УКЛОНЫ.")
		       (profile-calculation-and-draw-length-and-downgrade-by-pline block-profile-obj (list p1 p2) list-select-string)
		       )
		      ((if(= profile-mode 1)(eq (nth 1(nth(car list-select-string)profile-string-list)) "deep")(= list-select-string "Глубина"))
		       ;;;		    (princ "\nСтрока определена как ГЛУБИНА ЗАЛОЖЕНИЯ.")
		       (profile-calculation-and-draw-deep-by-pline block-profile-obj (list p1 p2) list-select-string)
		       )
		      (T
		       (alert "Ошибка! Данная строка подвала не подразумевает автоматического заполнения данной командой!")
		       )
		      )
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
    )
  (princ)
  )