(defun C:profile_draw_point_to_plan ( /
				     block-obj block-profile-obj buffer-zone comm curent-profile elivation
				     height-text horizontal-scale   plan-point profil-point profile-mode
				     reference-level scale-coefficient start-point trace-2dpline vertical-scale)
  ; найти точку на плане
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
	      (if
		(setq profil-point (getpoint  "\nУкажи точку в профиле: "))
		(progn
		  (setq block-profile-obj (cdr curent-profile))
		  (setq trace-2Dpline (trace-create-2Dpline-by-block-profile block-profile-obj))
		  (setq block-obj (cdr(assoc "block"(vlax-ldata-get block-profile-obj"obj-list"))))
		  (setq vertical-scale (vlax-ldata-get block-profile-obj"vertical-scale"))
		  (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
		  (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
		  (setq height-text (*(atof(nth 9 (vlax-ldata-get block-profile-obj"profile-parameters")))scale-coefficient))
		  (setq reference-level (vlax-ldata-get block-profile-obj"reference-level"))
		  (setq buffer-zone(*(atof(nth 3 (vlax-ldata-get block-profile-obj"profile-parameters")))scale-coefficient))
		  (setq start-point(mapcar'+ (vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj)))(list buffer-zone 0)))
		  (setq plan-point (vlax-curve-getPointAtParam trace-2Dpline(vlax-curve-getParamAtDist trace-2Dpline (-(nth 0 profil-point)(nth 0 start-point)))))
		  (vla-erase trace-2Dpline)
		  (setq elivation (+(*(- (nth 1 profil-point) (nth 1 start-point))(/ (* vertical-scale (/ 1000.0 horizontal-scale)) 1000.0))reference-level))
		  (initget"Да Нет")
		  (setq comm (getpoint plan-point (strcat"\nОтметка: "(rtos elivation 2 3)" вставить в виде мультивыноски [Да/Нет]<Нет>: ")))
		  (if (= comm "Да")
		    (draw-mleader-vla-by-list-property
		      (list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
			   (cons "LeaderLineCoor" (list plan-point (mapcar '+ plan-point (list (/ 10.0 (getvar "CANNOSCALEVALUE")) (/ 10.0 (getvar "CANNOSCALEVALUE"))0))))
			   (cons "StyleName" "Подписи")
			   (cons "TextHeight" (/ 2.0 (getvar "CANNOSCALEVALUE")))
			   (cons "TextString" (rtos elivation 2 3))
			   )))
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
  (progn)
  )