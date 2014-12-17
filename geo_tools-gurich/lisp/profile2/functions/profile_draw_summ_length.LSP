(defun C:profile_draw_summ_length (/ block-profile-obj curent-profile flag h horizontal-scale n p1 p2 pline-obj profile-mode reference-level s spisokv summa-h summa-l vertical-scale
		height-text insert-point profile-parameters scale-coefficient textstring tstring)
  ; вычисляет длинну профиля с учетом вертикальных участков
  (geo_tools-help "profile_draw_summ_length")
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
	      (setq profile-parameters(vlax-ldata-get block-profile-obj"profile-parameters"))
	      (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
	      (setq height-text (*(atof(nth 9 profile-parameters))scale-coefficient))
	      ))
	  )
	(progn
	  (setq horizontal-scale (cadr(assoc "horizontal-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	  (setq vertical-scale (cadr(assoc "vertical-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	  (setq reference-level (cadr(assoc "reference-level" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	  (princ(strcat
		  "\nМасштабВ 1:" (itoa horizontal-scale)
		  " МасштабГ 1:" (itoa vertical-scale)
		  " Отметка условного горизонта: " (rtos reference-level 2 2) "м"
		  ))
	  (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
	  (setq height-text (* (cadr(assoc "height-text" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list")))scale-coefficient))
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
	     (setq spisokv(extract_coord_lwpolyline (entget pline-obj)))
	     (setq n (1- (length spisokv)))
	     (setq summa-l 0)
	     (setq summa-h 0)
	     (repeat n
	       (setq p1 (car spisokv))
	       (setq spisokv (cdr spisokv))
	       (setq p2 (car spisokv))
	       (setq s  (- (nth 0 p2) (nth 0 p1)))
	       (setq h (abs(* (-(nth 1 p2)(nth 1 p1)) (/ (* vertical-scale (/ 1000.0 horizontal-scale)) 1000.0))))
	       (if (> s 0)
		 (setq summa-l (+ summa-l s))
		 (setq summa-h (+ summa-h h))
		 )
	       )
	     (setq TString(strcat "\U+03A3L="(rtos summa-l 2 2)"м"))
	     (setq TextString(strcat "Сумма длинн: "(rtos summa-l 2 2)"м"))
	     (if (> summa-h 0 )
	       (setq  TextString(strcat TextString "\nСумма превышений: "(rtos summa-h 2 2)"м"
					  "\nСумма общая: "(rtos (+ summa-l summa-h) 2 2)"м")))
	     (if (> summa-h 0 )
	       (setq  TString(strcat TString "\n\U+03A3h="(rtos summa-h 2 2)"м"
					  "\n\U+03A3общ="(rtos (+ summa-l summa-h) 2 2)"м")))
	     (princ (strcat "\n"TextString))
	     (setq insert-point(getpoint"\nУкажи куда будем вставлять текст <Пропустить>: "))
	     (if
	       insert-point
	       (progn
		 (draw-mtext-vla-by-list-property
		   (list
		     (cons "spece" model_spece)
		     (cons "InsertionPoint" insert-point)
		     (cons "Width" 0)
		     (cons "TextString" TString)
		     (cons "Height" height-text)
		     ))
		 )
	       )
	     )
	    );cond
	  )
	)
      )
    )
  (princ)
  )