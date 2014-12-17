(defun profile-calculation-and-draw-length-by-pline (block-profile-obj 2Dpline list-select-string /
						     ang curent-text-style down-y-coor-in-curent-string horizontal-scale
						     hs ht list_points mleader_obj n number-string p1 p2 profile-parameters
						     profile-string-list pt_array s scale-coefficient spisokv
						     t3 text-obj up-y-coor-in-curent-string vertical-scale obj-list dec flag ht-current)
  ;чертит расстояния на профиле
  
  (if(=(vlax-ldata-get "geo_tools_dictionary" "profile-mode")1)
    (progn
      (setq up-y-coor-in-curent-string (nth 1 list-select-string))
      (setq down-y-coor-in-curent-string(nth 2 list-select-string))
      (setq number-string(nth 0 list-select-string))
      (setq profile-string-list(vlax-ldata-get block-profile-obj"profile-string-list"))
      (setq list-select-string(nth number-string profile-string-list))
      (princ(setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale")))
      (terpri)
      (princ(setq vertical-scale (vlax-ldata-get block-profile-obj"vertical-scale")))
      (setq profile-parameters(vlax-ldata-get block-profile-obj"profile-parameters"))
      (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
      (setq curent-text-style (nth 1 profile-parameters))
      (if(or(eq curent-text-style "***Текущий***")(not(member curent-text-style(get-text-style-list))))
	(setq curent-text-style(getvar "TEXTSTYLE")))
      (setq ht (*(atof(nth 6 list-select-string))scale-coefficient))
      (setq hs (*(atof(nth 2 list-select-string))scale-coefficient))
      (setq dec (atoi(nth 4 list-select-string)))
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
	    (setq ht (*
		       (cadr(assoc "height-text" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list")))
		       scale-coefficient))
	    (setq hs (abs(- up-y-coor-in-curent-string down-y-coor-in-curent-string)))
	    (setq dec (cadr(assoc "accuracy" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	    (setq flag T)
	    )))
      )
    )
  (setq min-h-text (* 1.6 scale-coefficient))
  (if flag
    (progn
      (if (=(type 2Dpline) 'list)
	(setq spisokv 2Dpline)
	(setq spisokv(extract_coord_lwpolyline (entget 2Dpline))))
      (setq n (1- (length spisokv)))
      (repeat n
	(setq p1 (car spisokv))
	(setq spisokv (cdr spisokv))
	(setq p2 (car spisokv))
	(setq s  (- (nth 0 p2) (nth 0 p1)))
	(if (> s 0)
	  (progn
	    (if
	      (>(+ min-h-text(* min-h-text(/ 2.0 3.0)))s); если совсем не влазит текст, даже боком, вставляем мультивыноску
	      (progn
		(setq t3 (list (+(nth 0 p1)(/ s 2)) (+ down-y-coor-in-curent-string(/ hs 2))0))
		(setq list_points (list t3 (mapcar '+ t3 (list (* 3 scale-coefficient) (* -10 scale-coefficient)))))
		(setq pt_array (convert_coords_list_from_safearray_mleader list_points))
		(setvar "CMLEADERSTYLE" "Подписи")
		(setq mleader_obj (vla-addmleader model_spece pt_array 0))
		(vla-put-StyleName mleader_obj "Подписи")
		(vla-put-textstring mleader_obj (rtos s 2 dec))
		(vla-put-TextHeight mleader_obj (* 2.5 scale-coefficient))
		(setq obj-list(append obj-list (list mleader_obj)))
		(setq obj-list(append obj-list (list (draw-line-vla model_spece (list(nth 0 p1)down-y-coor-in-curent-string) (list(nth 0 p1)up-y-coor-in-curent-string)))))
		)
	      (progn
		(if
		  (< (+(calculation_str_long_be_h_stile (rtos s 2 dec) ht curent-text-style)(* ht(/ 2.0 3.0))) s); влазит ли текст горизонтально
		  (progn ;влазит
		    (setq ang 0)
		    (setq ht-current ht)
		    )
		  (progn ;не влазит
		    (setq ang (/ pi 2))
		    (if
		      (>(+ ht (* ht (/ 2.0 3.0)))s); нужно ли уменьшить вертикальный текст
		      (setq ht-current (* s (/ 2.0 3.0)))
		      (setq ht-current ht)
		      )
		    )
		  ); if
		(setq text-obj(draw_text_vla model_spece (rtos s 2 dec) (list (+(nth 0 p1)(/ s 2.0)) (+ down-y-coor-in-curent-string(/ hs 2.0))0) ht-current 4 ang nil T T))
		(vla-put-StyleName text-obj curent-text-style)
		(setq obj-list(append obj-list (list text-obj)))
		(setq obj-list(append obj-list(list(draw-line-vla model_spece (list(nth 0 p1)down-y-coor-in-curent-string) (list(nth 0 p1)up-y-coor-in-curent-string)))))
		); progn
	      ); if
	    )
	  )
	)
      (setq obj-list(append obj-list(list(draw-line-vla model_spece (list(nth 0 p2)down-y-coor-in-curent-string) (list(nth 0 p2)up-y-coor-in-curent-string)))))
      )
    )
  )