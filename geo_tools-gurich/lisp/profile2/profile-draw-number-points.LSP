(defun profile-draw-number-points (block-profile-obj  / 2Dpline profile-string-list profile-parameters
				   block-obj buffer-zone current-str diskription down-y-coor-in-curent-str flag h-str h-text insert-point
				   leaders_list mid-y-coor-in-curent-str multileader-obj n scale-coefficient spisokv ss start-point
				   up-y-coor-in-curent-str result-list text-obj curent-text-style horizontal-scale vertical-scale leaders-list
				   )
  ; подписывает номера точек трассы в подвале профиля
  (setq 2Dpline (trace-create-2Dpline-by-block-profile block-profile-obj))
  (setq block-obj (cdr(assoc "block"(vlax-ldata-get block-profile-obj"obj-list"))))
  (setq vertical-scale (vlax-ldata-get block-profile-obj"vertical-scale"))
  (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
  (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
  (setq profile-string-list(vlax-ldata-get block-profile-obj"profile-string-list"))
  (setq profile-parameters(vlax-ldata-get block-profile-obj"profile-parameters"))
  (setq buffer-zone(*(atof(nth 3 profile-parameters))scale-coefficient))
  (setq start-point(mapcar'+ (vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj)))(list buffer-zone 0)))
  
  (setq curent-text-style (nth 1 profile-parameters))
  
  (if(or(eq curent-text-style "***Текущий***")(not(member curent-text-style(get-text-style-list))))
    (setq curent-text-style(getvar "TEXTSTYLE")))
  (setq up-y-coor-in-curent-str (nth 1 start-point))
  (setq n 0)
  (while (not(or (> n (1-(length profile-string-list)))(eq flag T)))
    (if (eq "name-points" (nth 1 (nth n profile-string-list)))
      (progn
	(setq flag T)
	(setq current-str (nth n profile-string-list)))
      (progn
	(setq up-y-coor-in-curent-str(- up-y-coor-in-curent-str (*(atof(nth 2 (nth n profile-string-list)))(/ 1.0 (/ 1000.0 horizontal-scale)))))
	(setq n (1+ n))
	)
      )
    )

  ;;;  (?? ????? name-points 10 - - - 2.5 1 - -)
  (if flag
    (progn
      (setq h-text (*(atof(nth 6 current-str))scale-coefficient))
      (setq h-str (*(atof(nth 2 current-str))scale-coefficient))
      (setq mid-y-coor-in-curent-str (- up-y-coor-in-curent-str (/ h-str 2)))
      (setq down-y-coor-in-curent-str (- up-y-coor-in-curent-str h-str))
      (setq ss (ssget "_X" '((0 . "MULTILEADER"))))
      (if ss
	(progn
	  (setq leaders-list(convert_ss_to_list ss))
	  (foreach x leaders-list
	    (if (/= (vla-get-ContentBlockName (vlax-ename->vla-object x)) "")
	      (setq leaders_list (append leaders_list (list x))))
	    )
	  (setq spisokv (extract_coord_lwpolyline (entget(vlax-vla-object->ename 2Dpline))))
	  (foreach x spisokv
	    (if (setq multileader-obj(find-multileader-in-list-be-point leaders_list x))
	      (if (/= (vla-get-ContentBlockName (vlax-ename->vla-object multileader-obj)) "")
		(progn
		  (setq diskription (cadr(extract_302 (entget multileader-obj))))
		  (setq insert-point (list(+(nth 0 start-point)(vlax-curve-getDistAtPoint 2Dpline x))
					  mid-y-coor-in-curent-str
					  ))
		  (if (eq (nth 7 current-str) "1")
		    (setq result-list(append result-list
					     (list
					       (draw-circle-vla model_spece insert-point (/ h-str 2)))))
		    )
		  (setq text-obj(draw_text_vla model_spece diskription insert-point h-text nil T nil T T))
		  (vla-put-StyleName text-obj curent-text-style)
		  (setq result-list(append result-list(list text-obj)))
		  )
		)
	      )
	    );foreach
	  )
	);if
      )
    )
  (vla-erase 2Dpline)
  result-list
  )