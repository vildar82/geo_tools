(defun profile-draw-angle-trace (block-profile-obj / 
				 a b block-obj buffer-zone current-str down-y-coor-in-curent-str flag
				 g h-str h-text mid-y-coor-in-curent-str n p1 p2 p3 2Dpline
				 result-list scale-coefficient spisokv start-point up-y-coor-in-curent-str
				 arrow-angle insert-point length-text lwpolyline-list-points text_vla curent-text-style
				 profile-parameters profile-string-list horizontal-scale
				 )
  ; подписывает углы поворота трассы в подвале профиля
  (setq 2Dpline (trace-create-2Dpline-by-block-profile block-profile-obj))
  (setq block-obj (cdr(assoc "block"(vlax-ldata-get block-profile-obj"obj-list"))))
  (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
  (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
  (setq profile-parameters(vlax-ldata-get block-profile-obj"profile-parameters"))
  (setq buffer-zone(*(atof(nth 3 profile-parameters))scale-coefficient))
  (setq profile-string-list(vlax-ldata-get block-profile-obj"profile-string-list"))
  
  (setq start-point(mapcar'+ (vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj)))(list buffer-zone 0)))
  (setq up-y-coor-in-curent-str (nth 1 start-point))
  (setq curent-text-style (nth 1 profile-parameters))
  (if(or(eq curent-text-style "***Текущий***")(not(member curent-text-style(get-text-style-list))))
    (setq curent-text-style(getvar "TEXTSTYLE")))
  
  (setq n 0)
  (while (not(or (> n (1-(length profile-string-list)))(eq flag T)))
    (if (eq "angle-trace" (nth 1 (nth n profile-string-list)))
      (progn
	(setq flag T)
	(setq current-str (nth n profile-string-list)))
      (progn
	(setq up-y-coor-in-curent-str(- up-y-coor-in-curent-str (*(atof(nth 2 (nth n profile-string-list)))(/ 1.0 (/ 1000.0 horizontal-scale)))))
	(setq n (1+ n))
	)
      )
    )

  (if flag
    (progn
      (setq h-text (*(atof(nth 6 current-str))scale-coefficient))
      (setq h-str (*(atof(nth 2 current-str))scale-coefficient))
      (setq mid-y-coor-in-curent-str (- up-y-coor-in-curent-str (/ h-str 2)))
      (setq down-y-coor-in-curent-str (- up-y-coor-in-curent-str h-str))
      (setq spisokv (extract_coord_lwpolyline (entget(vlax-vla-object->ename 2Dpline))))
      (repeat (- (length spisokv) 2)
	(setq p1 (car spisokv))
	(setq spisokv (cdr spisokv))
	(setq p2 (car spisokv))
	(setq p3 (cadr spisokv))
	(setq a (angle p2 p1))
	(setq b (angle p2 p3))
	(cond
	  ((> a b)
	   (setq g (- a b)))
	  ((< a b)
	   (setq g (-(* pi 2)(- b a))))
	  )
	(if (not (and (< (- pi (*(/ pi 180)(atof(nth 8 current-str)))) g) (< g (+ pi (*(/ pi 180)(atof(nth 8 current-str)))))))
	  (progn
	    (setq insert-point (list(+(nth 0 start-point)(vlax-curve-getDistAtPoint 2Dpline p2))
					mid-y-coor-in-curent-str))
	    (setq text_vla(draw_text_vla model_spece (strcat (rtos (/ (if (> g pi) (- (* pi 2) g) g) (/ pi 180)) 2 0)"%%d")
			    (mapcar'- insert-point (list (* scale-coefficient 0.5) 0))
			    h-text 11 T nil T T))
	    (vla-put-StyleName text_vla curent-text-style)
	    (setq result-list(append result-list(list text_vla)))
	    (setq length-text (+(calculation_text_height_be_object text_vla)(* scale-coefficient 1)))
	    (setq lwpolyline-list-points
		   (if(> g pi)
		     (list
		       (list (nth 0 insert-point)
			     (- mid-y-coor-in-curent-str (* h-text 0.8)))
		       insert-point
		       (list (nth 0 insert-point)
			     (+ mid-y-coor-in-curent-str (* h-text 0.8)))
		       (list (-(nth 0 insert-point)length-text)
			     (+ mid-y-coor-in-curent-str (* h-text 0.8)))
		       )
		     (list
		       (list (nth 0 insert-point)
			     (+ mid-y-coor-in-curent-str (* h-text 0.8)))
		       insert-point
		       (list (nth 0 insert-point)
			     (- mid-y-coor-in-curent-str (* h-text 0.8)))
		       (list (-(nth 0 insert-point)length-text)
			     (- mid-y-coor-in-curent-str (* h-text 0.8)))
		       )
		     )
		  )
	(setq arrow-angle (vla-addLightWeightPolyline model_spece
			    (convert-list-points-to-lwpolyline-safearray lwpolyline-list-points)))
	    (vla-put-Linetype arrow-angle "Continuous")
	    (vla-setWidth arrow-angle 0 0.0 (* 1.0 scale-coefficient))
	    
	    (setq result-list(append result-list(list arrow-angle)))
	    )
	  )
	;
	)
      )
    )
  (vla-erase 2Dpline)
  result-list
  )