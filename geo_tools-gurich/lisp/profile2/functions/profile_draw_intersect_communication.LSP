(defun C:profile_draw_intersect_communication
       ( / block-profile-obj curent-profile d horizontal-scale r string t1 vertical-scale z communication)
  ; чертит элипс пересекаемой трубы на профиле2
  (geo_tools-help "profile_draw_intersect_communication")
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
	  (setq flag T))
	)
      (if
	flag
	(progn
	  (defun get-intersect-communication ( / p)
	    (while (not (eq 'LIST (type p)))
	      (initget "Верх Низ Центр Труба Обойма Канал" 1)
	      (setq p (getpoint(strcat "\nУкажи точку вставки элемента "
				       communication
				       "["
				       (cond
					 ((= string "Верх")
					  "Центр/Низ")
					 ((= string "Центр")
					  "Верх/Низ")
					 ((= string "Низ")
					  "Верх/Центр"))
				       (cond
					 ((= communication "Труба")
					  "/Обойма/Канал")
					 ((= communication "Обойма")
					  "/Труба/Канал")
					 ((= communication "Канал")
					  "/Труба/Обойма"))
				       "]<"
				       string
				       ">: ")))
	      (cond
		((or (= p "Верх")(= p "Низ")(= p "Центр"))
		 (setq string p)
		 )
		((or (= p "Труба")(= p "Обойма")(= p "Канал"))
		 (setq communication p)
		 )
		)
	      )
	    p
	    )
	  (setq scale-coefficient (/(* vertical-scale(/ 1000.0 horizontal-scale))1000.0))
	  (setq string "Низ")
	  (setq communication "Труба")
	  (setq t1(get-intersect-communication))
	  (while
	    (/= t1 nil)
	    (initget 1)
	    (setq d
		   (cond
		     ((= communication "Труба")
		      (getreal "\nВведи диаметр трубы в мм : "))
		     ((= communication "Обойма")
		      (getreal "\nВведи сторону обоймы в мм : "))
		     ((= communication "Канал")
		      (getreal "\nВведи высоту канала в мм : ")))
		  )
	    (setq d (/ d 1000.0))
	    (setq z (/ (float vertical-scale) (float horizontal-scale)))
	    (setq r (/ (/ d 2)z))
	    (cond
	      ((= string "Низ")
	       (setq t1 (mapcar '+ t1 (list 0.0(/(/ d 2)scale-coefficient)0.0)))
	       )
	      ((= string "Верх")
	       (setq t1 (mapcar '- t1 (list 0.0(/(/ d 2)scale-coefficient)0.0)))
	       )
	      )
	    (cond
	      ((= communication "Труба")
	       (entmake (list '(0 . "ELLIPSE")
			      '(100 . "AcDbEntity")
			      '(6 . "Continuous")
			      '(100 . "AcDbEllipse")
			      (cons 10 t1)
			      (cons 11 (cons 0.0 (cons (- r (* r 2)) '(0.0))))
			      '(210 0.0 0.0 1.0)
			      (cons 40 z)
			      '(41 . 0.0)
			      (cons 42 (* pi 4))
			      )
			))
	      ((= communication "Обойма")
	       (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
			      '(90 . 4)'(70 . 1)
			      (cons 10 (mapcar '+ t1 (list (*(/ d 2)-1.0) (/(/ d 2) scale-coefficient) 0.0)))
			      (cons 10 (mapcar '+ t1 (list (/ d 2) (/(/ d 2)scale-coefficient) 0.0)))
			      (cons 10 (mapcar '+ t1 (list (/ d 2) (*(/(/ d 2)scale-coefficient)-1.0) 0.0)))
			      (cons 10 (mapcar '+ t1 (list (*(/ d 2)-1.0) (*(/(/ d 2)scale-coefficient)-1.0) 0.0)))))
	       )
	      ((= communication "Канал")
	       (setq width (getreal "\nВведи ширину канала в мм : "))
	       (if width
		 (progn
		   (setq width (/ width 1000.0))
		   (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
				  '(90 . 4)'(70 . 1)
				  (cons 10 (mapcar '+ t1 (list (*(/ width 2)-1.0) (/(/ d 2)scale-coefficient) 0.0)))
				  (cons 10 (mapcar '+ t1 (list (/ width 2) (/(/ d 2)scale-coefficient) 0.0)))
				  (cons 10 (mapcar '+ t1 (list (/ width 2) (*(/(/ d 2)scale-coefficient)-1.0) 0.0)))
				  (cons 10 (mapcar '+ t1 (list (*(/ width 2)-1.0) (*(/(/ d 2)scale-coefficient)-1.0) 0.0)))))
		   )
		 )
	       )
	      )
	    
	    (setq t1(get-intersect-communication))
	    )
	  )
	)
      )
    )
  (princ)
  )