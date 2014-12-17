(defun C:profile_draw_excess (/ block-profile-obj curent-profile h horizontal-scale t1 t2 vertical-scale flag profile-mode reference-level)
  ; Ставит маркер на указанном превышении от указаной точки
  (geo_tools-help "profile_draw_excess")
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
	  (setq flag nil)
	  (initget)(setq t1(getpoint "\nУкажи точку: "))
	  (while (not(= t1 nil))
	    (initget)(setq h (getreal "\nВведи превышение: "))
	    (if h
	      (progn
		(vla-StartUndoMark active_document)
		(setq t2 (mapcar '+ t1 (list 0 (/ h(/ (* vertical-scale (/ 1000.0 horizontal-scale)) 1000.0)))))
		(entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t1) (append '(11) t2)))
		(entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
			       '(90 . 3) '(70 . 1) (cons 43 0)
			       (cons 10 (mapcar '+ t2 (list -1.25 -2.165)))
			       (cons 10 t2)
			       (cons 10 (mapcar '+ t2 (list 1.25 -2.165)))
			       )
			 )
		(vla-EndUndoMark active_document)
		)
	      )
	    (initget)(setq t1(getpoint "\nУкажи точку: "))
	    )
	  )
	)
      )
    )
  (princ)
  )