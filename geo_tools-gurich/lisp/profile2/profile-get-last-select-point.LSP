(defun profile-get-last-select-point (/ last-select-point profile-mode block-profile-obj curent-profile)
  ; запрос на ввод точки на условном горизонте
  (setq profile-mode (vlax-ldata-get "geo_tools_dictionary" "profile-mode"))
  (if (= profile-mode 1)
    (progn
      (setq curent-profile(vlax-ldata-get "geo_tools_dictionary" "curent-profile"))
      (setq block-profile-obj (cdr curent-profile))
      (setq last-select-point (vlax-ldata-get block-profile-obj"last-select-point"))
      (if
	(eq last-select-point nil)
	(progn
	  (initget)(setq last-select-point (getpoint "\nУкажи точку в профиле: "))
	  (if last-select-point(vlax-ldata-put block-profile-obj"last-select-point"last-select-point)))
	(progn
	  (draw_temp_X last-select-point 1)
	  (initget)(setq last-select-point (getpoint "\nУкажи точку в профиле <ПРЕЖНЯЯ>: "))
	  (redraw)
	  (if (eq last-select-point nil)
	    (setq last-select-point (vlax-ldata-get block-profile-obj"last-select-point"))
	    (vlax-ldata-put block-profile-obj"last-select-point"last-select-point)))
	)
      )
    (progn
      (setq last-select-point(vlax-ldata-get "geo_tools_dictionary" "reference-level-point"))
      (if
	(eq last-select-point nil)
	(progn
	  (initget)(setq last-select-point (getpoint "\nУкажи точку на условном горизонте: "))
	  (if last-select-point(vlax-ldata-put "geo_tools_dictionary" "reference-level-point"last-select-point)))
	(progn
	  (draw_temp_X last-select-point 1)
	  (initget)
	  (setq last-select-point (getpoint "\nУкажи точку на условном горизонте <ПРЕЖНЯЯ>: "))
	  (redraw)
	  (if (eq last-select-point nil)
	    (setq last-select-point (vlax-ldata-get "geo_tools_dictionary" "reference-level-point"))
	    (vlax-ldata-put "geo_tools_dictionary" "reference-level-point"last-select-point)))
	)
      )
    )
  last-select-point
  )