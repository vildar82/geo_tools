(defun C:select_obj_in_drewning_by_sel (/ obj nabor-first nabor-get n)
  ; выбор на чертеже таких же объектов как указал пользователь
  (geo_tools-help "select_obj_in_drewning_by_sel")
  (if (setq obj(car(entsel "\nВыбери объект образец: ")))
    (progn
      (setq nabor-first (cadr(ssgetfirst)))
      (setq nabor-get (ssget "_X" (list (assoc 0 (entget obj)))))
      (if nabor-first
	(progn
	  (setq n 0)
	  (if nabor-get
	    (progn
	      (repeat (sslength nabor-first)
		(setq nabor-get (ssadd (ssname nabor-first n) nabor-get ))
		(setq n (1+ n)))
	      )
	    )
	  
	  )
	)
      (sssetfirst nil nabor-get)
      )
    (princ "\nНичего не выбрано.")
    )
  (princ)
  )