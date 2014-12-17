(defun get-string-multileader (object / points list-elevation pos)
  (if (eq (type object) 'ename)
    (setq object (vlax-ename->vla-object object)))
  (foreach x (convert-string-to-list-be-delimiter (vla-get-TextString object) "\\P")
    (if (and (>(ascii(substr x 1 1))47)(<(ascii(substr x 1 1))58))
      (progn
	(setq pos(vl-string-position (ascii " ") x))
	(setq elevation (atof(substr x 1 pos)))
	(if pos
	  (setq Description (substr x (+ pos 2)))
	  (setq Description x))
	(setq list-elevation (append list-elevation (list(list elevation Description))))
	)
      )
    )
  list-elevation
  )