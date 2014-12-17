(defun profile-calculation-string-by-point (block-profile-obj select-string-point /
					    down-y-coor-in-curent-str flag horizontal-scale n profile-string-list
					    result-list select-string-y start-point up-y-coor-in-curent-str)
  ; определяет какой строке подвала принадлежит указаная точка
  (setq select-string-y (nth 1 select-string-point))
  (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
  (setq profile-string-list(vlax-ldata-get block-profile-obj"profile-string-list"))
  (setq start-point(vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj))))
  (setq up-y-coor-in-curent-str (nth 1 start-point))
  (setq n 0)
  (while (not(or (> n (1-(length profile-string-list)))(eq flag T)))
    (setq down-y-coor-in-curent-str(- up-y-coor-in-curent-str (*(atof(nth 2 (nth n profile-string-list)))(/ 1.0 (/ 1000.0 horizontal-scale)))))
    (if (or
	  (and (> up-y-coor-in-curent-str select-string-y)
	       (< down-y-coor-in-curent-str select-string-y)
	       )
	  (eq up-y-coor-in-curent-str select-string-y)
	  )
      (progn
	(setq flag T)
	(setq result-list (nth n profile-string-list)))
      (progn
	(setq up-y-coor-in-curent-str down-y-coor-in-curent-str)
	(setq n (1+ n))
	)
      )
    )
  (if
    (> select-string-y (nth 1 start-point))
    (list -1 nil nil)
    (if result-list
      (list n up-y-coor-in-curent-str down-y-coor-in-curent-str)
      )
    )
  )