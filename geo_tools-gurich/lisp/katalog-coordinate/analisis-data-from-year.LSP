(defun analisis-data-from-year (string / flag list-string n year)
  (setq list-string (reverse(vl-string->list string)))
  (setq n 0)
  (while (not flag)
    (if (or (=(nth n list-string)nil)(analisis-kod-by-delimiter (nth n list-string)))
      (setq flag T)
      (setq year (append year (list (nth n list-string))))
      )
    (setq n (1+ n))
    )
  (if year
    (progn
      (setq year(vl-list->string(reverse year)))
      (cond
	((= (strlen year) 4)
	 (atoi year))
	((> (atoi year) 30)
	 (+(atoi year)1900))
	((< (atoi year) 30)
	 (+(atoi year)2000))
	)
      )
    );if
  )