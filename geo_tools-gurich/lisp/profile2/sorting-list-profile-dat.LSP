(defun sorting-list-profile-dat (list-profile / result-list x flag pre-result-list)
  ; программа спутник profile-get-sorting-list-profile-dat
  ; обрабатывает строки полученые из файла расчлен€€ их на параметры строк подвала
  (setq result-list
	 (append result-list
		 (list
		   (nth 0 list-profile))))
  (foreach x (cdr list-profile)
    (setq flag nil)
    (while (not flag)
      (if (vl-string-position (ascii "|") x nil)
	(progn
	  (setq pre-result-list(append pre-result-list (list(substr x 1 (vl-string-position (ascii "|") x nil)))))
	  (setq x (substr x (+ 2(vl-string-position (ascii "|") x nil))(strlen x)))
	  )
	(progn
	  (setq pre-result-list(append pre-result-list (list(substr x 1 (strlen x)))))
	  (setq flag T)
	  )
	)
      )
    (setq result-list(append result-list (list pre-result-list)))
    (setq pre-result-list nil)
    )
  result-list
  )