(defun edit_translit_str ( stroka / list_stroka x result)
  (setq list_stroka (vl-string->list stroka))
  (setq result "")
  (mapcar '(lambda (x)
	     (setq result (strcat result (edit_translit_letter x)))
	     )
	     list_stroka)
  result
	  )
