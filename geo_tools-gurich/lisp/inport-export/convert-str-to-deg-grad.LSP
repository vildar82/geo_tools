(defun convert-str-to-deg-grad (string / pos)
  ;конвертирует строку вида "123.4556" (в которой 123 градусы 45 минуты 56 секунды) в десятичные градусы
  (setq pos (vl-string-search "." string))
    (+
      (atof(substr string 1 pos))
      (/
	(+
	  (*
	    (atof(substr string (+ pos 2) 2))
	    60)
	  (atof(substr string (+ pos 4) 2)))
	3600
	)
      )
  )