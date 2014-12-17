(defun analisis-coor-raw-string-nikon (string current-param / result)
  ; преобразование строки с описанием известной точки в ассоциативный список с конвертацией
  (setq list-str (convert-string-to-list-be-delimiter string ","))
  (setq result(list
		(list "genesis" (cond
				  ((=(nth 0 list-str)"UP")"upload")
				  ((=(nth 0 list-str)"MP")"input")
				  ((=(nth 0 list-str)"RE")"resection")
				  ((=(nth 0 list-str)"CC")"calculation")
				  (T"unknown")
				  ))
		(list "name" (nth 1 list-str))
		(list "N" (atof(nth 3 list-str)))
		(list "E" (atof(nth 4 list-str)))
		(list "Z" (atof(nth 5 list-str)))
		(list "code" (nth 6 list-str))
		))
  (if current-param
    (append
      result
      current-param)
    result)
  )