(defun C:proba (/ input-file list-point list-points s string)
  (setq input-file (strcat(getvar "DWGPREFIX")"REPERы.txt"))
  (if (file-p input-file)
    (progn
      (setq n 1)
      (setq input-file(open input-file "r"))
      (setq string (read-line input-file))
      (while string
	(if (=(rem n 100)0)(grtext -1 (strcat"Обрабатываю "(itoa n))))
	(convert-string-to-ass-list x)
	(setq list-point (convert-string-to-list-be-delimiter string "|"))
	;name|N|E|Z|type|address|year|comment
	;0   |1|2|3|4   |5      |6   |7
	(if (/= (nth 5 list-point) "")
	  (progn
	    (setq year_new (analisis-string-from-data (nth 5 list-point)))
	    (setq type_new (analisis-addres-from-type (nth 5 list-point)))
	    )
	  )
	(setq list-points (append list-points (list list-point)))
	
	(setq string (read-line input-file))
	(setq n (1+ n))
	)
      (princ "\n строк ")
      (princ (length list-points))
      (grtext -1 "")
      (close input-file)
      )
    (alert "Ошибка! Исходный файл координат не обнаружен!")
    )
  (princ)
  )