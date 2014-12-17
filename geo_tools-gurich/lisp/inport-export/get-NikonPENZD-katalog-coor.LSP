(defun get-NikonPENZD-katalog-coor (put-file / file_points_list file str temp)
  ;преобразование текстового файла формата Nikon PENZD в каталог координат
  (setq file (open put-file"r"))
  (while(setq str (read-line file))
    (setq temp (convert-string-to-list-be-delimiter str ","))
    (setq file_points_list
	   (append file_points_list
		   (list (list
			   (list "name"(vl-string-trim" "(nth 0 temp)))
			   (list "N"(atof(vl-string-trim" "(nth 1 temp))))
			   (list "E"(atof(vl-string-trim" "(nth 2 temp))))
			   (list "Z"(atof(vl-string-trim" "(nth 3 temp))))
			   (list "code"(vl-string-trim" "(nth 4 temp)))
			   ))))
    )
  (close file)
  file_points_list
  )