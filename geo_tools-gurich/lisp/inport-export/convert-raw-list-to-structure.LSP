(defun convert-raw-list-to-structure (str-list-file / structure-list str-list-file current-st-list current-param)
  ; конвертирует текстовый файл RAW данных Nikon в список строктурированных данных
  (setq structure-list (list
			 (list "header")
			 (list "catalog-coord")
			 (list "observations")
			 (list "resection")
			 )
	)
  (foreach x str-list-file
    (cond
      ((=(substr x 1 2)"CO")
       (if(=(substr x 1 8)"CO,Temp:")
	 (progn
	   (setq list-param (convert-string-to-list-be-delimiter(substr x 3)" "))
	   (setq current-param (list
				 (list "temp" (substr (nth 0 list-param)7))
				 (list "press" (substr (nth 1 list-param)7))
				 (list "prism" (substr (nth 2 list-param)7))
				 (list "data" (convert-data-format-acssos-list-to-string(convert-data-format-string-to-acssos-list (nth 3 list-param))))
				 (list "time" (nth 4 list-param))
				 )
		 )
	   )
	 (if (=(substr x 1 4)"CO, ")
	   (progn
	     (setq list-param (convert-string-to-list-be-delimiter(substr x 3)" "))
	     (setq current-param (list
				   (list "data" (convert-data-format-acssos-list-to-string(convert-data-format-string-to-acssos-list (nth 4 list-param))))
				   (list "time" (nth 5 list-param))
				   )
		   )
	     )
	   )
	 )
       )
      ((or (=(substr x 1 2)"UP")(=(substr x 1 2)"MP")(=(substr x 1 2)"CC")(=(substr x 1 2)"RE"))
       (setq structure-list (put-item-by-name-in-list structure-list "catalog-coord"
			      (append (get-item-by-name-in-list structure-list "catalog-coord") (list (analisis-coor-raw-string-nikon x current-param)))))
       )
      ((=(substr x 1 2)"ST")
       (if current-st-list
	 (progn
	   (setq structure-list
		  (put-item-by-name-in-list structure-list "observations"
		    (append
		      (get-item-by-name-in-list structure-list "observations")
		      (list current-st-list))))
	   (setq current-st-list nil)
	   )
	 )
       (setq current-st-list (append current-st-list (list (analisis-st-raw-string-nikon x current-param))))
       )
      ((=(substr x 1 2)"SS")
       (setq current-st-list (append current-st-list (list (analisis-ss-raw-string-nikon x current-param)))))
      )
    )
  (if current-st-list
	 (progn
	   (setq structure-list
		  (put-item-by-name-in-list structure-list "observations"
		    (append
		      (get-item-by-name-in-list structure-list "observations")
		      (list current-st-list))))
	   (setq current-st-list nil)
	   )
	 )
  structure-list
  )