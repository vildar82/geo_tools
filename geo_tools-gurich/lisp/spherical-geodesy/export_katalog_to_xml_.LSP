(defun C:export_katalog_to_xml_ (/ bl deskription list-punct-ass-list list-strings n nn o-doc put-file squares style punct-ass-list LIST-POINTS-KONTUR TYPE-LIST suum)
  ;экспортирует не весь каталог в xml файл
  (if (setq o-doc(XML-Get-XMLObject (get-patch-to-katalog)))
    (if(setq put-file (getfiled "Имя сохраняемого файла..." (getvar "DWGPREFIX") "xml" 1))
      (progn
	(setq type-list(get-punct-type-list-in-katalog o-doc))
	(add-mggt-punct-types-in-drawning)
	(setq list-points-kontur (convert-safearray-to-list-points(vlax-variant-value(vla-get-Coordinates(vlax-ename->vla-object(car(entsel "\nВыбери линию: ")))))2))
	(setq squares(XML-Get-Child (XML-Get-Child o-doc nil "puncts") nil "squares"))
	(setq n 1)
	(setq nn 0)
	(foreach x (XML-Get-ChildList squares)
	  (foreach y (XML-Get-ChildList x)
	    (setq punct-ass-list(convert-punkt-recorg-to-ass-list y))
	    (if
	      (analisis-point-in-contur list-points-kontur (list (atof(get-item-by-name-in-list punct-ass-list "E"))(atof(get-item-by-name-in-list punct-ass-list "N"))))
	      (progn
		(setq list-punct-ass-list (append list-punct-ass-list (list punct-ass-list)))
		(setq nn (1+ nn))
		)
	      )
	    (if (=(rem n 100)0)(grtext -1 (strcat"читаю" (itoa n) "-" (itoa nn))))
	    (setq n (1+ n))
	    )
	  )
	(grtext -1 "")
	(if squares(vlax-release-object squares))
	(if o-doc(vlax-release-object o-doc))
	(setq n 1)
	(setq suum (itoa(length list-punct-ass-list)))
	(foreach z list-punct-ass-list
	  (setq deskription
		 (strcat
		   "name=" (get-item-by-name-in-list z "name") "; "
		   "N=" (get-item-by-name-in-list z "N") "; "
		   "E=" (get-item-by-name-in-list z "E")
		   (if (/=(get-item-by-name-in-list z "Z")nil) (strcat "; Z=" (get-item-by-name-in-list z "Z")) "")
		   (if (/=(get-item-by-name-in-list z "year")nil) (strcat "; year=" (get-item-by-name-in-list z "year")) "")
		   (if (/=(get-item-by-name-in-list z "address")nil) (strcat "; address=" (get-item-by-name-in-list z "address")) "")
		   (if (/=(get-item-by-name-in-list z "comment")nil) (strcat "; comment=" (get-item-by-name-in-list z "comment")) "")
		   )
		)
	  (setq BL (convert-mggt-xy-to-wgs-84-bl (atof(get-item-by-name-in-list z "N"))(atof(get-item-by-name-in-list z "E"))))
	  (setq list-strings
		 (append list-strings
			 (list
			   (strcat
			     "<GeoObject><style>#poi-map-webcam-1</style><gml:description><![CDATA["deskription"]]></gml:description><gml:Point><gml:pos>"(rtos(nth 1 BL)2 6)" "(rtos(nth 0 BL)2 6)"</gml:pos></gml:Point></GeoObject>")
			   )
			 )
		)
	  (if (=(rem n 100)0)(grtext -1 (strcat"считаю" (itoa n) "/" suum)))
	  (setq n (1+ n))
	  )
	(grtext -1 "запись...")
	(file_write_list_to_file put-file list-strings)
	(grtext -1 "")
	(princ (strcat "\nЭкспортировано " (itoa(length list-strings)) " пунктов."))
	
	)
      )
    (alert "Файл каталога координат пунктов не обнаружен!")
    )
  (princ)
  )