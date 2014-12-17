(defun C:export_katalog_to_xml (/ bl deskription list-punct-ass-list list-strings n o-doc put-file squares style)
  ;экспортирует весь каталог в kml файл
  (if (setq o-doc(XML-Get-XMLObject (get-patch-to-katalog)))
    (if(setq put-file (getfiled "»м€ сохран€емого файла..." (getvar "DWGPREFIX") "xml" 1))
      (progn
	(setq squares(XML-Get-Child (XML-Get-Child o-doc nil "puncts") nil "squares"))
	(setq n 1)
	(foreach x (XML-Get-ChildList squares)
	  (foreach y (XML-Get-ChildList x)
	    (setq list-punct-ass-list (append list-punct-ass-list (list (convert-punkt-recorg-to-ass-list y))))
	    (if (=(rem n 100)0)(grtext -1 (strcat"читаю"(itoa n))))
	    (setq n (1+ n))
	    )
;;;	  (gc)
	  
	  )
	(grtext -1 "")
	(if squares(vlax-release-object squares))
	(if o-doc(vlax-release-object o-doc))
;;;	(setq list-strings (list
;;;			     "<?xml version=\"1.0\" encoding=\"windows-1251\"?>"
;;;			     "<kml xmlns=\"http://earth.google.com/kml/2.2\">"
;;;			     "<Document>"
;;;			     "  <name>MGGT</name>"
;;;			     "  <description><![CDATA[ѕункты съемочного обосновани€, карта создана 2012.10.23]]></description>"
;;;			     "  <Style id=\"true-z\"><IconStyle><Icon><href>http://maps.gstatic.com/mapfiles/ms2/micons/red-dot.png</href></Icon></IconStyle></Style>"
;;;			     "  <Style id=\"true\"><IconStyle><Icon><href>http://maps.gstatic.com/mapfiles/ms2/micons/red.png</href></Icon></IconStyle></Style>"
;;;			     "  <Style id=\"false\"><IconStyle><Icon><href>http://maps.gstatic.com/mapfiles/ms2/micons/blue.png</href></Icon></IconStyle></Style>"
;;;			     "  <Style id=\"false-z\"><IconStyle><Icon><href>http://maps.gstatic.com/mapfiles/ms2/micons/blue-dot.png</href></Icon></IconStyle></Style>"
;;;			     )
;;;	      )
	(setq n 1)
	(foreach z list-punct-ass-list
;;;	  (cond
;;;	    ((and (get-item-by-name-in-list z "year")(get-item-by-name-in-list z "Z"))
;;;	     (setq style "true-z")
;;;	     )
;;;	    ((and (get-item-by-name-in-list z "year")(=(get-item-by-name-in-list z "Z")nil))
;;;	     (setq style "true")
;;;	     )
;;;	    ((and (=(get-item-by-name-in-list z "year")nil)(get-item-by-name-in-list z "Z"))
;;;	     (setq style "false-z")
;;;	     )
;;;	    (T
;;;	     (setq style "false")
;;;	     )
;;;	    )
	  (setq deskription
		 (strcat
		   "name=" (get-item-by-name-in-list z "name") "; "
		   "N=" (get-item-by-name-in-list z "N") "; "
		   "E=" (get-item-by-name-in-list z "E") 
		   (if (/=(get-item-by-name-in-list z "Z")nil) (strcat "; Z=" (get-item-by-name-in-list z "Z") "<br>") "")
		   (if (/=(get-item-by-name-in-list z "address")nil) (strcat "; address=" (get-item-by-name-in-list z "address") "<br>") "")
		   (if (/=(get-item-by-name-in-list z "comment")nil) (strcat "; comment=" (get-item-by-name-in-list z "comment") "<br>") "")
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
	  (if (=(rem n 100)0)(grtext -1 (strcat"считаю"(itoa n))))
	  (setq n (1+ n))
	  )
;;;	(setq list-strings
;;;	       (append list-strings
;;;		       (list
;;;			 "</Document>"
;;;			 "</kml>"
;;;			 )
;;;		       )
;;;	      )
	(grtext -1 "запись...")
	(file_write_list_to_file put-file list-strings)
	(grtext -1 "")
	(princ (strcat "\nЁкспортировано " (itoa(length list-strings)) " пунктов."))

	)
      )
    (alert "‘айл каталога координат пунктов не обнаружен!")
    )
  (princ)
  )