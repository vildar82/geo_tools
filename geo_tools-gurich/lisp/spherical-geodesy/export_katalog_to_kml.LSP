(defun C:export_katalog_to_kml (/ bl deskription list-punct-ass-list list-strings n o-doc put-file squares style)
  ;������������ ���� ������� � kml ����
  (if (setq o-doc(XML-Get-XMLObject (get-patch-to-katalog)))
    (if(setq put-file (getfiled "��� ������������ �����..." (getvar "DWGPREFIX") "kml" 1))
      (progn
	(setq squares(XML-Get-Child (XML-Get-Child o-doc nil "puncts") nil "squares"))
	(setq n 1)
	(foreach x (XML-Get-ChildList squares)
	  (foreach y (XML-Get-ChildList x)
	    (setq list-punct-ass-list (append list-punct-ass-list (list (convert-punkt-recorg-to-ass-list y))))
	    (if (=(rem n 100)0)(grtext -1 (strcat"�����"(itoa n))))
	    (setq n (1+ n))
	    )
;;;	  (gc)
	  
	  )
	(grtext -1 "")
	
	(setq list-strings (list
			     "<?xml version=\"1.0\" encoding=\"windows-1251\"?>"
			     "<kml xmlns=\"http://earth.google.com/kml/2.2\">"
			     "<Document>"
			     "  <name>MGGT</name>"
			     "  <description><![CDATA[������ ���������� �����������, ����� ������� 2012.10.23]]></description>"
			     "  <Style id=\"true-z\"><IconStyle><Icon><href>http://maps.gstatic.com/mapfiles/ms2/micons/red-dot.png</href></Icon></IconStyle></Style>"
			     "  <Style id=\"true\"><IconStyle><Icon><href>http://maps.gstatic.com/mapfiles/ms2/micons/red.png</href></Icon></IconStyle></Style>"
			     "  <Style id=\"false\"><IconStyle><Icon><href>http://maps.gstatic.com/mapfiles/ms2/micons/blue.png</href></Icon></IconStyle></Style>"
			     "  <Style id=\"false-z\"><IconStyle><Icon><href>http://maps.gstatic.com/mapfiles/ms2/micons/blue-dot.png</href></Icon></IconStyle></Style>"
			     )
	      )
	(setq n 1)
	(foreach z list-punct-ass-list
	  (cond
	    ((and (get-item-by-name-in-list z "year")(get-item-by-name-in-list z "Z"))
	     (setq style "true-z")
	     )
	    ((and (get-item-by-name-in-list z "year")(=(get-item-by-name-in-list z "Z")nil))
	     (setq style "true")
	     )
	    ((and (=(get-item-by-name-in-list z "year")nil)(get-item-by-name-in-list z "Z"))
	     (setq style "false-z")
	     )
	    (T
	     (setq style "false")
	     )
	    )
	  (setq deskription
		 (strcat
		   "N=" (get-item-by-name-in-list z "N") "<br>"
		   "E=" (get-item-by-name-in-list z "E") "<br>"
		   (if (/=(get-item-by-name-in-list z "Z")nil) (strcat "Z=" (get-item-by-name-in-list z "Z") "<br>") "")
		   (if (/=(get-item-by-name-in-list z "address")nil) (strcat "address=" (get-item-by-name-in-list z "address") "<br>") "")
		   (if (/=(get-item-by-name-in-list z "comment")nil) (strcat "comment=" (get-item-by-name-in-list z "comment") "<br>") "")
		   )
		)
	  (setq BL (convert-mggt-xy-to-wgs-84-bl (atof(get-item-by-name-in-list z "N"))(atof(get-item-by-name-in-list z "E"))))
	  (setq list-strings
		 (append list-strings
			 (list
			   (strcat"  <Placemark><name>" (get-item-by-name-in-list z "name")
				  "</name><description><![CDATA[" deskription
				  "]]></description><styleUrl>#" style
				  "</styleUrl><Point><coordinates>" (rtos(nth 1 BL)2 6) "," (rtos(nth 0 BL)2 6) ",0.000000</coordinates></Point></Placemark>")
			   )
			 )
		)
	  (if (=(rem n 100)0)(grtext -1 (strcat"������"(itoa n))))
	  (setq n (1+ n))
	  )
	(setq list-strings
	       (append list-strings
		       (list
			 "</Document>"
			 "</kml>"
			 )
		       )
	      )
	(grtext -1 "������...")
	(file_write_list_to_file put-file list-strings)
	(grtext -1 "")
	(princ (strcat "\n�������������� � ���� " (itoa(length list-strings)) " �����."))
	(if squares(vlax-release-object squares))
	(if o-doc(vlax-release-object o-doc))
	)
      )
    (alert "���� �������� ��������� ������� �� ���������!")
    )
  (princ)
  )