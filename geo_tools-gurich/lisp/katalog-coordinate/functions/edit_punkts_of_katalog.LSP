(defun C:edit_punkts_of_katalog ( / ATTRIBUTES O-DOC OBJ ASS-LIST PUNKT-RECORG)
  (if (setq o-doc(XML-Get-XMLObject (get-patch-to-katalog)))
    (progn
      (if(setq obj(car(entsel "\nВыбери блок пункта: ")))
	(progn
	  (setq obj(vlax-ename->vla-object obj))
	  (foreach x (get-all-attributes obj)
	    (if (not(=(nth 1 x)""))
	      (setq attributes (append attributes (list (list (nth 0 x) (vl-string-left-trim (strcat (nth 0 x)"=")(nth 1 x))))))
	      )
	    )
	  (setq punkt-recorg(get-punkt-recorg-by-ass-list attributes o-doc))
	  (setq ass-list(dialog-edit-punct-in-katalog
			  (convert-punkt-recorg-to-ass-list punkt-recorg)
			  (get-punct-type-list-in-katalog o-doc)
			  (get-attributs-list-in-katalog o-doc)))
	  (if ass-list
	    (progn
	      (add-puncts-in-katalog o-doc ass-list T)
	      (delete-punkt-recorg punkt-recorg T)
	      (vla-Delete obj)
	      )
	    )
	  )
	)
      (vlax-release-object o-doc)
      );progn
    (alert "Файл каталога координат пунктов не обнаружен!")
    )
  (princ)
  )

(defun get-punkt-recorg-by-ass-list (ass-list o-doc / N PUNKT-RECORG RESULT SQUARE SQUARES)
  (setq squares(XML-Get-Child (XML-Get-Child o-doc nil "puncts") nil "squares"))
  (setq square
	 (XML-Get-Children
	   (XML-Get-Child-ByAttribute squares nil "name"
	     (analisis-point-square
	       (extract-coor-of-ass-list ass-list))) nil))
  (setq n 0)
  (while (setq punkt-recorg (vlax-get-property square 'item n))
    (if (and (= (cadr(assoc"N"ass-list))(XML-Get-Attribute punkt-recorg "N" ""))
	     (= (cadr(assoc"E"ass-list))(XML-Get-Attribute punkt-recorg "E" "")))
      (progn
	(setq result punkt-recorg)
	(setq n (1+(vlax-get-property square 'length)))
	))
    (setq n (1+ n))
    )
  result
  )

(defun extract-coor-of-ass-list (ass-list / )
  (list (atof (cadr(assoc"E"ass-list)))
	(atof (cadr(assoc"N"ass-list)))
	)
  )