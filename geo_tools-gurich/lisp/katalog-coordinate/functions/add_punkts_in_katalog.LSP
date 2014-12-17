(defun C:add_puncts_in_katalog ( / ass-list corrent-list-item squares o-doc point record)
  (if (setq o-doc(XML-Get-XMLObject (get-patch-to-katalog)))
    (progn
      (setq ass-list(dialog-edit-punct-in-katalog
		      nil
		      (get-punct-type-list-in-katalog o-doc)
		      (get-attributs-list-in-katalog o-doc)))
      (if ass-list(add-puncts-in-katalog o-doc ass-list T))
      (vlax-release-object o-doc)
      )
    (alert "Файл каталога координат пунктов не обнаружен!")
    )
  (princ)
  )

(defun add-puncts-in-katalog (o-doc ass-list draw-flag / corrent-list-item point record squares z-only)
  (setq squares(XML-Get-Child (XML-Get-Child o-doc nil "puncts") nil "squares"))
  (setq z-only(XML-Get-Child (XML-Get-Child o-doc nil "puncts") nil "z-only"))
  (if
    (and (cadr(assoc"E"ass-list))(cadr(assoc"N"ass-list)))
    (progn
      (setq point (list (atof(cadr(assoc"E"ass-list)))(atof(cadr(assoc"N"ass-list)))))
      (setq corrent-list-item (XML-Get-Child-ByAttribute squares nil "name" (analisis-point-square point)))
      (if(not corrent-list-item)
	(progn
	  (setq corrent-list-item (XML-Add-Child squares "square"))
	  (XML-Add-Attribute corrent-list-item "name" (analisis-point-square point))
	  )
	)
      (setq record (convert-ass-list-to-punkt-recorg corrent-list-item ass-list))
      (if draw-flag (draw-punkt-by-record record (get-punct-type-list-in-katalog o-doc)))
      )
    (setq record (convert-ass-list-to-punkt-recorg z-only ass-list))
    )
  (if squares(vlax-release-object squares))
  (if z-only(vlax-release-object z-only))
  (if corrent-list-item(vlax-release-object corrent-list-item))
  (if record(vlax-release-object record))
  (XML-Save o-doc)
  )

(defun dialog-edit-punct-in-katalog (ass-list type-list attributs-list / att-list dcl_id num x)
  (setq type-list(mapcar '(lambda (x) (car x))type-list))
  (setq attributs-list(mapcar '(lambda (x) (car x))attributs-list))
  (setq dcl_id (geo-tools-load-dialog "d_edit_punkts"))
  (new_dialog "d_edit_punkts" dcl_id)
  (if ass-list
    (progn
      (set_tile "top" "Редактирование записи пункта")
      (foreach x ass-list
	(cond
	  ((= (car x) "type")
	   (if(setq num (analysis-number-item-in-list (cadr x) type-list))
	     (set_tile "type" (itoa (1+ num)))
	     )
	   )
	  (t
	   (set_tile (car x) (cadr x))
	   )
	  )
	)
      )
    (set_tile "top" "Создание записи пункта")
    )
  (start_list "type")
  (add_list "не определен")
  (mapcar 'add_list type-list)
  (end_list)
  (action_tile "save"
    (vl-prin1-to-string
      '(progn
	(foreach x attributs-list
	 (cond
	  ((= x "type")
	   (if (/=(get_tile "type")"0")
	    (setq att-list (append att-list (list(list "type" (nth (1-(atoi(get_tile "type"))) type-list)))))
	    )
	   )
	  ((/= (setq value (get_tile x)) "")
	   (setq att-list (append att-list (list(list x (get_tile x)))))
	   )
	  );cond
	 );foreach
	(cond
	 ((not(cadr(assoc"name"att-list)))
	  (alert"Внимание!\nНедостаточно данных! Необходимо имя пункта!")
	  (setq att-list nil))
	 ((not(cadr(assoc"N"att-list)))
	  (alert"Внимание!\nНедостаточно данных! Нет Северной координаты!")
	  (setq att-list nil))
	 ((not(cadr(assoc"E"att-list)))
	  (alert"Внимание!\nНедостаточно данных! Нет Восточной координаты!")
	  (setq att-list nil))
	 (T
	  (done_dialog 2))
	 )
	)
      )
    )
  (if (eq (start_dialog) 2)
    att-list
    nil
    )
  )


(defun get-punct-type-list-in-katalog ( o-doc / N PUNCT-TYPE RESULT-LIST TYPE-PUNCTS)
  (setq type-puncts(XML-Get-Children(XML-Get-Child (XML-Get-Child o-doc nil "properties") nil "type-puncts")nil))
  (setq n 0)
  (while (setq punct-type (vlax-get-property type-puncts 'item n))
    (setq result-list (append result-list
			      (list
				(list(XML-Get-Attribute punct-type "name" "")
				     (XML-Get-Attribute punct-type "block-name" "")
				     ))))
    (setq n (1+ n))
    )
  result-list
  )

(defun get-attributs-list-in-katalog ( o-doc / N PUNCT-TYPE RESULT-LIST TYPE-PUNCTS)
  (setq attributs(XML-Get-Children(XML-Get-Child (XML-Get-Child o-doc nil "properties") nil "attributs")nil))
  (setq n 0)
  (while (setq attribut (vlax-get-property attributs 'item n))
    (setq result-list (append result-list
			      (list(list(XML-Get-Attribute attribut "name" "")
					(XML-Get-Attribute attribut "comment" "")
					))))
    (setq n (1+ n))
    )
  result-list
  )



;;;(defun Viewport_Dimensions (/ actDoc actVp)
;;;(vl-load-com)
;;;  (setq actDoc(vla-get-ActiveDocument
;;;		 (vlax-get-acad-object)))
;;;  (vla-put-ActiveSpace actDoc acPaperSpace)
;;;  (vla-put-ActiveSpace actDoc acModelSpace)
;;;       (setq actVp(vla-get-ActiveViewport actDoc))
;;;  (list
;;;	(vla-get-Width actVp)
;;;	(vla-get-Height actVp)
;;;    ); end list
;;;  ); end of Vieport_Dimensions


;;;(setq actVp(vla-get-ActiveViewport active_document))
;;;(vla-put-Target (vla-get-ActiveViewport active_document) (vlax-3D-point '(0 0 0)))
;;;(vla-put-Center (vla-get-ActiveViewport active_document) (vlax-3D-point (list 0 0 0)))
;;;(vlax-dump-object (vla-get-ActiveViewport active_document) T)