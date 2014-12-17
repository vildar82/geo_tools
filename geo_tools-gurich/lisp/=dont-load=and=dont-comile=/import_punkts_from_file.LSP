(defun C:import_punkts_from_file ( / list-points put-file childlist corrent-list-item name-new
				  name-old o-doc point punkt-old squares x-new x-old y-new y-old
				  punkt-new raport flag-type flag-year
				  flag-new n n-all result-punkt y z-only)
  ;|
   импорт пунктов из файла с форматированием:
   имя|X|Y|Z|тип центра|местоположение,адрес|год получения|примечание
  |;
  (terpri)
  (setq flag-type T
	flag-year T)
  (setq put-file (getfiled "Выбор файла" (getvar "DWGPREFIX")  "txt" 0))
  (if put-file
    (progn
      (foreach x (file_read_to_list put-file)
	(setq list-points(append list-points (list (analisis-ex-asslist-punkt (convert-string-to-ass-list x) flag-type flag-year)))))
      (setq raport (append raport (list (strcat"Пунктов в файле:"(itoa (length list-points))))))
;;;      (princ "\nПунктов в файле:")(princ(length list-points))(terpri)
      (if (setq o-doc(XML-Get-XMLObject (get-patch-to-katalog)))
	(progn
	  (setq squares(XML-Get-Child (XML-Get-Child o-doc nil "puncts") nil "squares"))
	  (setq z-only(XML-Get-Child (XML-Get-Child o-doc nil "puncts") nil "z-only"))
	  (setq n-all 1)
	  (foreach x list-points
	    (grtext -1 (strcat(itoa n-all)"/"(itoa(length list-points))))
	    (setq punkt-new x)
	    (if (and (cadr(assoc"E"x))(cadr(assoc"N"x)))
	      (progn
		(setq point (list (atof(cadr(assoc"E"x)))(atof(cadr(assoc"N"x)))))
		(setq corrent-list-item (XML-Get-Child-ByAttribute squares nil "name" (analisis-point-square point)))
		(if(not corrent-list-item)
		  (progn
		    (setq corrent-list-item (XML-Add-Child squares "square"))
		    (XML-Add-Attribute corrent-list-item "name" (analisis-point-square point))
		    )
		  )
		(setq ChildList(XML-Get-ChildList corrent-list-item))
		(setq raport (append raport (list (strcat "\nКвадрат :<" (analisis-point-square point) "> пунктов в квадрате:"(itoa (length ChildList))))))
;;;		(princ "\nКвадрат :<")
;;;		(princ(analisis-point-square point))
;;;		(princ "> пунктов в квадрате:")
;;;		(princ (length ChildList))(terpri)
		(setq n 0)
		(setq flag-new T)
		(setq y
		       (if ChildList
			 (nth n ChildList)
			 nil)
		      )
		(while y
;;;		  (princ(strcat"\r "(itoa (1+ n))"/" (itoa (length ChildList))"       "))
		  (setq punkt-old(convert-punkt-recorg-to-ass-list y))
		  (if (setq result-punkt (chek-punkt punkt-old punkt-new T))
		    (progn
		      (setq raport (append raport (list "Текущий пункт")))
		      (setq raport (append raport (list (convert-ass-list-to-string punkt-new))))
		      (setq raport (append raport (list "похож на пункт каталога")))
		      (setq raport (append raport (list (convert-ass-list-to-string punkt-old))))
		      (setq raport (append raport (list "заменить его на")))
		      (setq raport (append raport (list (convert-ass-list-to-string result-punkt))))
;;;		      (princ "\nТекущий пункт\n")
;;;		      (princ (convert-ass-list-to-string punkt-new))
;;;		      (princ "\nпохож на пункт каталога\n")
;;;		      (princ (convert-ass-list-to-string punkt-old))
;;;		      (princ "\nзаменить его на \n")
;;;		      (princ (convert-ass-list-to-string result-punkt))
		      (delete-punkt-recorg y);удалить пункт из каталога
		      (add-puncts-in-katalog o-doc result-punkt T);добавить пункт в каталог
		      (setq y nil)
		      (setq flag-new nil)
		      )
		    )
		  (setq n (1+ n))
		  (if y (setq y (nth n ChildList)))
		  )
		(if flag-new
		  (progn
		    (setq raport (append raport (list "Похожих пунктов не найдено. Добавить новый пункт.")))
		    (setq raport (append raport (list (convert-ass-list-to-string punkt-new))))
;;;		    (princ "\nПохожих пунктов не найдено. Добавить новый пункт.\n")
;;;		    (princ (convert-ass-list-to-string punkt-new))		    
		    (add-puncts-in-katalog o-doc punkt-new T);добавить пункт в каталог
		    )
		  )
		)
	      (if
		(cadr(assoc"Z"x))
		(progn
		  (setq ChildList(XML-Get-ChildList z-only))
		  (setq raport (append raport (list "\nПункт без планового положения")))
;;;		  (princ "\nПункт без планового положения\n")
		  (setq n 0)
		  (setq flag-new T)
		  (setq y
		       (if ChildList
			 (nth n ChildList)
			 nil)
		      )
		  (while y
;;;		    (if (=(rem n 10)0)
;;;		    (princ(strcat"\r "(itoa (1+ n))"/" (itoa (length ChildList))"       ")))
		    (setq punkt-old(convert-punkt-recorg-to-ass-list y))
		    (if (setq result-punkt (chek-punkt punkt-old punkt-new nil))
		      (progn
			(setq raport (append raport (list "Текущий пункт")))
			(setq raport (append raport (list (convert-ass-list-to-string punkt-new))))
			(setq raport (append raport (list "похож на пункт каталога")))
			(setq raport (append raport (list (convert-ass-list-to-string punkt-old))))
			(setq raport (append raport (list "заменить его на")))
			(setq raport (append raport (list (convert-ass-list-to-string result-punkt))))
;;;			(princ "\nТекущий пункт\n")
;;;			(princ (convert-ass-list-to-string punkt-new))
;;;			(princ "\nпохож на пункт каталога\n")
;;;			(princ (convert-ass-list-to-string punkt-old))
;;;			(princ "\nзаменить его на \n")
;;;			(princ (convert-ass-list-to-string result-punkt))
			(delete-punkt-recorg y);удалить пункт из каталога
			(add-puncts-in-katalog o-doc result-punkt T);добавить пункт в каталог
			(setq y nil)
			(setq flag-new nil)
			)
		      )
		    (setq n (1+ n))
		    (if y (setq y (nth n ChildList)))
		    )
		  (if flag-new
		  (progn
		    (setq raport (append raport (list "Похожих пунктов не найдено. Добавить новый пункт.")))
		    (setq raport (append raport (list (convert-ass-list-to-string punkt-new))))
;;;		    (princ "\nПохожих пунктов не найдено. Добавить новый пункт.\n")
;;;		    (princ (convert-ass-list-to-string punkt-new))
		    (add-puncts-in-katalog o-doc punkt-new T);добавить пункт в каталог
		    )
		  )
		  )
		(princ (strcat "\nВнимание у нового пункта отсутствует планово высотное положение, строка: " (itoa n-all)))
		)
	      )
	    (setq n-all (1+ n-all))
	    )
	  (grtext -1 "")
	  (vlax-release-object squares)
	  (vlax-release-object z-only)
	  (vlax-release-object corrent-list-item)
	  (XML-Save o-doc)
	  (vlax-release-object o-doc)
	  )
	(alert "Файл каталога координат пунктов не обнаружен!")
	)
      )
    )
  (if raport (file_write_list_to_file (strcat (vl-filename-directory put-file)"\\raport.txt") raport))
  (princ)
  )
  
;;;(bump)

