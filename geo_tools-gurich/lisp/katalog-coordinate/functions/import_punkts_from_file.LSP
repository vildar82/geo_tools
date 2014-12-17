(defun C:import_punkts_from_file ( / flag-file-name-messege kol-vo-str list-options put-file katalog-file flag-continue-calc file-list-katalog-files list-input-files)
  ;|
импорт пунктов из файла с форматированием:
   имя|X|Y|Z|тип центра|местоположение,адрес|год получения|примечание
  |;
  (setq list-options (dialog-import-punkts-optyons))
  (if list-options
    (progn
      (terpri)
      (setq kol-vo-str (get-item-by-name-in-list list-options "kol-vo-str")
	    flag-file-name-messege (if (=(get-item-by-name-in-list list-options "file-name-messege")1) T)
	    flag-continue-calc (if (=(get-item-by-name-in-list list-options "continue-calc")1) T))
      (setq put-file (getfiled "Выбор файла" (getvar "DWGPREFIX")  "txt" 0))
      (if put-file
	(progn
	  (if flag-continue-calc
	    (progn
	      (setq list-input-files(file_read_to_list put-file))
	      (setq file-list-katalog-files put-file)
	      )
	    (progn
	      (setq list-input-files(if (> kol-vo-str 0)(divide-txt-file-by-num-string put-file kol-vo-str flag-file-name-messege)(list put-file)))
	      (setq file-list-katalog-files (strcat (vl-filename-directory put-file)"\\file-list-katalog-files_"(vl-filename-base put-file)(vl-filename-extension put-file)))
	      (file_write_list_to_file file-list-katalog-files list-input-files)
	      (vl-file-copy file-list-katalog-files (strcat (vl-filename-directory file-list-katalog-files)
							    "\\bak-input-"
							    (vl-filename-base file-list-katalog-files)
							    (vl-filename-extension file-list-katalog-files)))
	      )
	    )
	  
	  (setq katalog-file (get-patch-to-katalog))
	  (vl-file-copy katalog-file (strcat (vl-filename-directory katalog-file)"\\bak-input-file-katalog-"(vl-filename-base katalog-file)(vl-filename-extension katalog-file)))
	  (foreach x list-input-files
	    (if (file-p x)
	      (progn
		(low-level-fun-import-punkts-from-file x list-options)
		(vl-file-copy katalog-file (strcat (vl-filename-directory katalog-file)"\\"(vl-filename-base katalog-file)"+"(vl-filename-base x)(vl-filename-extension katalog-file)))
		)
	      )
	    (file_write_list_to_file file-list-katalog-files (cdr(file_read_to_list file-list-katalog-files)))
	    )
	  )
	)
      )
    )
  (princ)
  )

(defun low-level-fun-import-punkts-from-file(put-file list-options / childlist corrent-list-item corrent-square-list-points
					     flag-draw-punkt flag-file-name-messege flag-new flag-type flag-year length-punkts
					     list-points n n-all name-square o-doc point punkt-new punkt-old put-to-katalog raport
					     result-punkt squares squares-asslist temp-list y z-only z-only-asslist messege-text)
  
  (setq flag-type (if (=(get-item-by-name-in-list list-options "flag-type")1) T)
	    flag-year (if (=(get-item-by-name-in-list list-options "flag-year")1) T)
	    flag-draw-punkt (if (=(get-item-by-name-in-list list-options "draw-new-punkt")1) T)
	    flag-file-name-messege (if (=(get-item-by-name-in-list list-options "file-name-messege")1) T)
	    )
  
  (if (= (get-item-by-name-in-list list-options "flag-memory-file") 0)
      
      (progn;                                    start file
	
	(foreach x (file-read-to-list-punkts put-file flag-file-name-messege)
	  (setq list-points(append list-points (list (analisis-ex-asslist-punkt (convert-string-to-ass-list x) flag-type flag-year))))
	  
	  )
	(setq length-punkts (length list-points))
	(setq raport (append raport (list (strcat"Пунктов в файле:"(itoa length-punkts)))))
	(if (setq o-doc(XML-Get-XMLObject (get-patch-to-katalog)))
	  (progn
	    (setq squares(XML-Get-Child (XML-Get-Child o-doc nil "puncts") nil "squares"))
	    (setq z-only(XML-Get-Child (XML-Get-Child o-doc nil "puncts") nil "z-only"))
	    (setq n-all 1)
	    (foreach x list-points
	      (setq messege-text (strcat"Обработка:"(itoa n-all)"/"(itoa length-punkts)))
	      (grtext -1 messege-text)
	      (if flag-file-name-messege(name-file-to-messege messege-text))
	      (setq punkt-new x)
	      (if (and (cadr(assoc"E"x))(cadr(assoc"N"x)))
		(progn
		  (setq point (list (atof(cadr(assoc"E"x)))(atof(cadr(assoc"N"x)))))
		  (setq name-square(analisis-point-square point));текущий квадрат
		  (setq corrent-list-item (XML-Get-Child-ByAttribute squares nil "name" name-square))
		  (if(not corrent-list-item)
		    (progn
		      (setq corrent-list-item (XML-Add-Child squares "square"))
		      (XML-Add-Attribute corrent-list-item "name" name-square)
		      )
		    )
		  (setq ChildList(XML-Get-ChildList corrent-list-item))
		  (setq raport (append raport (list (strcat "\nКвадрат :<" name-square "> пунктов в квадрате:"(itoa (length ChildList))))))
		  (setq n 0)
		  (setq flag-new T)
		  (setq y
			 (if ChildList
			   (nth n ChildList)
			   nil)
			)
		  (while y
		    (setq punkt-old(convert-punkt-recorg-to-ass-list y))
		    (if (setq result-punkt (chek-punkt punkt-old punkt-new T))
		      (progn
			(setq raport (append raport (list "Текущий пункт")))
			(setq raport (append raport (list (convert-ass-list-to-string punkt-new))))
			(setq raport (append raport (list "похож на пункт каталога")))
			(setq raport (append raport (list (convert-ass-list-to-string punkt-old))))
			(setq raport (append raport (list "заменить его на")))
			(setq raport (append raport (list (convert-ass-list-to-string result-punkt))))
			(delete-punkt-recorg y T);удалить пункт из каталога
			(add-puncts-in-katalog o-doc result-punkt flag-draw-punkt);добавить пункт в каталог
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
		      (add-puncts-in-katalog o-doc punkt-new flag-draw-punkt);добавить пункт в каталог
		      )
		    )
		  )
		(if
		  (cadr(assoc"Z"x))
		  (progn
		    (setq ChildList(XML-Get-ChildList z-only))
		    (setq raport (append raport (list "\nПункт без планового положения")))
		    (setq n 0)
		    (setq flag-new T)
		    (setq y
			   (if ChildList
			     (nth n ChildList)
			     nil)
			  )
		    (while y
		      (setq punkt-old(convert-punkt-recorg-to-ass-list y))
		      (if (setq result-punkt (chek-punkt punkt-old punkt-new nil))
			(progn
			  (setq raport (append raport (list "Текущий пункт")))
			  (setq raport (append raport (list (convert-ass-list-to-string punkt-new))))
			  (setq raport (append raport (list "похож на пункт каталога")))
			  (setq raport (append raport (list (convert-ass-list-to-string punkt-old))))
			  (setq raport (append raport (list "заменить его на")))
			  (setq raport (append raport (list (convert-ass-list-to-string result-punkt))))
			  (delete-punkt-recorg y T);удалить пункт из каталога
			  (add-puncts-in-katalog o-doc result-punkt flag-draw-punkt);добавить пункт в каталог
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
			(add-puncts-in-katalog o-doc punkt-new flag-draw-punkt);добавить пункт в каталог
			)
		      )
		    )
		  (progn
		    (setq raport (append raport (list (strcat "\nВнимание у нового пункта отсутствует планово высотное положение, строка: " (itoa n-all)))))
		    (setq raport (append raport (list (convert-ass-list-to-string punkt-new))))
		    )
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
	);                                                          end file
      (progn;                                                        start memory
	(foreach x (file-read-to-list-punkts put-file flag-file-name-messege)
	  (setq list-points(append list-points (list (analisis-ex-asslist-punkt (convert-string-to-ass-list x) flag-type flag-year)))))
	(setq length-punkts (length list-points))
	(setq raport (append raport (list (strcat"Пунктов в файле:"(itoa length-punkts)))))
	
	(if (file-p(setq put-to-katalog(get-patch-to-katalog)))
	  (progn
	    (setq temp-list (convert-xml-to-asslist-puncts (XML-Get-XMLObject put-to-katalog)flag-file-name-messege))
	    (setq squares-asslist (nth 0 temp-list))
	    (setq z-only-asslist (nth 1 temp-list))
	    (setq n-all 1)
	    (foreach x list-points
	      (setq messege-text (strcat"Обработка:"(itoa n-all)"/"(itoa length-punkts)))
	      (grtext -1 messege-text)
	      (if flag-file-name-messege(name-file-to-messege messege-text))
	      (setq punkt-new x)
	      (if (and (cadr(assoc"E"x))(cadr(assoc"N"x)))
		(progn
		  (setq point (list (atof(cadr(assoc"E"x)))(atof(cadr(assoc"N"x)))))
		  (setq name-square(analisis-point-square point));текущий квадрат
		  (setq corrent-square-list-points (get-item-by-name-in-list squares-asslist name-square));содержимое текущего квадрата
		  (if(not corrent-square-list-points)
		    (setq squares-asslist (put-item-by-name-in-list squares-asslist name-square nil)); если такого квадрата нет создаем пустой
		    )
		  
		  (setq raport (append raport (list (strcat "\nКвадрат :<" name-square "> пунктов в квадрате:"(itoa (length corrent-square-list-points))))))
		  (setq n 0)
		  (setq flag-new T)
		  (setq y
			 (if corrent-square-list-points
			   (nth n corrent-square-list-points)
			   nil)
			)
		  (while y
		    (setq punkt-old y)
		    (if (setq result-punkt (chek-punkt punkt-old punkt-new T))
		      (progn
			(setq raport (append raport (list "Текущий пункт")))
			(setq raport (append raport (list (convert-ass-list-to-string punkt-new))))
			(setq raport (append raport (list "похож на пункт каталога")))
			(setq raport (append raport (list (convert-ass-list-to-string punkt-old))))
			(setq raport (append raport (list "заменить его на")))
			(setq raport (append raport (list (convert-ass-list-to-string result-punkt))))
			(setq squares-asslist (put-item-by-name-in-list
						squares-asslist
						name-square (edit-list-del-item-by-number n(get-item-by-name-in-list squares-asslist name-square))
						)
			      );удалить пункт из каталога
			(setq squares-asslist (put-item-by-name-in-list
						squares-asslist
						name-square (append (get-item-by-name-in-list squares-asslist name-square) (list result-punkt))
						)
			      );добавить пункт в каталог
			(setq y nil)
			(setq flag-new nil)
			)
		      )
		    (setq n (1+ n))
		    (if y (setq y (nth n corrent-square-list-points)))
		    )
		  (if flag-new
		    (progn
		      (setq raport (append raport (list "Похожих пунктов не найдено. Добавить новый пункт.")))
		      (setq raport (append raport (list (convert-ass-list-to-string punkt-new))))
		      (setq squares-asslist (put-item-by-name-in-list
					      squares-asslist
					      name-square (append (get-item-by-name-in-list squares-asslist name-square) (list punkt-new))
					      )
			    );добавить пункт в каталог
		      )
		    )
		  )
		(if
		  (cadr(assoc"Z"x))
		  (progn
		    (setq raport (append raport (list "\nПункт без планового положения")))
		    (setq n 0)
		    (setq flag-new T)
		    (setq y
			   (if z-only-asslist
			     (nth n z-only-asslist)
			     nil)
			  )
		    (while y
		      (setq punkt-old y)
		      (if (setq result-punkt (chek-punkt punkt-old punkt-new nil))
			(progn
			  (setq raport (append raport (list "Текущий пункт")))
			  (setq raport (append raport (list (convert-ass-list-to-string punkt-new))))
			  (setq raport (append raport (list "похож на пункт каталога")))
			  (setq raport (append raport (list (convert-ass-list-to-string punkt-old))))
			  (setq raport (append raport (list "заменить его на")))
			  (setq raport (append raport (list (convert-ass-list-to-string result-punkt))))
			  (setq z-only-asslist(edit-list-del-item-by-number n z-only-asslist));удалить пункт из каталога
			  (setq z-only-asslist(append z-only-asslist (list result-punkt)));добавить пункт в каталог
			  (setq y nil)
			  (setq flag-new nil)
			  )
			)
		      (setq n (1+ n))
		      (if y (setq y (nth n z-only-asslist)))
		      )
		    (if flag-new
		      (progn
			(setq raport (append raport (list "Похожих пунктов не найдено. Добавить новый пункт.")))
			(setq raport (append raport (list (convert-ass-list-to-string punkt-new))))
			(setq z-only-asslist(append z-only-asslist (list punkt-new)));добавить пункт в каталог
			)
		      )
		    )
		  (progn
		    (setq raport (append raport (list (strcat "\nВнимание у нового пункта отсутствует планово высотное положение, строка: " (itoa n-all)))))
		    (setq raport (append raport (list (convert-ass-list-to-string punkt-new))))
		    )
		  )
		)
	      (setq n-all (1+ n-all))
	      )
	    (grtext -1 "")
	    ;;;		(convert-asslist-puncts-to-txt put-to-katalog (list squares-asslist z-only-asslist)flag-file-name-messege)
	    (convert-asslist-puncts-to-xml put-to-katalog (list squares-asslist z-only-asslist)flag-draw-punkt flag-file-name-messege)
	    )
	  (alert "Файл каталога координат пунктов не обнаружен!")
	  )
	);                                                      end memory
      )
  (delete-name-file-to-messege)
  (if raport (file_write_list_to_file (strcat (vl-filename-directory put-file)"\\"(vl-filename-base put-file)"_raport.txt") raport ))
  )
  
;;;(bump)

(defun dialog-import-punkts-optyons ( / list-optyons flag dcl_id)
  (setq list-optyons (list
		       (list "flag-type" 1)
		       (list "flag-year" 1)
		       (list "flag-memory-file" 0); 1 - память, 0 - файл
		       (list "draw-new-punkt" 0)
		       (list "file-name-messege" 1)
		       (list "kol-vo-str" 1000)
		       (list "continue-calc" 0)
		       ))
  
  (setq dcl_id (geo-tools-load-dialog "import_punkts_optyons"))
  (new_dialog "import_punkts_optyons" dcl_id)
  
  (action_tile "flag-type"
    (vl-prin1-to-string
      '(setq list-optyons(put-item-by-name-in-list list-optyons "flag-type" (atoi(get_tile "flag-type" ))))))
  (action_tile "flag-year"
    (vl-prin1-to-string
      '(setq list-optyons(put-item-by-name-in-list list-optyons "flag-year" (atoi(get_tile "flag-year" ))))))
  (action_tile "draw-new-punkt"
    (vl-prin1-to-string
      '(setq list-optyons(put-item-by-name-in-list list-optyons "draw-new-punkt" (atoi(get_tile "draw-new-punkt" ))))))
  (action_tile "memory"
    (vl-prin1-to-string
      '(setq list-optyons(put-item-by-name-in-list list-optyons "flag-memory-file" 1))))
  (action_tile "file"
    (vl-prin1-to-string
      '(setq list-optyons(put-item-by-name-in-list list-optyons "flag-memory-file" 0))))
  (action_tile "file-name-messege"
    (vl-prin1-to-string
      '(setq list-optyons(put-item-by-name-in-list list-optyons "file-name-messege" (atoi(get_tile "file-name-messege" ))))))
  (action_tile "kol-vo-str"
    (vl-prin1-to-string
      '(setq list-optyons(put-item-by-name-in-list list-optyons "kol-vo-str" (abs(atoi(get_tile "kol-vo-str" )))))))
  (action_tile "continue-calc"
    (vl-prin1-to-string
      '(setq list-optyons(put-item-by-name-in-list list-optyons "continue-calc" (atoi(get_tile "continue-calc" ))))))
  (if(>(start_dialog)0)(setq flag T))
  (unload_dialog dcl_id)
  (if flag list-optyons)
  )


(defun convert-xml-to-asslist-puncts (o-doc flag-file-name-messege / n squares squares-asslist temp-list z-only z-only-asslist messege-text)
  (setq squares(XML-Get-Child (XML-Get-Child o-doc nil "puncts") nil "squares"))
  (setq z-only(XML-Get-Child (XML-Get-Child o-doc nil "puncts") nil "z-only"))
  (setq n 1)
  (foreach x (XML-Get-ChildList squares)
    (setq temp-list nil)
    (foreach y (XML-Get-ChildList x)
      (setq temp-list (append temp-list (list (convert-punkt-recorg-to-ass-list y))))
      (if (=(rem n 10)0)
	(progn
	  (setq messege-text(strcat"Читаю xml:"(itoa n)))
	  (grtext -1 messege-text)
	  (if flag-file-name-messege messege-text)
	  )
	)
      (setq n (1+ n))
      )
    (setq squares-asslist (append squares-asslist (list (list (XML-Get-Attribute x "name" "")temp-list))))
    )
  (foreach z (XML-Get-ChildList z-only)
    (setq z-only-asslist (append z-only-asslist (list (convert-punkt-recorg-to-ass-list z))))
    (if (=(rem n 10)0)
      (progn
	(setq messege-text (strcat"Читаю xml:"(itoa n)))
	(grtext -1 messege-text)
	(if flag-file-name-messege(name-file-to-messege messege-text))
	)
      )
    (setq n (1+ n))
    )
  (vlax-release-object o-doc)
  (list squares-asslist z-only-asslist)
  )

(defun convert-asslist-puncts-to-xml (put-to-katalog input-list flag-draw-punkt flag-file-name-messege / n squares squares-asslist temp-list z-only z-only-asslist o-doc length-all list-all messege-text)
  (if (file-p put-to-katalog)
    (vl-file-delete put-to-katalog))
  (vl-file-copy (strcat *geo_tools_root_dir*"\\template\\blank.xml") put-to-katalog)
  (setq o-doc(XML-Get-XMLObject put-to-katalog))
  
  
  (setq squares-asslist (nth 0 input-list)
	z-only-asslist (nth 1 input-list))
  (setq input-list nil)
  (setq n 1)
  (foreach x squares-asslist
    (setq x (nth 1 x))
    (foreach y x
      (setq list-all (append list-all (list y)))
      (if (=(rem n 100)0)
	(progn
	  (setq messege-text (strcat"Привожу к общему списку:"(itoa n)))
	  (grtext -1 messege-text)
	  (if flag-file-name-messege(name-file-to-messege messege-text))
	  )
	)
      (setq n (1+ n))
      )
    )
  (setq list-all (append list-all z-only-asslist))
  (setq squares-asslist nil
	z-only-asslist nil)
  (setq length-all (length list-all))
  (setq n 1)
  (foreach a list-all
    (add-puncts-in-katalog o-doc a flag-draw-punkt);добавить пункт в каталог
    (if (=(rem n 10)0)
      (progn
	(setq messege-text(strcat"Записываю в xml:"(itoa n)"/"(itoa length-all)))
	(grtext -1 messege-text)
	(if flag-file-name-messege(name-file-to-messege messege-text))
	)
      )
    
    (setq n (1+ n))
    )
  (grtext -1 "")
  
  (XML-Save o-doc)
  (vlax-release-object o-doc)
  (princ)
  )

(defun convert-asslist-puncts-to-txt (put-to-katalog input-list flag-file-name-messege / list-all n squares-asslist z-only-asslist messege-text)
  (setq n 1)
  (setq squares-asslist (nth 0 input-list)
	z-only-asslist (nth 1 input-list))
  
  (foreach x squares-asslist
    (setq x (nth 1 x))
    (foreach y x
      (setq list-all(append list-all (list (convert-ass-list-to-string y))))
      (if (=(rem n 100)0)
	(progn
	  (setq messege-text(strcat"Конвертирую в текст"(itoa n)))
	  (grtext -1 messege-text)
	  (if flag-file-name-messege(name-file-to-messege messege-text))
	  ))
      (setq n (1+ n))
      )
    )
  (foreach z z-only-asslist
    (setq list-all(append list-all (list (convert-ass-list-to-string z))))
    (if (=(rem n 100)0)
      (progn
	(setq messege-text (strcat"Конвертирую в текст"(itoa n)))
	(grtext -1 messege-text)
	(if flag-file-name-messege(name-file-to-messege messege-text))
	)
      )
    (setq n (1+ n))
    )
  (if list-all (file_write_list_to_file (strcat (vl-filename-directory put-to-katalog)"\\result.txt") list-all))
  (princ)
  )


(defun file-read-to-list-punkts (patch flag-file-name-messege / file list_cell cell n messege-text)
  ; файл -> список
  (setq n 0)
  (if (file_check patch)
    (progn
      (setq file(open patch "r"))
      (setq cell (read-line file))
      (while cell
	(if (=(rem n 100)0)
	  (progn
	    (setq messege-text(strcat"Читаю txt:"(itoa n)))
	    (grtext -1 messege-text)
	    (if flag-file-name-messege(name-file-to-messege messege-text))
	    )
	  )
	(setq n (1+ n))
	(setq list_cell(append list_cell (list cell)))
	(setq cell (read-line file)))
      (grtext -1 "")
      (close file)
      list_cell
      )
    nil
    )
  )

(defun name-file-to-messege (maessege / )
  (foreach z (list ":" "/" ";" "|" "\\")
    (setq maessege(edit-string-replase-str "-" z maessege))
    )
  
  (if (file-p *filename-massege*)
    (vl-file-delete *filename-massege*))
  (setq *filename-massege* (strcat (getvar "DWGPREFIX")maessege))
  (file_write_list_to_file *filename-massege* (list " -===- "))
  (princ)
  )
(defun delete-name-file-to-messege ()
  (if (file-p *filename-massege*)
    (vl-file-delete *filename-massege*))
  (setq *filename-massege* nil)
  (princ)
  )