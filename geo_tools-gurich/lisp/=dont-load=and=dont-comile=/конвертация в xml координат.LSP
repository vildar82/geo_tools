(defun C:aaa ( / block blocks coordinates elevation leng n name result)
  ;преобразую "спрятаные" в блоках пункты в текстовый файл каталога координат
  (terpri)
  (setq Blocks (vla-get-Blocks(vla-get-ActiveDocument(vlax-get-acad-object))))
  (setq n 0)
  (setq leng(vla-get-Count Blocks))
  (repeat leng
    (princ (strcat"\r "(itoa n)"/" (itoa leng)))
    (setq block(vla-item Blocks n))
    (if (vl-string-search "BLCK" (vla-get-name block))
      (progn
	(setq coordinates(vlax-safearray->list(vlax-variant-value(vla-get-Coordinates(vla-item block 2)))))
	(setq name(vla-get-TextString(vla-item block 0)))
	(setq elevation(vla-get-TextString(vla-item block 1)))
	(setq result (append result (list
				      (convert-list-to-string-be-delimiter (list
									     name
									     (rtos(nth 1 coordinates)2 3)
									     (rtos(nth 0 coordinates)2 3)
									     elevation
									     ""
									     ""
									     ""
									     ""
									     )"|" )
				      )))
	)
      )
    (setq n (1+ n))
    )
  (file_write_list_to_file (strcat(getvar "DWGPREFIX")"test.txt") result)
  (princ)
  )

(defun C:ccc ( / input-file list-item-all list-item-new-coor list-item-no-xy-coor list-point n o-doc point-item string)
  ;конвертирую "правильный" текстовый файл координат в формат xml с отделением пунктов без координат XY в отдельный список
  (setq o-doc (XML-create-blank-file(strcat(getvar "DWGPREFIX")"большой каталог координат.xml")))
  
  (setq list-item-no-xy-coor (XML-Add-Child o-doc "item"))
  (XML-Add-Attribute list-item-no-xy-coor "name" "no-xy-coor")
  
  (setq list-item-all (XML-Add-Child o-doc "item"))
  (XML-Add-Attribute list-item-all "name" "all")
  
  (setq list-item-new-coor (XML-Add-Child o-doc "item"))
  (XML-Add-Attribute list-item-new-coor "name" "new-coor")
  
  (setq input-file (strcat(getvar "DWGPREFIX")"no-xy-coor.txt"))
  (if (file-p input-file)
    (progn
      (setq n 1)
      (setq input-file(open input-file "r"))
      (setq string (read-line input-file))
      (while string
	(grtext -1 (itoa n))
	(setq list-point (convert-string-to-list-be-delimiter string "|"))
	(setq point-item (XML-Add-Child list-item-no-xy-coor "item"))
	(if (/= (nth 0 list-point) "")(XML-Add-Attribute point-item "name" (nth 0 list-point)))
	(if (/=(atof(nth 1 list-point))0)(XML-Add-Attribute point-item "N" (rtos(atof(nth 1 list-point))2 3)))
	(if (/=(atof(nth 2 list-point))0)(XML-Add-Attribute point-item "E" (rtos(atof(nth 2 list-point))2 3)))
	(if (/=(atof(nth 3 list-point))0)(XML-Add-Attribute point-item "Z" (rtos(atof(nth 3 list-point))2 3)))
	(if (/= (nth 4 list-point) "")(XML-Add-Attribute point-item "type" (nth 4 list-point)))
	(if (/= (nth 5 list-point) "")(XML-Add-Attribute point-item "address" (nth 5 list-point)))
	(if (/= (nth 6 list-point) "")(XML-Add-Attribute point-item "year" (nth 6 list-point)))
	(if (/= (nth 7 list-point) "")(XML-Add-Attribute point-item "comment" (nth 7 list-point)))
	(vlax-release-object point-item)
	(setq string (read-line input-file))
	(setq n (1+ n))
	)
      (close input-file)
      )
    (alert "Ошибка! Исходный файл координат no-xy-coor.txt не обнаружен!")
    )
  
  (setq input-file (strcat(getvar "DWGPREFIX")"каталог координат.txt"))
  (if (file-p input-file)
    (progn
      (setq n 1)
      (setq input-file(open input-file "r"))
      (setq string (read-line input-file))
      (while string
	(grtext -1 (itoa n))
	(setq list-point (convert-string-to-list-be-delimiter string "|"))
	(if(not(and (=(atof(nth 1 list-point))0)(=(atof(nth 2 list-point))0)))
	  (progn
	    (setq point-item (XML-Add-Child list-item-all "item"))
	    (if (/= (nth 0 list-point) "")(XML-Add-Attribute point-item "name" (nth 0 list-point)))
	    (if (/=(atof(nth 1 list-point))0)(XML-Add-Attribute point-item "N" (rtos(atof(nth 1 list-point))2 3)))
	    (if (/=(atof(nth 2 list-point))0)(XML-Add-Attribute point-item "E" (rtos(atof(nth 2 list-point))2 3)))
	    (if (/=(atof(nth 3 list-point))0)(XML-Add-Attribute point-item "Z" (rtos(atof(nth 3 list-point))2 3)))
	    (if (/= (nth 4 list-point) "")(XML-Add-Attribute point-item "type" (nth 4 list-point)))
	    (if (/= (nth 5 list-point) "")(XML-Add-Attribute point-item "address" (nth 5 list-point)))
	    (if (/= (nth 6 list-point) "")(XML-Add-Attribute point-item "year" (nth 6 list-point)))
	    (if (/= (nth 7 list-point) "")(XML-Add-Attribute point-item "comment" (nth 7 list-point)))
	    (vlax-release-object point-item)
	    )
	  )
	(setq string (read-line input-file))
	(setq n (1+ n))
	)
      (close input-file)
      )
    (alert "Ошибка! Исходный файл координат каталог координат.txt не обнаружен!")
    )
  
  (setq input-file (strcat(getvar "DWGPREFIX")"доп.каталог.txt"))
  (if (file-p input-file)
    (progn
      (setq n 1)
      (setq input-file(open input-file "r"))
      (setq string (read-line input-file))
      (while string
	(grtext -1 (itoa n))
	(setq list-point (convert-string-to-list-be-delimiter string "|"))
	(setq point-item (XML-Add-Child list-item-new-coor "item"))
	(if (/= (nth 0 list-point) "")(XML-Add-Attribute point-item "name" (nth 0 list-point)))
	(if (/=(atof(nth 1 list-point))0)(XML-Add-Attribute point-item "N" (rtos(atof(nth 1 list-point))2 3)))
	(if (/=(atof(nth 2 list-point))0)(XML-Add-Attribute point-item "E" (rtos(atof(nth 2 list-point))2 3)))
	(if (/=(atof(nth 3 list-point))0)(XML-Add-Attribute point-item "Z" (rtos(atof(nth 3 list-point))2 3)))
	(if (/= (nth 4 list-point) "")(XML-Add-Attribute point-item "type" (nth 4 list-point)))
	(if (/= (nth 5 list-point) "")(XML-Add-Attribute point-item "address" (nth 5 list-point)))
	(if (/= (nth 6 list-point) "")(XML-Add-Attribute point-item "year" (nth 6 list-point)))
	(if (/= (nth 7 list-point) "")(XML-Add-Attribute point-item "comment" (nth 7 list-point)))
	(vlax-release-object point-item)
	(setq string (read-line input-file))
	(setq n (1+ n))
	)
      (close input-file)
      )
    (alert "Ошибка! Исходный файл координат доп.каталог.txt не обнаружен!")
    )
  
  (XML-Save o-doc)
  (vlax-release-object list-item-no-xy-coor)
  (vlax-release-object list-item-all)
  (vlax-release-object list-item-new-coor)
  (vlax-release-object o-doc)
  (princ)
  )

(defun C:rrr ( / input-file list-point list-points output-file
	      raw-list-point raw-list-points result string n year description pos comment adress leng type-znak)
  ;конвертация файла исходных координат(имя|X|Y|Z|D) в geo_tools формат пунктов (имя|X|Y|Z|тип центра|местоположение,адрес|год получения|примечание)
  (setq input-file (strcat(getvar "DWGPREFIX")"Исходный каталог координат.txt"))
  (setq output-file (strcat(getvar "DWGPREFIX")"Готовый каталог координат.txt"))
  (if (file-p input-file)
    (progn
      (setq n 0)
      (setq input-file(open input-file "r"))
      (setq string (read-line input-file))
      (while string
	(if (vl-string-search "г.)" string)
	  (progn
	    (princ (strcat"\r "(itoa n)))
	    (setq raw-list-point (convert-string-to-list-be-delimiter string "|"))
	    (setq list-point
		   (list (nth 0 raw-list-point);имя
			 (nth 1 raw-list-point);X
			 (nth 2 raw-list-point);Y
			 (nth 3 raw-list-point);Z
			 ))
	    (setq description (nth 4 raw-list-point))
	    (setq year (if (setq pos (vl-string-search "г.)" description))
			 (substr description (- pos 3) 4)
			 ""
			 ))
	    (setq type-znak (if (wcmatch description "ст`.репер*,гор`.марка*,комп`.знак*,местн`.предмет*,ПЗ*")
			      (substr description 1 (vl-string-search " " description))
			      ""
			      ))
	    (setq comment (if (=(+(vl-string-search "г.)" description)3)(strlen description))
			    ""
			    (vl-string-trim " " (substr description (+(vl-string-search "г.)" description)4)))))
	    (setq adress description)
	    (if (not(= type-znak ""))
	      (setq adress (vl-string-trim " " (vl-string-subst "" type-znak adress))))
	    (if (not(= comment ""))
	      (setq adress (vl-string-trim " " (vl-string-subst "" comment adress))))
	    (if (not(= year ""))
	      (setq adress (vl-string-trim " " (vl-string-subst "" (strcat "("year"г.)") adress))))
	    (setq list-point(append list-point(list type-znak adress year comment)))
	    (setq list-points(append list-points (list list-point)))
	    )
	  (progn
	    (setq raw-list-point (convert-string-to-list-be-delimiter string "|"))
	    (setq list-point
		   (list (nth 0 raw-list-point);имя
			 (nth 1 raw-list-point);X
			 (nth 2 raw-list-point);Y
			 (nth 3 raw-list-point);Z
			 "";тип центра; ст.репер гор.марка комп.знак ПЗ
			 (nth 4 raw-list-point);местоположение,адрес
			 "";год получения
			 "";примечание
			 ))
	    (setq list-points(append list-points (list list-point)))
	    )
	  )
	(setq string (read-line input-file))
	(setq n (1+ n))
	)
      (close input-file)
      (terpri)
      (setq n 1)
      (setq leng (itoa(length list-points)))
      (setq output-file (open output-file "w"))
      (repeat (length list-points)
	(princ (strcat"\r "(itoa n)"/" leng "    "))
	(write-line (convert-list-to-string-be-delimiter (car list-points) "|" ) output-file)
	(setq list-points (cdr list-points))
	(setq n (1+ n))
	)
      (close output-file)
      )
    (alert "Ошибка! Исходный файл координат не обнаружен!")
    )
  (princ)
  )

(defun C:na ( / list-strings o-doc corrent-list-item list-item-no-xy point-item point)
  ;конвертирую "правильный" текстовый файл координат в формат xml с раскладыванием пунктов по квадратам 10х10 км
  (setq o-doc (XML-create-blank-file(strcat(getvar "DWGPREFIX")"Каталог координат.xml")))
  (setq list-strings (file_read_to_list(strcat(getvar "DWGPREFIX")"Каталог координат.txt")))
  (setq list-item-no-xy (XML-Add-Child o-doc "item"))(XML-Add-Attribute list-item-no-xy "name" "только отметка")
  (foreach x list-strings
    (setq list-point (convert-string-to-list-be-delimiter x "|"))
    (setq point (list (atof(nth 1 list-point))(atof(nth 2 list-point))))
    (cond
      ((and (=(atof(nth 1 list-point))0)(=(atof(nth 2 list-point))0))
       (setq corrent-list-item list-item-no-xy)
       )
      ((setq corrent-list-item (XML-Get-Child-ByAttribute o-doc nil "name" (analisis-point-square point)))
       )
      (T
       (setq corrent-list-item (XML-Add-Child o-doc "item"))
       (XML-Add-Attribute corrent-list-item "name" (analisis-point-square point))
       corrent-list-item)
      )
    (setq point-item (XML-Add-Child corrent-list-item "item"))
    (if (/= (nth 0 list-point) "")(XML-Add-Attribute point-item "name" (nth 0 list-point)))
    (if (/=(atof(nth 1 list-point))0)(XML-Add-Attribute point-item "N" (rtos(atof(nth 1 list-point))2 3)))
    (if (/=(atof(nth 2 list-point))0)(XML-Add-Attribute point-item "E" (rtos(atof(nth 2 list-point))2 3)))
    (if (/=(atof(nth 3 list-point))0)(XML-Add-Attribute point-item "Z" (rtos(atof(nth 3 list-point))2 3)))
    (if (/= (nth 4 list-point) "")(XML-Add-Attribute point-item "type" (nth 4 list-point)))
    (if (/= (nth 5 list-point) "")(XML-Add-Attribute point-item "address" (nth 5 list-point)))
    (if (/= (nth 6 list-point) "")(XML-Add-Attribute point-item "year" (nth 6 list-point)))
    (if (/= (nth 7 list-point) "")(XML-Add-Attribute point-item "comment" (nth 7 list-point)))
    (vlax-release-object point-item)
    )
  (XML-Save o-doc)
  (vlax-release-object list-item-no-xy)
  (vlax-release-object o-doc)
  (princ)
  )

(defun XML-create-blank-file (patch / )
  ; создание пустого xml файла
  (file_write_list_to_file patch(list "<?xml version=\"1.0\" encoding=\"windows-1251\"?><root/>"))
  (XML-Get-XMLObject patch)
  )


(defun C:save-true ( / list-all-coor list-item-all-coor n o-doc punkt result)
  (if(setq o-doc(XML-Get-XMLObject (strcat(getvar "DWGPREFIX")"каталог координат.xml")))
    (progn
      (setq list-item-all-coor(XML-Get-Child-ByAttribute o-doc nil "name" "all"))
      (setq list-all-coor (convert-child-to-ass-list list-item-all-coor))
      (setq n 0)
      (while (setq punkt (nth n list-all-coor))
	(if (=(rem n 100)0)(grtext -1 (strcat"Обрабатываю "(itoa n)"/"(itoa(length list-all-coor)))))
	(if(assoc"year"punkt)
	  (setq result(append result(list (convert-ass-list-to-string punkt))))
	  )
	(setq n (1+ n))
	)
      (file_write_list_to_file (strcat(getvar "DWGPREFIX")"result-true.txt") result)
      (vlax-release-object list-item-all-coor)
      )
    (alert "Ошибка! Исходный файл координат не обнаружен!")
    )
  (vlax-release-object o-doc)
  (grtext -1 "")
  (princ)
  )

(defun txt-to-txt (file  / INPUT-FILE N RAW-LIST-POINT RESULT STRING pos)
  ;конвертация файла исходных координат(имя|X|Y|Z|D) в geo_tools формат пунктов (имя|X|Y|Z|тип центра|местоположение,адрес|год получения|примечание)
  (setq input-file (strcat(getvar "DWGPREFIX")file))
  (if (file-p input-file)
    (progn
      (setq n 0)
      (setq input-file(open input-file "r"))
      (while (setq string (read-line input-file))
	(princ (strcat"\r "(itoa n)))
	(setq raw-list-point (convert-string-to-list-be-delimiter (vl-string-left-trim "|" string) "|"))
	
	(setq result
	       (append result
		       (list (convert-list-to-string-be-delimiter
			       (list (if(setq pos(vl-string-search "-" (vl-string-trim " "(nth 0 raw-list-point))))
				       (substr (vl-string-trim " "(nth 0 raw-list-point)) 1 pos)
				       (vl-string-trim " "(nth 0 raw-list-point))
				       );имя
				     (vl-string-trim " "(nth 1 raw-list-point));X
				     (vl-string-trim " "(nth 2 raw-list-point));Y
				     (vl-string-trim " "(nth 3 raw-list-point));Z
				     "";тип центра; ст.репер гор.марка комп.знак ПЗ
				     (vl-string-trim " "(nth 4 raw-list-point));местоположение,адрес
				     "";год получения
				     "";примечание
				     )"|"))))
	(setq n (1+ n))
	)
      (close input-file)
      (file_write_list_to_file (strcat(getvar "DWGPREFIX")"_"file) result)
      )
    (alert "Ошибка! Исходный файл координат не обнаружен!")
    )
  (princ)
  )

(defun txt-1 (file  / INPUT-FILE N RAW-LIST-POINT RESULT STRING pos)
  ;конвертация файла исходных координат(имя|X|Y|Z|D) в geo_tools формат пунктов (имя|X|Y|Z|тип центра|местоположение,адрес|год получения|примечание)
  (setq input-file (strcat(getvar "DWGPREFIX")file))
  (if (file-p input-file)
    (progn
      (setq n 0)
      (setq input-file(open input-file "r"))
      (while (setq string (read-line input-file))
	(princ (strcat"\r "(itoa n)))
	(setq raw-list-point (convert-string-to-list-be-delimiter (vl-string-left-trim "|" string) "|"))
	(setq type-punct
	       (cond
		 ((or(vl-string-search "СИГНАЛ"(nth 5 raw-list-point))(vl-string-search "сигнал"(nth 5 raw-list-point)))
		  "сигнал"
		  )
		 ((or(vl-string-search "НАДСТРОЙКА"(nth 5 raw-list-point))(vl-string-search "надстройка"(nth 5 raw-list-point)))
		  "надстройка"
		  )
		 ((or(vl-string-search "ПИРАМИДА"(nth 5 raw-list-point))(vl-string-search "пирамида"(nth 5 raw-list-point)))
		  "пирамида"
		  )
		 ((or(vl-string-search "КЕРН"(nth 5 raw-list-point))(vl-string-search "Керн"(nth 5 raw-list-point))(vl-string-search "керн"(nth 5 raw-list-point)))
		  "керн"
		  )
		 ((vl-string-search "ПЗ"(nth 5 raw-list-point))
		  "ПЗ"
		  )
		 ((or(vl-string-search "ШПИЛЬ"(nth 5 raw-list-point))(vl-string-search "шпиль"(nth 5 raw-list-point)))
		  "местн.предмет"
		  )
		 
		 ((or(vl-string-search "МАРКА"(nth 5 raw-list-point))(vl-string-search "марка"(nth 5 raw-list-point)))
		  "марка"
		  )
		 ((or(vl-string-search "РЕЛЬС"(nth 5 raw-list-point))(vl-string-search "рельс"(nth 5 raw-list-point)))
		  "рельс"
		  )
		 ((or(vl-string-search "ДЮБЕЛЬ"(nth 5 raw-list-point))(vl-string-search "дюбель"(nth 5 raw-list-point)))
		  "дюбель"
		  )
		 ((or(vl-string-search "ГВОЗДЬ"(nth 5 raw-list-point))(vl-string-search "гвоздь"(nth 5 raw-list-point)))
		  "гвоздь"
		  )
		 (t (nth 4 raw-list-point))
		 ))
	(setq raw-list-point
	       (list
		 (nth 0 raw-list-point)
		 (nth 1 raw-list-point)
		 (nth 2 raw-list-point)
		 (nth 3 raw-list-point)
		 type-punct
		 (nth 5 raw-list-point)
		 (nth 6 raw-list-point)
		 (nth 7 raw-list-point)))
	(setq result(append result(list(convert-list-to-string-be-delimiter raw-list-point"|"))))
	(setq n (1+ n))
	)
      (close input-file)
      (file_write_list_to_file (strcat(getvar "DWGPREFIX")"_"file) result)
      )
    (alert "Ошибка! Исходный файл координат не обнаружен!")
    )
  (princ)
  )

(defun txt-2 (file  / INPUT-FILE N RAW-LIST-POINT RESULT1 RESULT2 STRING)
  ;отделение без NE в отдельный файл
  (setq input-file (strcat(getvar "DWGPREFIX")file))
  (if (file-p input-file)
    (progn
      (setq n 1)
      (setq input-file(open input-file "r"))
      (while (setq string (read-line input-file))
	(if (=(rem n 100)0)(grtext -1 (strcat"Читаю "(itoa n))))
	(setq raw-list-point (convert-string-to-list-be-delimiter (vl-string-left-trim "|" string) "|"))
	(if(=(nth 1 raw-list-point)"")
	  (setq result1(append result1(list(convert-list-to-string-be-delimiter raw-list-point"|"))))
	  (setq result2(append result2(list(convert-list-to-string-be-delimiter raw-list-point"|"))))
	  )
	(setq n (1+ n))
	)
      (close input-file)
      (file_write_list_to_file (strcat(getvar "DWGPREFIX")"_"file) result2)
      (file_write_list_to_file (strcat(getvar "DWGPREFIX")"_no_z_"file) result1)
      )
    (alert "Ошибка! Исходный файл координат не обнаружен!")
    )
  (princ)
  )



(defun test-fun-eq (punkt-1 punkt-2 flag / n1 n2 e1 e2)
  (setq n1 (atof(cadr(assoc"N"punkt-1))))
  (setq e1 (atof(cadr(assoc"E"punkt-1))))
  (setq n2 (atof(cadr(assoc"N"punkt-2))))
  (setq e2 (atof(cadr(assoc"E"punkt-2))))
  (if
    flag
    (if (and(equal n1 n2 1.5)(equal e1 e2 1.5))
      T
      nil)
    (if (and(= n1 n2)(= e1 e2))
      T
      nil)
    )
  )


(defun C:txt-4 (  / ADD-COMMENT COMMENT-OLD FILE-1 FILE-2 LIST-NEW-COOR LIST-OLD-COOR N NAME-NEW NAME-OLD NN PUNKT-NEW PUNKT-OLD RESULT X-NEW X-OLD Y-NEW Y-OLD YEAR-OLD)
    ;сравнивает два раздела файла с пунктами, второй без отметок и адресов... перед сравнением переводит в асоциированный список
  (setq file-1 "vis.txt")
  (setq file-2 "dop.vis.txt")
  (if (and(file-p (strcat (strcat(getvar "DWGPREFIX"))file-1))(file-p (strcat (strcat(getvar "DWGPREFIX"))file-2)))
    (progn
      (setq list-old-coor(convert-text-file-to-ass-list (strcat (strcat(getvar "DWGPREFIX"))file-1)))
      (setq list-new-coor(convert-text-file-to-ass-list (strcat (strcat(getvar "DWGPREFIX"))file-2)))
      (setq n 0)
      (while (setq punkt-new (nth n list-new-coor))
	(grtext -1 (strcat"Обрабатываю "(itoa n)"/"(itoa(length list-new-coor))))
	(setq nn 0)
	(setq name-new (cadr(assoc"name"punkt-new)))
	(setq x-new (cadr(assoc"N"punkt-new)))
	(setq y-new (cadr(assoc"E"punkt-new)))
	(while (setq punkt-old (nth nn list-old-coor))
	  (setq name-old nil
		x-old nil
		y-old nil
		year-old nil
		comment-old nil)
	  (setq name-old (cadr(assoc"name"punkt-old)))
	  (setq x-old (cadr(assoc"N"punkt-old)))
	  (setq y-old (cadr(assoc"E"punkt-old)))
	  (if(cadr(assoc"year"punkt-old))(setq year-old (cadr(assoc"year"punkt-old))))
	  (if(cadr(assoc"comment"punkt-old))(setq comment-old (cadr(assoc"comment"punkt-old))))
	  (cond
	    ((and(= x-new x-old)
		 (= y-new y-old)
		 )
	     (setq result(append result(list (strcat "Удален|"(convert-ass-list-to-string punkt-new)))))
	     (setq list-new-coor(vl-remove punkt-new list-new-coor))
	     (if (/= name-new name-old)
	       (progn
		 (if comment-old
		   (setq list-old-coor(subst(subst (list "comment" (strcat comment-old"; Альт.: name="name-new))(assoc "comment" punkt-old) punkt-old)punkt-old list-old-coor))
		   (setq list-old-coor(subst(append punkt-old(list(list "comment" (strcat "Альт.: name="name-new))))punkt-old list-old-coor))
		   )
		 (setq result(append result(list (strcat "Изменен|"(convert-ass-list-to-string (nth nn list-old-coor))))))
		 )
	       )
	     (setq n (1- n))
	     (setq nn (+ nn 1000000))
	     )
	    ((equal (list (atof y-new)(atof x-new))
		    (list (atof y-old)(atof x-old))1.5)
	     (if (not year-old)
	       (progn
		 (setq result(append result(list (strcat "Удален|"(convert-ass-list-to-string punkt-new)))))
		 (setq list-new-coor(vl-remove punkt-new list-new-coor))
		 
		 (setq add-comment
			(if (= name-new name-old)
			  (strcat "Альт.: N="x-new " E="y-new)
			  (strcat "Альт.: name="name-new" N="x-new" E="y-new)
			  ))
		 (terpri)
		 (princ add-comment)
		 (if comment-old
		   (setq list-old-coor(subst(subst (list "comment" (strcat comment-old"; "add-comment))(assoc "comment" punkt-old) punkt-old)punkt-old list-old-coor))
		   (setq list-old-coor(subst(append punkt-old(list(list "comment" add-comment)))punkt-old list-old-coor))
		   )
		 (setq result(append result(list (strcat "Изменен|"(convert-ass-list-to-string (nth nn list-old-coor))))))
		 (setq n (1- n))
		 (setq nn (+ nn 1000000))
		 )))
	    );cond
	  (setq nn (1+ nn))
	  )
	(setq n (1+ n))
	)
      (file_write_list_to_file (strcat(getvar "DWGPREFIX")"_"file-1) (mapcar (function (lambda (x) (convert-ass-list-to-string x))) list-old-coor))
      (file_write_list_to_file (strcat(getvar "DWGPREFIX")"_"file-2) (mapcar (function (lambda (x) (convert-ass-list-to-string x))) list-new-coor))
      )
    (alert "Ошибка! Исходный файл координат не обнаружен!")
    )
  (princ)
  )

(defun C:txt-5 (  / ADD-COMMENT ADDRESS-1 ADDRESS-2 COMMENT-1 COMMENT-2 E-1 E-2 FILE LIST-ALL-COOR N N-1 N-2 NAME-1 NAME-2 NN PUNKT-1 PUNKT-2 RESULT Z-1 Z-2)
    ;ищет дубликаты среди пунктов каталога
  (setq file "all.coor.txt")
  (if (file-p (strcat (strcat(getvar "DWGPREFIX"))file))
    (progn
      (setq list-all-coor(convert-text-file-to-ass-list (strcat (strcat(getvar "DWGPREFIX"))file)))
      (setq n 0)
      (while (setq punkt-1 (nth n list-all-coor))
	(grtext -1 (strcat"Обрабатываю "(itoa n)"/"(itoa(length list-all-coor))))
	(setq name-1 nil
	      N-1 nil
	      E-1 nil
	      Z-1 nil
	      address-1 nil
	      comment-1 nil)
	(setq name-1 (cadr(assoc"name"punkt-1)))
	(setq N-1 (cadr(assoc"N"punkt-1)))
	(setq E-1 (cadr(assoc"E"punkt-1)))
	(if(cadr(assoc"Z"punkt-1))(setq Z-1 (cadr(assoc"Z"punkt-1))))
	(if(cadr(assoc"address"punkt-1))(setq address-1 (cadr(assoc"address"punkt-1))))
	(if(cadr(assoc"comment"punkt-1))(setq comment-1 (cadr(assoc"comment"punkt-1))))
	(setq nn (1+ n))
	(while (setq punkt-2 (nth nn list-all-coor))
	  (setq name-2 nil
		N-2 nil
		E-2 nil
		Z-2 nil
		address-2 nil
		comment-2 nil
		add-comment nil)
	  (setq name-2 (cadr(assoc"name"punkt-2)))
	  (setq N-2 (cadr(assoc"N"punkt-2)))
	  (setq E-2 (cadr(assoc"E"punkt-2)))
	  (if(cadr(assoc"Z"punkt-2))(setq Z-2 (cadr(assoc"Z"punkt-2))))
	  (if(cadr(assoc"address"punkt-2))(setq address-2 (cadr(assoc"address"punkt-2))))
	  (if(cadr(assoc"comment"punkt-2))(setq comment-2 (cadr(assoc"comment"punkt-2))))
	  (cond
	    ((and(= N-1 N-2)(= E-1 E-2))
	     (setq result(append result(list (strcat "Удален|"(convert-ass-list-to-string punkt-2)))))
	     (setq list-all-coor(vl-remove punkt-2 list-all-coor))
	     (if (or(/= name-1 name-2)(/= Z-1 Z-2)(/= address-1 address-2))
	       (progn
		 (if comment-1
		   (setq add-comment (strcat comment-1"; Альт.:"))
		   (setq add-comment "Альт.:"))
		 (if(/= name-1 name-2)(setq add-comment(strcat add-comment(strcat" name="name-2))))
		 (if(and Z-2(/= Z-1 Z-2))(setq add-comment(strcat add-comment(strcat" Z="Z-2))))
		 (if(and address-2(/= address-1 address-2))(setq add-comment(strcat add-comment(strcat" address="address-2))))
		 (if(and comment-2(/= comment-1 comment-2))(setq add-comment(strcat add-comment(strcat" comment="comment-2))))
		 (if comment-1
		   (setq list-all-coor(subst(subst (list "comment" add-comment)(assoc "comment" punkt-1) punkt-1)punkt-1 list-all-coor))
		   (setq list-all-coor(subst(append punkt-1(list(list "comment" add-comment)))punkt-1 list-all-coor)))
		 (setq result(append result(list (strcat "Изменен|"(convert-ass-list-to-string (nth n list-all-coor))))))
		 )
	       )
	     (setq n (1- n))
	     (setq nn (+ nn 1000000))
	     )
	    ((equal (list (atof N-1)(atof E-1))
		    (list (atof N-2)(atof E-2))1.5)
	     (setq result(append result(list (strcat "Удален|"(convert-ass-list-to-string punkt-2)))))
	     (setq list-all-coor(vl-remove punkt-2 list-all-coor))
	     (if (or(/= name-1 name-2)(/= Z-1 Z-2)(/= address-1 address-2))
	       (progn
		 (if comment-1
		   (setq add-comment (strcat comment-1"; Альт.:"))
		   (setq add-comment "Альт.:"))
		 (if(/= name-1 name-2)(setq add-comment(strcat add-comment(strcat" name="name-2))))
		 (setq add-comment(strcat add-comment(strcat" N="N-2)))
		 (setq add-comment(strcat add-comment(strcat" E="E-2)))
		 (if(and Z-2(/= Z-1 Z-2))(setq add-comment(strcat add-comment(strcat" Z="Z-2))))
		 (if(and address-2(/= address-1 address-2))(setq add-comment(strcat add-comment(strcat" address="address-2))))
		 (if(and comment-2(/= comment-1 comment-2))(setq add-comment(strcat add-comment(strcat" comment="comment-2))))
		 (if comment-1
		   (setq list-all-coor(subst(subst (list "comment" add-comment)(assoc "comment" punkt-1) punkt-1)punkt-1 list-all-coor))
		   (setq list-all-coor(subst(append punkt-1(list(list "comment" add-comment)))punkt-1 list-all-coor)))
		 (setq result(append result(list (strcat "Изменен|"(convert-ass-list-to-string (nth n list-all-coor))))))
		 )
	       )
	     (setq n (1- n))
	     (setq nn (+ nn 1000000))
	     )
	    )
	  (setq nn (1+ nn))
	  )
	(setq n (1+ n))
	)
      (if result(file_write_list_to_file (strcat(getvar "DWGPREFIX")"raport.txt") result))
      (file_write_list_to_file (strcat(getvar "DWGPREFIX")"_"file) (mapcar (function (lambda (x) (convert-ass-list-to-string x))) list-all-coor))
      )
    (alert "Ошибка! Исходный файл координат не обнаружен!")
    )
  (princ)
  )