(defun list-to-file-xml (patch list-input /  o-doc )
  ;запись списка в виде xml файла
  (if (file-p patch)
    (vl-file-delete patch)
    )
  (setq o-doc(XML-create-blank-file patch))
  (list-to-file-xml-write o-doc list-input)
  (XML-Save o-doc)
  (vlax-release-object o-doc)
  (princ)
  )

(defun list-to-file-xml-write (o-doc list-input / list-record)
  (while list-input
    (if (=(type (car list-input)) 'list)
      (progn
	(setq list-record (XML-Add-Child o-doc "list"))
	(list-to-file-xml-write list-record (car list-input))
	(setq list-input(cdr list-input))
	)
      (progn
	(convert-item-to-record o-doc (car list-input))
	(setq list-input(cdr list-input))
	)
      )
    )
  )

(defun convert-item-to-record (parent value / record str-item type-item)
  ;конвертирует переменную в запись в файлке данных
  (setq str-item (cond
		    ((= (type value)'INT)
		     (setq type-item "INT")
		     (itoa value)
		     )
		    ((= (type value)'REAL)
		     (setq type-item "REAL")
		     (rtos value 2 12)
		     )
		    ((= (type value)'STR)
		     (setq type-item "STR")
		     value
		     )
		    (T
		     nil)
		    )
	)
  (if str-item
    (progn
      (setq record (XML-Add-Child parent "record"))
      (XML-Add-Attribute record "value" str-item)
      (XML-Add-Attribute record "type" type-item)
      record
      )
    nil
    )
  )