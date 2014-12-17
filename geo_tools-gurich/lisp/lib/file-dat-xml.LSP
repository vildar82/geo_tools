(defun file-dat-xml-read (variable defaut / )
  ;чтение значения переменной из xml файла данных
  (file-dat-xml nil variable nil defaut)
  )

(defun file-dat-xml-write (variable value / )
  ;запись значения переменной в xml файла данных
  (file-dat-xml T variable value nil)
  )

(defun file-dat-xml (flag-write variable value defaut / item-record o-doc result)
  ; низкая функция для чтения/записи переменной в файле данных
  (setq o-doc (get-xml-doc-file-dat))
  (if flag-write
    (if(setq item-record(XML-Get-Child-ByAttribute o-doc nil "variable" variable)); запись
      (progn
	(XML-Remove-Child item-record)
	(convert-variable-to-record o-doc variable value)
	)
      )
    (if(setq item-record(XML-Get-Child-ByAttribute o-doc nil "variable" variable));чтение
      (setq result(convert-record-to-variable item-record))
      (progn
	(convert-variable-to-record o-doc variable defaut)
	(setq result defaut)
	)
      )
    )
  (XML-Save o-doc)
  (if o-doc(vlax-release-object o-doc))
  (if item-record(vlax-release-object item-record))
  result
  )

(defun convert-variable-to-record (parent variable value / record str-item type-item)
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
      (XML-Add-Attribute record "variable" variable)
      (XML-Add-Attribute record "type" type-item)
      (XML-Add-Attribute record "value" str-item)
      record
      )
    nil
    )
  )

(defun convert-record-to-variable (record / )
  ;конвертирует запись в файлке данных в переменную
  (convert-str-be-type (XML-Get-Attribute record "value" "")(XML-Get-Attribute record "type" ""))
  )

(defun file-dat-xml-read (variable defaut / )
  ;чтение значения переменной из xml файла данных
  (file-dat-xml nil variable nil defaut)
  )

(defun file-dat-xml-write (variable value / )
  ;запись значения переменной в xml файла данных
  (file-dat-xml T variable value nil)
  )

(defun get-xml-doc-file-dat (/ o-doc)
  (if (setq o-doc(XML-Get-XMLObject (get-patch-to-file-dat-xml)))
    o-doc
    (XML-create-blank-file (get-patch-to-file-dat-xml))
    )
  )

(defun get-patch-to-file-dat-xml ()
  (strcat *geo_tools_user_dat_dir* "\\data\\file-dat.xml")
  )

(defun XML-create-blank-file (patch / )
  ; создание пустого xml файла
  (file_write_list_to_file patch(list "<?xml version=\"1.0\" encoding=\"windows-1251\"?><root/>"))
  (XML-Get-XMLObject patch)
  )