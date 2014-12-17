(defun file-xml-read-be-list (patch / x)
  ; чтение файла формата xml и предоставление его в виде списка
  ; пример (file_xml_read_be_list "C:\\test.xml")
  (text-xml-to-textlist
    (substr
      (apply 'strcat(mapcar '(lambda (x) (vl-string-trim (vl-list->string '(32 9)) x))(file_read_to_list patch)))
      46)
    )
  )

(defun text-xml-to-textlist (_str  / )
  ; деление строки xml файла на список текстов
  (foreach x (list (list "\"" "|")
		   (list "<list>" "(")
		   (list "</list>" ")")
		   (list "<item>" "\"")
		   (list "</item>" "\"")
		   )
    (setq _str (edit-string-replase-str(cadr x)(car x)_str))
    )
  (edit-list-replase-str(read _str))
  )

(defun edit-string-replase-str (str-new str-old string /)
  ; редактирунт строку заменяя все встречающиеся строки str-old на str-new
  ;внимание встречающиеся в строках символы "#" "@" "." "*" "?" "~" "[" "]" "," "`"
  ;могут привести к бесконечному циклу
  
;;;  (foreach x(list "#" "@" "." "*" "?" "~" "[" "]" "," "`")
;;;    (while (wcmatch str-old (strcat "*`" x "*"))
;;;      (setq str-old (vl-string-subst (strcat "`" x) x str-old))
;;;      )
;;;    )
  (while (wcmatch string (strcat "*"str-old"*"))
      (setq string (vl-string-subst str-new str-old string))
      )
  string
  )

(defun edit-list-replase-str (input-list / item)
  (if input-list
    (cons
      (if (eq (type (setq item (car input-list))) 'LIST)
	(edit-list-replase-str item)
	(convert-str-to-value(edit-string-replase-str "\"" "|" item))
	)
      (edit-list-replase-str (cdr input-list))
      )
    )
  )

(defun convert-str-to-value (string / text-value type-item)
  (setq type-item (substr string (+(vl-string-search "type=" string) (strlen "type=") 1) (-(vl-string-search " " string)(strlen "type="))))
  (setq text-value(substr string (+(vl-string-search "value=" string) (strlen "value=") 1)))
  (cond
    ((= type-item "INT")
     (atoi text-value)
     )
    ((= type-item "REAL")
     (atof text-value)
     )
    ((= type-item "STR")
     text-value
     )
    )
  )