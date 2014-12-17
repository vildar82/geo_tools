(defun file-xml-to-list (patch /  result)
  (if (file-p patch)
    (file-xml-to-list-read (XML-Get-XMLObject patch))
    (progn
      (alert (strcat"Ошибка! Файл \"" patch "\" не обнаружен!"))
      nil
      )
    )
  )

(defun file-xml-to-list-read (o-doc / list-child  result)
  (if(or(= (vlax-get-property o-doc "nodeName") "list")
	(= (vlax-get-property o-doc "nodeName") "root"))
    (setq list-Child(XML-Get-ChildList o-doc)))
  (while list-Child
    (if (= (vlax-get-property (car list-Child) "nodeName") "list")
      (progn
	(setq result (append result (list(file-xml-to-list-read (car list-Child)))))
	(setq list-Child(cdr list-Child))
	)
      (progn
	(setq result (append result (list(convert-record-to-variable (car list-Child)))))
	(setq list-Child(cdr list-Child))
	)
      )
    )
  result
  )