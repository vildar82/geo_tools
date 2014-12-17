(defun get-geo_tools_options-list(/ result-list)
  (foreach x (list (list "server-check-update-flag" "1")
		   (list "server-auto-update-flag" "0")
		   (list "geo_tools-server-patch" "\\\\w001\\GEO\\!Документы!\\geo_tools"))
    (setq result-list (append result-list (list(list(car x)(file-dat-xml-read (car x) (cadr x))))))
    )
  result-list
  )