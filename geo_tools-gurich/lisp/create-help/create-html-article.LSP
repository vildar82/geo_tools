(defun create-html-article (put-txt-file ohtput-put deep flag-cut-name / list-html-file list-txt-file str-deep x)
  (setq list-txt-file (file_read_to_list put-txt-file))
  (setq str-deep "")
  (if (> deep 0)
    (repeat deep
      (setq str-deep (strcat str-deep "../"))
      )
    )
  (setq list-html-file
	 (append
	   (list
	     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
	     "<html>"
	     "<head>"
	     "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1251\">"
	     "<title>"
	     (car list-txt-file)
	     "</title>"
	     (strcat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" str-deep "style.css\">")
	     "</head>"
	     "<body>"
	     )
	   (mapcar '(lambda (x) (edit-string-replase-str ".html\"" ".xxx\"" x)) (cdr list-txt-file))
	   (list
	     "</body>"
	     "</html>"
	     )
	   )
	)
  (file_write_list_to_file(strcat ohtput-put "\\" (vl-filename-base put-txt-file) ".html")list-html-file)
  
  (if flag-cut-name
    (progn
      (setq put-txt-file (strcat (vl-filename-directory put-txt-file)"\\"(substr(vl-filename-base put-txt-file)4) (vl-filename-extension put-txt-file)))
      )
    )
  (copy-imege-file-by-fun put-txt-file ohtput-put)
  
  )
										  
