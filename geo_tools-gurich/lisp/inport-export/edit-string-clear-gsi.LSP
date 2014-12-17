(defun edit-string-clear-gsi (str / point list-str string)
  (setq list-str(convert-string-to-list-be-delimiter str " "))
  (foreach string list-str
    (if (/= string "")
      (cond
	((vl-string-search "+" string)
	 (setq point
		(append point
			(list
			  (cons
			    (atoi(substr string 1 (vl-string-search "+" string)))
			    (vl-string-left-trim "0"(substr string(+(vl-string-search "+" string)2)))
			    ))))
	 )
	((vl-string-search "-" string)
	 (setq point
		(append point
			(list
			  (cons
			    (atoi(substr string 1 (vl-string-search "-" string)))
			    (strcat "-"(vl-string-left-trim "0"(substr string(+(vl-string-search "-" string)2))))
			    ))))
	 )
	)
      
      )
    )
  (setq point(list
	      (cdr(nth 0 point)); номер
	      (cdr(assoc 82 point)); X
	      (cdr(assoc 81 point)); Y
	      (cdr(assoc 83 point)); Z
	      (cdr(assoc 71 point)); "code"
	      ))
  (if (not(or(eq(nth 0 point)nil)(eq(nth 1 point)nil)(eq(nth 2 point)nil)(eq(nth 3 point)nil)))
    (list
      (list "name"(nth 0 point))
      (list "N"(/(atof(nth 1 point))1000.0))
      (list "E"(/(atof(nth 2 point))1000.0))
      (list "Z"(/(atof(nth 3 point))1000.0))
      (list "code"(nth 4 point))
      )
    nil
    )
  )