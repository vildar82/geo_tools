(defun file-write-coord-list (com_list spisok table_name / LIST-RESULT LIST_SHEL NUMBER-ITEM PUTSDR33FILE PUTTXTFILE STRING )
  (setq putTXTfile(strcat(getvar "DWGPREFIX")(substr (getvar "DWGNAME") 1 (-(strlen (getvar "DWGNAME"))4)) "_" table_name))
  (cond
    ((eq (nth 10 com_list) "0")
     (setq putSDR33file (getfiled "Имя сохраняемого файла..." putTXTfile "lst" 1))
     (foreach list_shel spisok
       (setq list-result
	      (append list-result
		      (list
			(strcat
			  (edit_str_trim_for_nunber_symbols
			    (vl-string-translate "," "."
			      (edit_translit_str
				(strcat
				  (if(eq(nth 0 com_list)"1")
				    (cond
				      ((eq(nth 7 com_list)"0")table_name)
				      ((eq(nth 7 com_list)"1")(nth 0 list_shel))
				      ((eq(nth 7 com_list)"2")(nth 1 list_shel)))
				    "")
				  (if(eq(nth 1 com_list)"1")
				    (nth 5 com_list)
				    "")
				  (if(eq(nth 2 com_list)"1")
				    (cond
				      ((eq(nth 8 com_list)"0")table_name)
				      ((eq(nth 8 com_list)"1")(nth 0 list_shel))
				      ((eq(nth 8 com_list)"2")(nth 1 list_shel)))
				    "")
				  (if(eq(nth 3 com_list)"1")
				    (nth 6 com_list)
				    "")
				  (if(eq(nth 4 com_list)"1")
				    (cond
				      ((eq(nth 9 com_list)"0")table_name)
				      ((eq(nth 9 com_list)"1")(nth 0 list_shel))
				      ((eq(nth 9 com_list)"2")(nth 1 list_shel)))
				    "")
				  )
				)
			      )
			    15
			    nil)
			  ","
			  (nth 2 list_shel)
			  ","
			  (nth 3 list_shel)
			  ","
			  "0.000"
			  ","
			  " "
			  ))
		      )))
     )
    ((eq (nth 10 com_list) "1")
     (setq putSDR33file (getfiled "Имя сохраняемого файла..." putTXTfile "sdr" 1))
     (foreach list_shel spisok
       (setq list-result
	      (append list-result
		      (list
			(strcat
			  "08TP"
			  (edit_str_adding_probel
			    (edit_str_trim_for_nunber_symbols
			      (edit_translit_str
				(strcat
				  (if(eq(nth 0 com_list)"1")
				    (cond
				      ((eq(nth 7 com_list)"0")table_name)
				      ((eq(nth 7 com_list)"1")(nth 0 list_shel))
				      ((eq(nth 7 com_list)"2")(nth 1 list_shel)))
				    "")
				  (if(eq(nth 1 com_list)"1")
				    (nth 5 com_list)
				    "")
				  (if(eq(nth 2 com_list)"1")
				    (cond
				      ((eq(nth 8 com_list)"0")table_name)
				      ((eq(nth 8 com_list)"1")(nth 0 list_shel))
				      ((eq(nth 8 com_list)"2")(nth 1 list_shel)))
				    "")
				  (if(eq(nth 3 com_list)"1")
				    (nth 6 com_list)
				    "")
				  (if(eq(nth 4 com_list)"1")
				    (cond
				      ((eq(nth 9 com_list)"0")table_name)
				      ((eq(nth 9 com_list)"1")(nth 0 list_shel))
				      ((eq(nth 9 com_list)"2")(nth 1 list_shel)))
				    "")
				  )
				)
			      15
			      nil)
			    16 T)
			  (edit_str_adding_probel (nth 2 list_shel) 16 nil)
			  (edit_str_adding_probel (nth 3 list_shel) 16 nil)
			  (edit_str_adding_probel "0.000" 16 nil)
			  (edit_str_adding_probel " " 16 nil)
			  )
			))))
     )
    ((eq (nth 10 com_list) "2")
     (setq putSDR33file (getfiled "Имя сохраняемого файла..." putTXTfile "gsi" 1))
     (setq number-item 1)
     (foreach point spisok
       (if (setq string (convert-point-to-gsi-string
			  (list
			    (cons "number-item" number-item)
			    (cons"number"
				 (edit_str_trim_for_nunber_symbols
				   (vl-string-translate "," "."
				     (edit_translit_str
				       (strcat
					 (if(eq(nth 0 com_list)"1")
					   (cond
					     ((eq(nth 7 com_list)"0")table_name)
					     ((eq(nth 7 com_list)"1")(nth 0 point))
					     ((eq(nth 7 com_list)"2")(nth 1 point)))
					   "")
					 (if(eq(nth 1 com_list)"1")
					   (nth 5 com_list)
					   "")
					 (if(eq(nth 2 com_list)"1")
					   (cond
					     ((eq(nth 8 com_list)"0")table_name)
					     ((eq(nth 8 com_list)"1")(nth 0 point))
					     ((eq(nth 8 com_list)"2")(nth 1 point)))
					   "")
					 (if(eq(nth 3 com_list)"1")
					   (nth 6 com_list)
					   "")
					 (if(eq(nth 4 com_list)"1")
					   (cond
					     ((eq(nth 9 com_list)"0")table_name)
					     ((eq(nth 9 com_list)"1")(nth 0 point))
					     ((eq(nth 9 com_list)"2")(nth 1 point)))
					   "")
					 )
				       )
				     )
				   15
				   nil))
			    (cons "N"(atof(nth 3 point)))
			    (cons "E"(atof(nth 2 point)))
			    (cons "Z" 0.0);z
			    (cons "description" ""))))
	 (setq list-result(append list-result(list string)))
	 )
       (setq number-item (1+ number-item))
       )
     )
    )
  (file_write_list_to_file putSDR33file list-result)
  )