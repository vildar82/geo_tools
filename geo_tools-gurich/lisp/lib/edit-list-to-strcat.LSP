(defun edit-list-to-strcat (com_list list_shel table_name /)
  (if (eq (nth 10 com_list) "0")
    ; NICON
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
      )
    ; SDR
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
    )
  )