(defun convert-profile-standart-str-dat-to-xml (/ list-dat n result-list-str result-list-string-in-basement templete-profile-string)
  ; конвертирование файла стандартных строк подвала профиля в формат xml
  (setq list-dat(profile-get-standart-list-str-dat))
  (setq templete-profile-string
	 (list
	   (list "name-str" "STR")
	   (list "code-str" "STR")
	   (list "h-str" "REAL")
	   (list "sel-units-downgrade" "INT")
	   (list "accuracy-text" "INT")
	   (list "accuracy-downgrade" "INT")
	   (list "h-text" "REAL")
	   (list "draw-circle" "INT")
	   (list "min-angle" "REAL")
	   )
	)
  (foreach y list-dat
    (setq n 0)
    (setq result-list-str nil)
    (repeat (length y)
      (if (not (=(nth n y)"-"))
	(setq result-list-str
	       (append result-list-str
		       (list (list (car(nth n templete-profile-string))
				   (convert-str-be-type(nth n y)
				     (cadr(nth n templete-profile-string)))))))
	)
      (setq n (1+ n))
      )
    (setq result-list-string-in-basement(append result-list-string-in-basement (list result-list-str)))
    )
  (file_xml_write_be_list (strcat *geo_tools_user_dat_dir* "\\data\\profile-standart-str.xml") result-list-string-in-basement)
  )