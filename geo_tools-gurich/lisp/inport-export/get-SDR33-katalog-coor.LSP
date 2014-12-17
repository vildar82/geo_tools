(defun get-SDR33-katalog-coor (putSDR33file / file_points_list sdr33file str)
  (setq SDR33file (open putSDR33file"r"))
  (setq str (read-line SDR33file))
  (while (not(or(eq str nil)(eq (substr str 1 4)"000")))
    (if (eq (substr str 1 4) "03NM")(setq h-target (atof(vl-string-trim" "(substr str 5)))))
    (if (or(eq (substr str 1 4) "08TP") (eq (substr str 1 4) "08KI") (eq (substr str 1 4) "02TP"))
      (progn
	(if (and (eq (substr str 1 4) "08TP")h-target)
	  (setq file_points_list
		 (append file_points_list
			 (list (list
				 (list "name"(vl-string-trim" "(substr str 5 16)))
				 (list "N"(atof(vl-string-trim" "(substr str 21 16)))); X
				 (list "E"(atof(vl-string-trim" "(substr str 37 16)))); Y
				 (list "Z"(atof(vl-string-trim" "(substr str 53 16)))); H
				 (list "code"(vl-string-trim" "(substr str 69 16))); описание
				 (list "h-target" h-target)
				 ))))
	  (setq file_points_list
		 (append file_points_list
			 (list (list
				 (list "name"(vl-string-trim" "(substr str 5 16)))
				 (list "N"(atof(vl-string-trim" "(substr str 21 16)))); X
				 (list "E"(atof(vl-string-trim" "(substr str 37 16)))); Y
				 (list "Z"(atof(vl-string-trim" "(substr str 53 16)))); H
				 (list "code"(vl-string-trim" "(substr str 69 16))); описание
				 ))))
	  )
	))
    (setq str (read-line SDR33file))
    )
  (close SDR33file)
  file_points_list
  )