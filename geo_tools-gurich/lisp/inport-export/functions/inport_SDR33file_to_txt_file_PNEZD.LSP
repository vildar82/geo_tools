(defun C:inport_SDR33file_to_txt_file_PNEZD ( / filename file_points_list osn putSDR33file putTXTfile SDR33file str TXTfile)
  (geo_tools-help "inport-SDR33file-to-txt-file-PNEZD")
  (setq osn (getvar "OSMODE"))
  (setq putSDR33file (getfiled "Выбор файла SDR33" (getvar "DWGPREFIX") "txt;sdr" 0))
  (setq FileName(substr putSDR33file (+(vl-string-position 92 putSDR33file 0 T)2) (-(strlen putSDR33file)(+(vl-string-position 92 putSDR33file 0 T)2)3)))
  (setq putTXTfile (strcat (substr putSDR33file 1 (-(strlen putSDR33file)4))"_" (substr putSDR33file (-(strlen putSDR33file)3) 4)))
  (setq SDR33file (open putSDR33file"r"))
  (setq str (read-line SDR33file))
  (while (not(or(eq str nil)(eq (substr str 1 4)"000")))
    (if (or(eq (substr str 1 4) "08TP") (eq (substr str 1 4) "08KI") (eq (substr str 1 4) "02TP"))
      (progn
	(setq file_points_list
	       (append file_points_list
		       (list (list
			       (vl-string-trim" "(substr str 5 16)); номер
			       (atof(vl-string-trim" "(substr str 21 16))); X
			       (atof(vl-string-trim" "(substr str 37 16))); Y
			       (atof(vl-string-trim" "(substr str 53 16))); H
			       (vl-string-trim" "(substr str 69 16)); описание
			       ))))))
    (setq str (read-line SDR33file))
    )
  (close SDR33file)
  
  (if file_points_list
    (progn
      (setq TXTfile (open putTXTfile "w"))
      (foreach x file_points_list
      (write-line (strcat
		      (nth 0 x)" "; номер
		      (rtos(nth 1 x)2 3)" "; X
		      (rtos(nth 2 x)2 3)" "; Y
		      (rtos(nth 3 x)2 3)" "; H
		      (nth 4 x))  ; описание
	  TXTfile)
	)
      (close TXTfile)
      )
    (alert "В выбраном файле координат не обнаружено!")
    )
  (princ)
  )