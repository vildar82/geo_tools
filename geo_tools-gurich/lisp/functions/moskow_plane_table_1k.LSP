(defun C:moskow_plane_table_1k ( /
				CURENT-PL KWORD LIST_FILES PARCH-PL SELECT-POINT
				)
  (load_global_variable)
  (vla-StartUndoMark active_document)
  (if (member "планшеты" (vl-directory-files (getvar "DWGPREFIX") nil -1))
    (setq list_files (file_find_be_extension (strcat(getvar "DWGPREFIX")"планшеты") "dwg"))
    )
  (if list_files
    (progn
      (foreach x list_files
	(setq list_dwg(append list_dwg(list(cons (vl-filename-base x)x))))
	)
      (princ (strcat "\nДоступно "(rtos(length list_dwg)2 0)" планшетов."))
      )
    (princ "\nВНИМАНИЕ!!! Файлы планшетов не найдены!")
    )
  (initget"Схему Ввести Указать")(setq kword (getkword "\nЗадайте опцию [нарисовать Схему/Ввести наменклатуру/Указать планшет точкой]: "))
  (cond
    ((eq kword "Схему")
     )
    ((eq kword "Ввести")
     (while (not(eq curent-pl ""))
       (setq curent-pl(getstring "\nВведи наменклатуру планшета: "))
       (if (not(eq curent-pl ""))
	 (progn
	   (setq parch-pl (cdr(assoc curent-pl list_dwg)))
	   (if parch-pl
	     (command "_xref" "" parch-pl "0,0" "" "" "")
	     (alert (strcat "Планшет \"" curent-pl "\" среди файлов не найден."))
	     )
	   )
	 )
       )
     )
    ((eq kword "Указать")
     (while (not curent-pl)
       (draw_osnap_off)
       (setq select-point(getpoint"\nУкажи точку внутри планшета: "))
       (draw_osnap_on)
       (cond
	 ((and(not(minusp(nth 0 select-point)))(not(minusp(nth 1 select-point)))); 5
	  (setq curent-pl(strcat "5-"(rtos(1+(fix(/(abs (nth 1 select-point))1000)))2 0)"-"(rtos(1+(fix(/(abs (nth 0 select-point))1000)))2 0))))
	 ((and(not(minusp(nth 0 select-point)))(minusp(nth 1 select-point))); 6
	  (setq curent-pl(strcat "6-"(rtos(1+(fix(/(abs (nth 1 select-point))1000)))2 0)"-"(rtos(1+(fix(/(abs (nth 0 select-point))1000)))2 0))))
	 ((and(minusp(nth 0 select-point))(minusp(nth 1 select-point))); 7
	  (setq curent-pl(strcat "7-"(rtos(1+(fix(/(abs (nth 1 select-point))1000)))2 0)"-"(rtos(1+(fix(/(abs (nth 0 select-point))1000)))2 0))))
	 ((and(minusp(nth 0 select-point))(not(minusp(nth 1 select-point)))); 8
	  (setq curent-pl(strcat "8-"(rtos(1+(fix(/(abs (nth 1 select-point))1000)))2 0)"-"(rtos(1+(fix(/(abs (nth 0 select-point))1000)))2 0))))
	 (T
	  (alert "Так не бывает!!!"))
	 )
       (if curent-pl
	 (progn
	   (setq parch-pl (cdr(assoc curent-pl list_dwg)))
	   (if parch-pl
	     (command "_xref" "" parch-pl "0,0" "" "" "")
	     (alert (strcat "Планшет \"" curent-pl "\" среди файлов не найден."))
	     )
	   )
	 )
       )
     )
    )
  (vla-EndUndoMark active_document)
  (princ)
  )