(defun load_mlinestyle (flnm stname reload / *error* lst_member answer fl fl2 ckl strmus strmus2 lst_style)
  ;flnm - имя файла стиля мультилинии
  ;stname - имя стиля мультилинии
  ;reload - если не nil, то перезагрузка стиля
  (defun *error* (msg)
    (vl-catch-all-apply 'close (list fl2))
    (princ (strcat "\nОшибка в файле стиля мультилинии " flnm))
    );defun
  (if (or (not (setq lst_member (member (cons 3 stname) (setq answer (dictsearch (namedobjdict) "ACAD_MLINESTYLE")))))
	  reload
	  );or
    (if (and (setq fl (findfile flnm))
	     (setq fl2 (open fl "r"))
	     );and
      (progn
	(read-line fl2)
	(setq ckl t)
	(while (and ckl
		    (setq strmus (read-line fl2))
		    (setq strmus2 (read-line fl2))
		    );and
	  (if (and (= (vl-string-trim " " strmus) "2")
		   (= (vl-string-trim " " strmus2) stname)
		   );and
	    (setq ckl nil)
	    );if
	  );while
	(while (and (setq strmus (read-line fl2))
		    (setq strmus2 (read-line fl2))
		    (/= (setq strmus (vl-string-trim " " strmus)) "0")
		    );and
	  (setq lst_style (cons
			    (cond
			      ((or (= strmus "3") (= strmus "6"))
			       (cons (atoi strmus) (vl-string-trim " " strmus2))
			       )
			      ((or (= strmus "51") (= strmus "52"))
			       (cons (atoi strmus) (* (atof (vl-string-trim " " strmus2)) (/ pi 180.0)))
			       )
			      (t
			       (read (strcat "(" strmus " . " (vl-string-trim " " strmus2) ")"))
			       )
			      );cond
			    lst_style))
	  );while
	(close fl2)
	(if lst_style
	  (progn
	    (setq lst_style (append
			      (list
				'(0 . "MLINESTYLE")
				(cons 330 (cdr (assoc -1 answer)))
				'(100 . "AcDbMlineStyle")
				(cons 2 stname))
			      (reverse lst_style)))
	    (if lst_member
	      (entmod (subst (cons 350 (entmakex lst_style)) (cadr lst_member) answer))
	      (entmod (append answer (list (cons 3 stname) (cons 350 (entmakex lst_style)))))
	      );if
	    );progn
	  );if
	);progn
      );if
    answer
    );if
  )
