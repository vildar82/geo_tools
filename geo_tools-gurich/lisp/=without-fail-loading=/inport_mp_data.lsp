(defun C:inport_mp_data ( / file patch poi-num polygon-num polyline-num text-line type-objects)
(setq patch (getfiled "Выбор файла mp" (getvar "DWGPREFIX") "mp" 0))
  (setq type-objects(get-type-for-gps-mp))
  (if (file-p patch)
    (progn
      (setq file(open patch "r"))
      (setq text-line (read-line file))
      (while text-line
	(bump)
	(if (or (= "[POI]" text-line) (= "[POLYLINE]" text-line) (= "[POLYGON]" text-line))
	  (progn
	    (setq list-elements (list(vl-string-trim "[]" text-line)))
	    (setq text-line (read-line file))
	    (while (not (= text-line "[END]"))
	      (if (wcmatch text-line "Data#=*")
		(setq list-elements(append list-elements (list (list "Level"(substr text-line 5(1+(-(vl-string-search "=" text-line)5))))
							       (list "Data"(substr text-line (+(vl-string-search "=" text-line)2)))
							       )))
		(setq list-elements(append list-elements (list(string-to-list text-line "="))))
		)
	      (setq text-line (read-line file))
	      )
	    (princ list-elements)
	    (draw-elements-be-mp-date list-elements type-objects)
	    )
	  (setq text-line (read-line file)))
	)
      (close file)
      )
    (alert(strcat"Файл "patch" не найден!"))
    )
  (princ)
  )

;;;(get-coordinates-by-textdat "Data0=(59.99908,0.00000),(59.99908,4.99878),(65.00061,4.99878),(65.00061,0.00000)")
;;;"Data0=(39.84466,-83.81745),(39.79248,-83.82569),(39.74304,-83.80921)"

(defun draw-elements-be-mp-date (input-list type-objects / level leyer-name type-element type-objects)
  (setq level 3); уровень детализации
  (setq type-element (car input-list))
  (setq leyer-name(cadr(assoc(cadr(assoc "Type" (cdr input-list)))(cdr(assoc type-element type-objects)))))
  (if (= (cadr(assoc "Level" (cdr input-list))) (itoa level))
    (progn
      (if(eq leyer-name nil)(setq leyer-name "0"))
      (if(eq(tblsearch "LAYER" leyer-name)nil)
	(entmake (list'(0 . "LAYER")'(100 . "AcDbSymbolTableRecord")'(100 . "AcDbLayerTableRecord")
		      (cons 2 leyer-name)'(70 . 0)'(6 . "Continuous")'(370 . -3))))
      (if (= type-element "POI")
	(entmake (list '(0 . "POINT")
		       (cons 8 leyer-name)
		       (cons 10 (get-coordinates-by-textstr (vl-string-trim "()" (cadr(assoc "Data" (cdr input-list))))))
		       ))
	(draw-3DPoly-vla-by-list-property
	  (list(cons "spece" model_spece)
	       (cons "Layer" leyer-name)
	       (cons "Coordinates" (get-coordinates-by-textdat (cadr(assoc "Data" (cdr input-list)))))
	       (if (= type-element "POLYGON")(cons "Closed" 1))
	       ))
	)
      );progn
    );if
  )
	
(defun get-coordinates-by-textdat (string / result-list)
  (foreach x (string-to-list (vl-string-trim "()"string) "),(")
    (setq result-list (append result-list
			      (list(get-coordinates-by-textstr x))))
    )
  result-list
  )

(defun get-coordinates-by-textstr (string / )
  (calculation-XYZ-by-BLH (list
			    (atof(substr string 1(vl-string-search "," string)))
			    (atof(substr string(+(vl-string-search "," string)2)))
			    ) 6378137.0 6356752.0); WGS84
  )

(defun string-to-list (_str _br / _pos)
  (if (setq _pos (vl-string-search _br _str))
    (cons (substr _str 1 _pos)
	  (string-to-list (substr _str (+ (strlen _br) _pos 1)) _br))
    (list _str)
    ))

(defun get-type-for-gps-mp ( / file list-type patch result-list text-line temp)
  (setq patch (strcat *geo_tools_root_dir*"\\data\\type-garmin-ru.dat"))
  (if (file-p patch)
    (progn
      (setq file(open patch "r"))
      (setq text-line (read-line file))
      (while text-line
	(if (=(ascii text-line)(ascii "."))
	  (progn
	    (setq list-type(list (substr text-line 2)))
	    (setq text-line (read-line file))
	    (while (not(or (eq text-line nil)(=(ascii text-line)(ascii "."))))
	      (if (not(or(eq text-line nil)(=(ascii text-line)(ascii ";"))(= text-line "")))
		(progn
		  (setq temp (string-to-list text-line(chr 9)))
		  (setq list-type(append list-type(list (list (car temp) (edit-string-replacement-for-leyer-name (cadr temp))))))
		  )
		)
	      (setq text-line (read-line file))
	      );while
	    (setq result-list(append result-list (list list-type)))
	    (setq list-type nil)
	    )
	  (setq text-line (read-line file))
	  );if
	);while
      (close file)
      )
    (alert(strcat"Файл "patch" не найден!"))
    )
  result-list
  )

(defun edit-string-replacement-for-leyer-name (string / x)
  (foreach x (list (list "<" "меньше")
		   (list ">" "больше")
		   (list "/" "-")
		   (list "\\" "-")
		   (list "\"" "'")
		   (list ":" "-")
		   (list ";" "-")
		   (list "?" "_")
		   (list "*" "_")
		   (list "|" "-")
		   (list "," ".")
		   (list "=" "-")
		   (list "`" "~")
		   )
    (while (member (ascii(car x))(vl-string->list string))
      (setq string (vl-string-subst (cadr x) (car x) string))
      )
    )
  string
  )