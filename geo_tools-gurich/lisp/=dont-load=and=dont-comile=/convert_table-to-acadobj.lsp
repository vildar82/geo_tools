;;;(defun C:proba ()
;;;  ;чертит отступление от проекта по одной точке и линии
;;;  (initget)(setq p-izm(getpoint "\nУкажи измереную точку<Выход>:"))
;;;  (if p-izm
;;;    (if (setq line-obj(car(entsel "\nВыбери проектную линию<Выход>:")))
;;;      (if (not(or (eq (cdr(assoc 0 (entget line-obj)))"LINE")(eq (cdr(assoc 0 (entget line-obj)))"LWPOLYLINE"))); контроль
;;;	(progn
;;;	  (if (eq(cdr(assoc 0(entget line-obj)))"LINE")
;;;	    (progn
;;;	      
;;;	      )
;;;	    (progn
;;;	      
;;;	      )
;;;	    )
;;;	  )
;;;	(princ"\nВыбран недопустимый объект! Выбирать надо линию или полилинию!")
;;;	)
;;;      (princ"\nНичего не выбрано!")
;;;      )
;;;    )
;;;  (princ)
;;;  )
;;;(defun C:proba (/ LINE-OBJ P-IZM)
;;;  ;чертит отступление от проекта по одной точке и линии
;;;  (initget)(setq p-izm(getpoint "\nУкажи измереную точку<Выход>:"))
;;;  (if p-izm
;;;    (if (setq line-obj(car(entsel "\nВыбери проектную линию<Выход>:")))
;;;      (if (eq (cdr(assoc 0 (entget line-obj)))"LWPOLYLINE"); контроль
;;;	(progn
;;;	  (if (analisis-point-in-contur (list-coordinates-lwpolyline line-obj)p-izm)
;;;	    (princ "\nПопадает!")
;;;	    (princ "\nНепопадает!"))
;;;	  )
;;;	(princ"\nВыбран недопустимый объект! Выбирать надо полилинию!")
;;;	)
;;;      (princ"\nНичего не выбрано!")
;;;      )
;;;    )
;;;  (princ)
;;;  )

;;;(defun C:proba (/ LINE-OBJ P-IZM)
;;;    (if (setq line-obj(car(entsel "\nВыбери проектную линию<Выход>:")))
;;;      (if (eq (cdr(assoc 0 (entget line-obj)))"LWPOLYLINE"); контроль
;;;	(progn
;;;	  (setq list-points (list-coordinates-lwpolyline line-obj))
;;;	  (foreach x list-points
;;;	    (setq list-text (append list-text (list (strcat "'("(rtos (car x)2 3)" "(rtos (cadr x)2 3)")"))))
;;;	    )
;;;	  (setq file-name(cdr(assoc 1(entget (car(entsel "\nВыбери АО:"))))))
;;;	  (file_write_list_to_file (strcat "D:\\temp\\" file-name ".txt")list-text)
;;;	  )
;;;	(princ"\nВыбран недопустимый объект! Выбирать надо полилинию!")
;;;	)
;;;      (princ"\nНичего не выбрано!")
;;;      )
;;;  (princ)
;;;  )

;;;(defun C:proba (/ P-IZM LIST-AO LIST-SELEKT-AO LIST-SELEKT-DISTRICT N POINT X)
;;;  (initget)(setq point(getpoint "\nУкажи измереную точку<Выход>:"))
;;;  (if point
;;;    (progn
;;;      (setq list-AO (list"ВАО" "ЗАО" "САО" "СВАО" "СЗАО" "ЦАО" "ЮАО" "ЮВАО" "ЮЗАО"))
;;;      (foreach x list-AO
;;;	(if (analisis-point-in-contur (get-point-list-AO x)point)
;;;	  (setq list-selekt-AO (append list-selekt-AO (list x)))
;;;	  )
;;;	)
;;;;;;      (setq n 1)
;;;;;;      (repeat 12
;;;;;;	(if (analisis-point-in-contur (get-point-list-MTK-district n)point)
;;;;;;	  (setq list-selekt-district (append list-selekt-district (list x)))
;;;;;;	  )
;;;;;;	(setq n (1+ n))
;;;;;;	)
;;;      (print list-selekt-AO)
;;;      (terpri)
;;;;;;      (print list-selekt-district)
;;;      )
;;;    )
;;;  (princ)
;;;  )

;;;(defun C:proba++ (/ LIST-AO)
;;;  (setq list-AO (list"ВАО"))
;;;  (foreach x list-AO
;;;    (draw-lwpolyline-vla-by-list-property
;;;      (list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
;;;	   (cons "Coordinates" (get-point-list-AO x))
;;;	   ))
;;;    )
;;;  (princ)
;;;  )

;;;(defun C:proba (/ nabor gorisontal-0-lines vertical-1-lines texts collums-lines rows-lines c1 c2 temp n table_obj)
;;;  (if (setq nabor (ssget '((0 . "LINE,TEXT"))))
;;;    (progn
;;;      (foreach x (convert_ss_to_list nabor)
;;;	(if (eq (cdr(assoc 0 (entget x)))"LINE")
;;;	  (cond
;;;	    ((equal(nth 0(cdr(assoc 10 (entget x))))(nth 0(cdr(assoc 11 (entget x))))(/(distance (cdr(assoc 10 (entget x)))(cdr(assoc 11 (entget x))))100))
;;;	     (setq gorisontal-0-lines (append gorisontal-0-lines (list (list(cdr(assoc 10 (entget x)))(cdr(assoc 11 (entget x)))))))
;;;	     )
;;;	    ((equal(nth 1(cdr(assoc 10 (entget x))))(nth 1(cdr(assoc 11 (entget x))))(/(distance (cdr(assoc 10 (entget x)))(cdr(assoc 11 (entget x))))100))
;;;	     (setq vertical-1-lines (append vertical-1-lines (list (list(cdr(assoc 10 (entget x)))(cdr(assoc 11 (entget x)))))))
;;;	     )
;;;	    )
;;;	  (setq texts (append texts (list (list(cdr(assoc 1 (entget x)))(cdr(assoc 40 (entget x)))(if (equal(cdr(assoc 11 (entget x)))'(0.0 0.0 0.0))
;;;							   (cdr(assoc 10 (entget x)))
;;;							   (cdr(assoc 11 (entget x))))))))
;;;	  )
;;;	);foreach
;;;      (setq gorisontal-0-lines (merge-list-lines-in-list gorisontal-0-lines 0))
;;;      (setq vertical-1-lines (merge-list-lines-in-list vertical-1-lines 1))
;;;      (setq collums-lines (vl-sort(mapcar (function (lambda (x) (nth 0 (car x))))gorisontal-0-lines)'<))
;;;      (setq rows-lines (vl-sort(mapcar (function (lambda (x) (nth 1 (car x))))vertical-1-lines)'>))
;;;      (princ (strcat"\n Столбцов " (itoa (1-(length collums-lines)))))
;;;      (princ (strcat"\n Строк " (itoa (1-(length rows-lines)))))
;;;      (setq table_obj (vla-AddTable model_spece
;;;			(vlax-3D-point (getpoint))
;;;			(1-(length rows-lines))
;;;			(1-(length collums-lines))
;;;			(- (car rows-lines)(cadr rows-lines))
;;;			(- (cadr collums-lines)(car collums-lines))))
;;;      (vla-put-StyleName table_obj "Координаты")
;;;      (setq n 0)
;;;      (setq temp collums-lines)
;;;      (repeat (1-(length collums-lines))
;;;	(setq c1 (car temp))
;;;	(setq temp (cdr temp))
;;;	(setq c2 (car temp))
;;;	(vla-SetColumnWidth table_obj n (abs(- c2 c1)))
;;;	(setq n (1+ n))
;;;	)
;;;      
;;;      (setq n 0)
;;;      (setq temp rows-lines)
;;;      (repeat (1-(length rows-lines))
;;;	(setq c1 (car temp))
;;;	(setq temp (cdr temp))
;;;	(setq c2 (car temp))
;;;	(vla-SetRowHeight table_obj n (abs(- c1 c2)))
;;;	(setq n (1+ n))
;;;	)
;;;	     
;;;;;;      (vla-SetColumnWidth table_obj 0 8)
;;;;;;      (vla-SetColumnWidth table_obj 0 8)
;;;;;;      (vla-SetText table_obj 1 0 "№\nп/п")
;;;;;;      (vla-SetCellAlignment table_obj 1 0 acMiddleCenter)
;;;;;;      (vla-SetText table_obj 1 1 " Наимено-\n вание")
;;;;;;      (vla-SetCellAlignment table_obj 1 1 acMiddleLeft)
;;;;;;      (vla-SetText table_obj 1 2 "X")
;;;;;;      (vla-SetCellAlignment table_obj 1 2 acMiddleCenter)
;;;;;;      (vla-SetCellTextHeight table_obj 1 2 2.8)
;;;;;;      (vla-SetText table_obj 1 3 "Y")
;;;;;;      (vla-SetCellAlignment table_obj 1 3 acMiddleCenter)
;;;;;;      (vla-SetCellTextHeight table_obj 1 3 2.8)
;;;      
;;;      )
;;;    )
;;;  (princ)
;;;  )

(defun merge-list-lines-in-list (input-list ne / n current-item result-list)
  ;объединяет линии в списке (список из пары точек)
  (while (/= 0 (length input-list))
    (setq current-item (car input-list))
    (setq input-list (cdr input-list))
    (setq n 0)
    (while (not(or(> n (length input-list))(= n (length input-list))))

      (if (equal (nth ne (car current-item))(nth ne (car(nth n input-list)))0.001)
	(progn
	  (setq current-item (merge-lines-in-list (list current-item (nth n input-list)) ne))
	  (setq input-list(edit-list-del-item-by-number n input-list))
	  )
	(setq n (1+ n))
	)
      )
    (setq result-list (append result-list (list current-item)))
    )
  result-list
  )

(defun merge-lines-in-list (input-list flag / os-list)
  (setq flag (if (= flag 0) 1 0))
  (setq os-list (mapcar (function (lambda (x) (nth flag x)))(apply 'append input-list)))
  (if (= flag 0)
    (list
      (list
	(apply 'max os-list)
	(nth 1 (nth 0(nth 0 input-list)))
	0.0
	)
      (list
	(apply 'min os-list)
	(nth 1 (nth 0(nth 0 input-list)))
	0.0
	)
      )
    (list
      (list
	(nth 0 (nth 0(nth 0 input-list)))
	(apply 'max os-list)
	0.0
	)
      (list
	(nth 0 (nth 0(nth 0 input-list)))
	(apply 'min os-list)
	0.0
	)
      )
    )
  )

;;;  (setq ne 1)
;;;  (setq input-list (list (list '(0.0 15.0 0.0) '(0.0 10.0 0.0))(list '(0.0 50.0 0.0) '(0.0 60.0 0.0))))
;;;  (vl-sort (apply 'append input-list) (function (lambda (e1 e2) (> (nth 0 e1)(nth 0 e2)))))
;;;  (apply 'car (apply 'append input-list))