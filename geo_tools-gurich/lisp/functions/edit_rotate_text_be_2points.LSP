(defun C:edit_rotate_text_be_2points (/ t1 a object ttt flag vla-object) ; Поворачивает выбранный текст по двум точкам (раньше работала с точками)
  (geo_tools-help "edit_rotate_text_be_2points")
  
  (if
    (setq object (car (entsel "\nВыбери текст или пикет: ")))
    (if
      (not (or (eq (cdr (assoc 0 (entget object))) "TEXT")(eq (cdr (assoc 0 (entget object))) "MTEXT")(eq (cdr (assoc 0 (entget object))) "AECC_COGO_POINT")))
      (progn
	(setq object nil)
	(princ "\nОШИБКА! Выбран объект неподдерживаемого типа!")
	); progn
      (progn
	(initget)(setq t1(getpoint "\nУкажи начальную точку линии ориентира: ")); указываю точку
	(if (eq t1 nil)
	  (setq a nil)
	  (progn
	    (initget)(setq a (getangle t1 "\nВведи угол поворота или укажи вторую точку ориентира: ")); указываю угол
	    ))
	(if (= a nil)
	  (setq a (* pi 1.5))
	  ); горизонтальное положение объекта при пустом вводе
	(vla-StartUndoMark active_document)
	(while (not flag)
	  (setq ttt (entget object))
	  (cond
	    ((eq (cdr(assoc 0 ttt)) "AECC_COGO_POINT")
	     (setq vla-object (vlax-ename->vla-object object))
	     (vlax-put-property vla-object 'LabelRotation (+ a (/ pi 2)))
	     )
	    ((or (eq (cdr(assoc 0 ttt)) "TEXT") (eq (cdr(assoc 0 ttt)) "MTEXT"))
	     (setq ttt (subst (cons 50 (+ a (/ pi 2)))(assoc 50 ttt) ttt))
	     (entmod ttt)
	     (entupd object)
	     )
	    ); cond
	  (if (setq object (car (entsel "\nВыбери текст или пикет: ")))
	    (if (not (or (eq (cdr (assoc 0 (entget object))) "TEXT")(eq (cdr (assoc 0 (entget object))) "MTEXT")(eq (cdr (assoc 0 (entget object))) "AECC_COGO_POINT")))
	      (progn
		(princ "\nОШИБКА! Выбран объект неподдерживаемого типа!")
		(setq flag T)
		)
	      )
	    (progn
	      (princ "\nНичего не выбрано!")
	      (setq flag T)
	      )
	    )
	  )
	(vla-EndUndoMark active_document)
	
	
	)
      ); if
    (princ "\nНичего не выбрано!")
    ); if
  
  (princ)
  )
