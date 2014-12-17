(defun get_2dpoly (str / pline); запрос указания полилинии
  (while (= pline nil)
    (setq pline (entget (car (entsel str))))
    (if
      (not (= pline nil))
      (if
	(not (eq (cdr (assoc 0 pline)) "LWPOLYLINE"))
	(progn
	  (setq pline nil)
	  (princ "\nОШИБКА! Выбран объект неполилиния!")
	  ); progn
	); if
      (princ "\nОШИБКА! Полилиния не выбрана!")
      ); if
    )
  pline
  )
