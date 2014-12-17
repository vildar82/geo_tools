(defun get_text_only (/ text)
  (while (= text nil)
    (setq text (car (entsel "\nВыбери текст: ")))
    (if
      (not (= text nil))
      (if
	(not (eq (cdr (assoc 0 (entget text))) "TEXT"))
	(progn
	  (setq text nil)
	  (princ "\nОШИБКА! Выбран объект неподдерживаемого типа!")
	  ); progn
	); if
      (princ "\nОШИБКА! Текст не выбран!")
      ); if
    ); while
  text
  )
