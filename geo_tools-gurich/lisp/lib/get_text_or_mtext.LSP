(defun get_text_or_mtext (/ text)
  (while (= text nil)
    (setq text (car (entsel "\nВыбери текст: ")))
    (if
      (not (= text nil))
      (if
	(not (or (eq (cdr (assoc 0 (entget text))) "TEXT") (eq (cdr (assoc 0 (entget text))) "MTEXT")))
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
