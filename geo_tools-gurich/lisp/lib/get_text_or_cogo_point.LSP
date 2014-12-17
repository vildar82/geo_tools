(defun get_text_or_cogo_point (/ text)
  (while (= text nil)
    (setq text (car (entsel "\nВыбери текст или пикет: ")))
    (if
      (not (= text nil))
      (if
	(not (or (eq (cdr (assoc 0 (entget text))) "TEXT") (eq (cdr (assoc 0 (entget text))) "AECC_POINT")))
	(progn
	  (setq text nil)
	  (princ "\nОШИБКА! Выбран объект неподдерживаемого типа!")
	  ); progn
	); if
      (princ "\nОШИБКА! Текст или пикет не выбран!")
      ); if
    ); while
  text
  )
