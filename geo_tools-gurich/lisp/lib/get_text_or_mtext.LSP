(defun get_text_or_mtext (/ text)
  (while (= text nil)
    (setq text (car (entsel "\n������ �����: ")))
    (if
      (not (= text nil))
      (if
	(not (or (eq (cdr (assoc 0 (entget text))) "TEXT") (eq (cdr (assoc 0 (entget text))) "MTEXT")))
	(progn
	  (setq text nil)
	  (princ "\n������! ������ ������ ����������������� ����!")
	  ); progn
	); if
      (princ "\n������! ����� �� ������!")
      ); if
    ); while
  text
  )
