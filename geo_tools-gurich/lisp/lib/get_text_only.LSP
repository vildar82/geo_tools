(defun get_text_only (/ text)
  (while (= text nil)
    (setq text (car (entsel "\n������ �����: ")))
    (if
      (not (= text nil))
      (if
	(not (eq (cdr (assoc 0 (entget text))) "TEXT"))
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
