(defun get_2dpoly (str / pline); ������ �������� ���������
  (while (= pline nil)
    (setq pline (entget (car (entsel str))))
    (if
      (not (= pline nil))
      (if
	(not (eq (cdr (assoc 0 pline)) "LWPOLYLINE"))
	(progn
	  (setq pline nil)
	  (princ "\n������! ������ ������ �����������!")
	  ); progn
	); if
      (princ "\n������! ��������� �� �������!")
      ); if
    )
  pline
  )
