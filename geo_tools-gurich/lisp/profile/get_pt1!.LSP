(defun get_pt1! (/ t1!_); ������ �� ���� ����� �� �������� ���������
  (if
    (eq t1! nil)
    (progn
      (initget)(setq t1! (getpoint "\n����� ����� �� �������� ���������: "))
      )
    (progn
      (setq t1!_ t1!)
      (initget)
      (setq t1! (getpoint "\n����� ����� �� �������� ��������� <�������>: "))
      (if (eq t1! nil)
	(setq t1! t1!_)
	)
      )
    )
  t1!
  )
