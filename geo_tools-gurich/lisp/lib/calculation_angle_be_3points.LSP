(defun calculation_angle_be_3points (p1 p2 p3 / a b g)
  ; ��������� ���� ����� ����� ������� �� ���������
  (setq a (angle p2 p1))
  (setq b (angle p2 p3))
  (setq g(if(> a b)
	   (- a b)
	   (-(* pi 2)(- b a))
	   )
	)
  )
