(defun draw_temp_X (ptdraw color / len ) ; ������ ��������� ������ � �������� ����� ��������� ������
  (setq len (* 0.03 (getvar "VIEWSIZE")))
  (grvecs (list color (polar ptdraw (* pi 1.25) len)
		(polar ptdraw (* pi 0.25) len)
		(polar ptdraw (* pi 1.75) len)
		(polar ptdraw (* pi 0.75) len)))
  )
