(defun calculation_radiany_to_gms (rad /)
  ; ��������� ������� � ��������� ������ ������� ������ ������� 180d30'15"
  (strcat (itoa(fix(/ rad (/ pi 180))))
	  "d"
	  (itoa(fix(*(-(/ rad (/ pi 180))(fix(/ rad (/ pi 180))))60)))
	  "'"
	  (itoa(fix(*(-(*(-(/ rad (/ pi 180))(fix(/ rad (/ pi 180))))60)(fix(*(-(/ rad (/ pi 180))(fix(/ rad (/ pi 180))))60)))60)))
	  "\"")
  )
