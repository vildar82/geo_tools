(defun convert-str-to-deg-grad (string / pos)
  ;������������ ������ ���� "123.4556" (� ������� 123 ������� 45 ������ 56 �������) � ���������� �������
  (setq pos (vl-string-search "." string))
    (+
      (atof(substr string 1 pos))
      (/
	(+
	  (*
	    (atof(substr string (+ pos 2) 2))
	    60)
	  (atof(substr string (+ pos 4) 2)))
	3600
	)
      )
  )