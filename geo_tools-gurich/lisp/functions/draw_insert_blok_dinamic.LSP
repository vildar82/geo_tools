(defun C:draw_insert_blok_dinamic ( / block_name block_name_d n_y); ������� ������������� ��������� �����
(geo_tools-help "draw_insert_blok_dinamic")
  (initget 1 )(setq block_name (getstring T "\n����� ��� �����: "))
  (initget 1 "�� ��� _Yes _No")(setq n_y (getkword "\n����� ������������ ����? [��/���]: "))
  (princ "\n����� ����� ������� �����: ")
  (if (not (or (eq block_name nil) (eq n_y nil)))
    (progn     (if
		 (not (tblobjname "block" block_name))
		 (progn
		   (setq block_name_d (strcat block_name "_d"))
		   (command "_insert" block_name_d "0,0,0" "1" "1" "0")
		   (entdel (entlast))
		   (command "_-purge" "�" block_name_d "�")
		   )
		 )
      (while T
	(command "_insert" block_name pause "1" (if (not(eq n_y "Yes"))0.0 pause))
	)
      )
    (princ "�� ������ ������!")
    )
  (princ)
  )
