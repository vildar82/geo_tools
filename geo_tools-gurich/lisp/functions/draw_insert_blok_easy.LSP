(defun C:draw_insert_blok_easy ( / block_name n_y); ������� ��������� �����
(geo_tools-help "draw_insert_blok_easy")
  (initget 1 )(setq block_name (getstring T "\n����� ��� �����: "))
  (initget 1 "�� ��� _Yes _No")(setq n_y (getkword "\n����� ������������ ����? [��/���]: "))
  (princ "\n����� ����� ������� �����: ")
  (if (not (or (eq block_name nil) (eq n_y nil)))
    (progn     (if
	(not (tblobjname "block" block_name))
	(progn
	  (command "_insert" block_name "0,0,0" "1" "1" "0")
	  (entdel (entlast))
	  )
	)
      (while T
	(command "_insert" block_name pause "1" "1" (if (not(eq n_y "Yes"))0.0 pause))
	)
      )
    (princ "�� ������ ������!")
    )
  (princ)
  );C:draw_insert_blok_easy
