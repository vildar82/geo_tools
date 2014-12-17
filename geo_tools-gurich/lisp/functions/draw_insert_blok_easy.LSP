(defun C:draw_insert_blok_easy ( / block_name n_y); вставка условного знака
(geo_tools-help "draw_insert_blok_easy")
  (initget 1 )(setq block_name (getstring T "\nВведи имя блока: "))
  (initget 1 "Да Нет _Yes _No")(setq n_y (getkword "\nБедем поворачивать блок? [Да/Нет]: "))
  (princ "\nУкажи точку вставки блока: ")
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
    (princ "Не верные данные!")
    )
  (princ)
  );C:draw_insert_blok_easy
