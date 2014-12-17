(defun edit_str_adding_probel (text kol_sym flag / ); flag - T добавлять в начало nil в конец
    (repeat (- kol_sym (strlen text))
      (if flag
	(setq text (strcat " " text))
	(setq text (strcat text " "))
	)
      )
    text
    )
