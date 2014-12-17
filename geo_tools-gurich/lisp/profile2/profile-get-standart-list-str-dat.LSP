(defun profile-get-standart-list-str-dat ( / big-list slall-list sort-list)
  ; ¬озвращает содержимое файла profile-standart-str.dat (стандартные строки)
  ; в виде сортированого списка
  (foreach x (file_read_to_list (strcat *geo_tools_user_dat_dir* "\\data\\profile-standart-str.dat"))
    (if (eq x "***")
      (progn
	(setq big-list (append big-list(list slall-list)))
	(setq slall-list nil)
	)
      (progn
	(setq slall-list (append slall-list(list x)))
	)
      )
    )
  big-list
  )