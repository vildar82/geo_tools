(defun profile-get-sorting-list-profile-dat ( / big-list slall-list sort-list)
  ; Возвращает содержимое файла basement-in-profile.dat (подвалы профилей)
  ; в виде сортированого списка
  (foreach x (file_read_to_list (strcat *geo_tools_user_dat_dir* "\\data\\basement-in-profile.dat"))
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
  (foreach x big-list
    (setq sort-list(append sort-list(list(sorting-list-profile-dat x))))
    )
  sort-list
  )