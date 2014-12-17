(defun put-sorting-list-standart-str (input-list / result-list)
  ; записывает сортированный список подвалов профилей в файл basement-in-profile.dat 
  (foreach str-prof input-list
    (foreach eliment-str str-prof
      (setq result-list(append result-list (list eliment-str)))
      )
    (setq result-list(append result-list (list "***")))
    )
  (file_write_list_to_file (strcat *geo_tools_user_dat_dir* "\\data\\profile-standart-str.dat") result-list)
  )