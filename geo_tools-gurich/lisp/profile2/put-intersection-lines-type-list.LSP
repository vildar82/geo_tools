(defun put-intersection-lines-type-list (input-list / result-list)
  (foreach x input-list
    (setq result-list(append result-list (list (car x ))))
    (setq result-list(append result-list (list (cadr x ))))
    )
  (file_write_list_to_file (strcat *geo_tools_user_dat_dir* "\\data\\intersection-lines-list.dat") result-list)
  )