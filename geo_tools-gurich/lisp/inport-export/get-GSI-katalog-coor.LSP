(defun get-GSI-katalog-coor (put-file / file_points_list file str temp)
  (setq file (open put-file"r"))
  (while(setq str (read-line file))
    (if (setq temp (edit-string-clear-gsi str))
      (setq file_points_list
	     (append file_points_list
		     (list temp))))
    )
  (close file)
  file_points_list
  )