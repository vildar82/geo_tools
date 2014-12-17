(defun get-curent-version-geo_tools (/ string)
  (setq string (car(file_read_to_list(strcat *geo_tools_root_dir* "\\version.txt"))))
  (if string
    (progn
      (setq string (edit-string-replase-str "0" " " string))
      (list(strcat (substr string 1 4)"."(substr string 5 2)"."(substr string 7 2)" " (substr string 9 2) ":" (substr string 11 2))(atof string))
      )
    (list "<не обнаружено>" 0)
    )
  )