(defun file_read_to_list (patch / file list_cell cell)
  ; файл -> список
  (if (file_check patch)
    (progn
      (setq file(open patch "r"))
      (setq cell (read-line file))
      (while cell
	(setq list_cell(append list_cell (list cell)))
	(setq cell (read-line file)))
      (close file)
      list_cell
      )
    nil
    )
  )
