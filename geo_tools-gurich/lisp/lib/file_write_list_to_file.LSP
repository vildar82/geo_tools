(defun file_write_list_to_file (patch list_cell / file); список -> файл
  (setq file (open patch "w"))
  (repeat (length list_cell)
    (write-line (car list_cell) file)
    (setq list_cell (cdr list_cell)))
  (close file)
  )
