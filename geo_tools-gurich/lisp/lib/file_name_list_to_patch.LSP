(defun file_name_list_to_patch (filename_list patch / result_list)
  ; возвращает список с путем добавленым к директориям
  (repeat (length filename_list)
    (setq result_list(append result_list(list(strcat patch "\\"(car filename_list)))))
    (setq filename_list (cdr filename_list))
    )
  result_list
  )