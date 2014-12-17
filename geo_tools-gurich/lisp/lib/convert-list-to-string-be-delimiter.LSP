(defun convert-list-to-string-be-delimiter (input-list delimiter / result)
  ; конвертирует список текстов в строку вставл€€ между элементами строку delimiter
  (setq result (car input-list))
  (setq input-list (cdr input-list))
  (foreach x input-list
    (setq result (strcat result delimiter x))
    )
  result
  )