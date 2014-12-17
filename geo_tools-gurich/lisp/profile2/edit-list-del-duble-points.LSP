(defun edit-list-del-duble-points (input-list / n)
  ; удаляет одинаковые точки следующие друг за другом
  (setq n 0)
  (while (not(or (eq n(length input-list))(> n(length input-list))))
    (if (equal (nth n input-list) (nth (1+ n) input-list) 0.001)
      (setq input-list(edit-list-del-item-by-number (1+ n) input-list))
      (setq n (1+ n))
      )
    )
  input-list
  )