(defun edit-list-del-item-by-number (number input-list / n item result-list flag)
  ; удаляет элемент из списка по номеру начиная с нулевого
  (setq n 0)
  (repeat (length input-list)
    (if (not(eq n number))
      (setq result-list(append result-list(list (nth n input-list))))
      )
    (setq n (1+ n))
    )
  result-list
  )