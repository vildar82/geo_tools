(defun edit-list-add-item-by-number (item number input-list / n result-list flag)
  ; добавл€ет элимент в список на указаную позицию
  (setq n 0)
  (repeat (1+(length input-list))
    (if (eq n (1+ number))
      (progn(setq result-list(append result-list(list item)))(setq flag T))
      (setq result-list(append result-list(list (nth (if flag(1- n)n) input-list))))
      )
    (setq n (1+ n))
    )
  result-list
  )