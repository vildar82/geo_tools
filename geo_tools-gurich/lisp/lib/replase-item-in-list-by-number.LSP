(defun replase-item-in-list-by-number (item number input-list / n result-list)
  ; замена элимента списка с указаным номером на указаный элимент
  (setq n 0)
  (repeat (length input-list)
    (if (eq n number)
      (setq result-list(append result-list(list item)))
      (setq result-list(append result-list(list (nth n input-list))))
      )
    (setq n (1+ n))
    )
  result-list
  )