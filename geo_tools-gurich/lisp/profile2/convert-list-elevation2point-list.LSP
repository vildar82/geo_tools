(defun convert-list-elevation2point-list (input-list / result)
  (foreach x input-list
    (setq result (append result (list (list 0.0 0.0 (car x)))))
    )
  result)