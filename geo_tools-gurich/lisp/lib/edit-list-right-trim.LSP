(defun edit-list-right-trim (input-list number / result-list n)
  ; возвращает усеченный с права список указанной длинной
  ; при ошибке - указанная длинна больше реальной длинны списка nil
  (if (< (length input-list) number)
    nil
    (progn
      (setq n 0)
      (repeat number
	(setq result-list (append result-list (list (nth n input-list))))
	(setq n (1+ n))
	)
      result-list
      )
    )
  )