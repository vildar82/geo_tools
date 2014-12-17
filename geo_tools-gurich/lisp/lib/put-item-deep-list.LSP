(defun put-item-deep-list (list-num-items input-list put-item / result-list n flag)
  ; заменяет содержимое элемента списка с произвольной вложеностью по номерам
  (if (get-item-deep-list list-num-items input-list)
    (progn
      (setq n (1-(length list-num-items)))
      (while (not flag)
	(if (< n 0)
	  (setq flag T)
	  (setq put-item (replase-item-in-list-by-number
			   put-item
			   (nth n list-num-items)
			   (get-item-deep-list (edit-list-right-trim list-num-items n) input-list)
			   ))
	  )
	(setq n (1- n))
	)
      put-item
      )
    nil)
  )

