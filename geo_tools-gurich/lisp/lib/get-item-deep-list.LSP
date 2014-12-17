(defun get-item-deep-list (list-num-items input-list / flag n item)
  ; возвращает содержимое элемента списка с произвольной вложеностью по номерам
  ; пример (get-item-deep-list (list 0 1 10) big-list)
  ; в передаваемом списке найдет элемент 0 в котором найдет элемент 1
  ; в котором элемент 10 который и вернет если ошибке (следующий элемент не список
  ; или элемента с таким номером не существует) вернет nil
  (setq n 0)
  (while (not flag)
    (if (= n (length list-num-items))
      (setq flag T)
      (if (= (type input-list) 'list)
	(progn
	  (setq item (nth n list-num-items))
	  (setq input-list (nth item input-list))
	  )
	(setq flag T
	      input-list nil)
	)
      )
    (setq n (1+ n))
    )
  input-list
  )