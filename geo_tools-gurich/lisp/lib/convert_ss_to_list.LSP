(defun convert_ss_to_list ( nabor / n _list) ; конвертирует набор в список
     (setq n 0)
     (if nabor
       (progn
	 (repeat (sslength nabor)
	   (setq _list (append _list (list(ssname nabor n))))
	   (setq n (1+ n)))
	 )
       )
  _list
  )
