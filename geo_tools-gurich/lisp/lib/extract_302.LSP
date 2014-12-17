(defun extract_302 (список);
  (
   (lambda (данные)
     (if данные
       (cons
	 (cdr (car данные))
	 (extract_302 (cdr данные))
	 )
       )
     )
    (member (assoc 302 список) список)
    )
  )
