(defun extract_302 (������);
  (
   (lambda (������)
     (if ������
       (cons
	 (cdr (car ������))
	 (extract_302 (cdr ������))
	 )
       )
     )
    (member (assoc 302 ������) ������)
    )
  )
