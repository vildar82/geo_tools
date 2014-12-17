(defun extract_coord_lwpolyline (entget-list)
  ; функция извлечения координат вершин 2Dполилинии
  (
   (lambda (data)
     (if data
       (cons
	 (cdr (car data))
	 (extract_coord_lwpolyline (cdr data))
	 )
       )
     )
    (member (assoc 10 entget-list) entget-list)
    )
  )
