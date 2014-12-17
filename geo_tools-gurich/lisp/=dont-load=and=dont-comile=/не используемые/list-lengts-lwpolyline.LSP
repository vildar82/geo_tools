(defun list-lengts-lwpolyline (line-obj / list-coor-points p1 p2 result)
  ;возвращает список длинн полилинии
  (setq list-coor-points (list-coordinates-lwpolyline line-obj))
  (repeat (1-(length list-coor-points))
	   (setq p1 (car list-coor-points))
	   (setq list-coor-points (cdr list-coor-points))
	   (setq p2 (car list-coor-points))
	   (setq result (append result (list (distance  p1 p2))))
	   )
  result
  )