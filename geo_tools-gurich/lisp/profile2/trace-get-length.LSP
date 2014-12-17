(defun trace-get-length (list-coor-points / p1 p2 summa)
  ; возвращает длинну трассы по списку координат
  (setq summa 0.0)
  (repeat (1- (length list-coor-points))
	   (setq p1 (car list-coor-points))
	   (setq list-coor-points (cdr list-coor-points))
	   (setq p2 (car list-coor-points))
	   (setq summa (+ summa (distance  p1 p2)))
	   )
  summa
  )