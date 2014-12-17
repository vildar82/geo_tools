(defun analisis-point-in-contur (list-points-contur point / a a0 list-angle-contur n point x)
  ;проверка попадания точки в контур из списка точек
  (setq list-angle-contur (mapcar '(lambda (x) (angle point x)) list-points-contur)
	a0 (last list-angle-contur))
  (setq n 0)
  (foreach x list-angle-contur
    (setq a (- x a0) a0 x)
    (or (<= (abs a) Pi) (setq n (1+ n)))
    )
  (if (/=(rem n 2)0)
    T
    nil)
  )