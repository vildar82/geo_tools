(defun calculation_h_triang_be_2point (p1 p2 p3 / a b c p) ; вычисляет высоту треугольника со 2й точки
  (setq a (distance p1 p3))
  (setq b (distance p1 p2))
  (setq c (distance p2 p3))
  (setq p (/(+ a b c)2))
  (* (/ 2 a) (sqrt (* p (- p a)(- p b)(- p c))))
  )
