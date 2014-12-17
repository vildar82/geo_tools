(defun calculation_h_text_in_triang (t1 t2 t3 k /); вычисляет высоту текста в треугольнике t1 прямой угол k отношение высоты к длинне текста
  (/ (* k (distance t1 t3) (distance t1 t2))(+(* k (distance t1 t3)) (distance t1 t2)))
  )
