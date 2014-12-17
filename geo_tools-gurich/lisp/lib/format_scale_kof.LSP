(defun format_scale_kof (); возврвщает масштабный коэффициент в зависимости от положения в Модели или в листе
  (if(= (getvar "CTAB")"Model")
    (getvar "CANNOSCALEVALUE")1)
  ); format_scale_kof
