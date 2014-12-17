(defun calculation_gms_to_radiany (grad minu sek /); переводит градусы минуты секунды в радианы
  (* (/ pi 180)(+ (float grad) (/ (+ (float minu) (/ (float sek) 60)) 60)))
    )
