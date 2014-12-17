(defun calculation_gms_to_dec (grad minu sek /); переводит градусы минуты секунды в десятичные градусы
  (+(float grad)(/ (+ (float minu)(/ (float sek)60))60))
    )
