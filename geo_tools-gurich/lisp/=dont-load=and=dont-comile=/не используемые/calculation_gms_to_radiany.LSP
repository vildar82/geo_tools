(defun calculation_gms_to_radiany (grad minu sek /); ��������� ������� ������ ������� � �������
  (* (/ pi 180)(+ (float grad) (/ (+ (float minu) (/ (float sek) 60)) 60)))
    )
