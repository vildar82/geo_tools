(defun calculation_gms_to_dec (grad minu sek /); ��������� ������� ������ ������� � ���������� �������
  (+(float grad)(/ (+ (float minu)(/ (float sek)60))60))
    )
