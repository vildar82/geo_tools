(defun convert-str-be-type (str-item  type-item /)
;�� ����� ��� ������ �� ������ �������� � �������� �� ������ ������ ����
  (cond
    ((= type-item "INT")
     (atoi str-item)
     )
    ((= type-item "REAL")
     (atof str-item)
     )
    ((= type-item "STR")
     str-item
     )
    (T
     nil)
    )
  )