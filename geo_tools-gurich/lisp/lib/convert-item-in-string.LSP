(defun convert-item-in-string (item accuracy /)
  ;�� ����� �������� �� ������ ������
  (cond
    ((= (type item) 'INT)
     (itoa item)
     )
    ((= (type item) 'REAL)
     (rtos item 2 accuracy)
     )
    ((= (type item) 'STR)
     item
     )
    (T
     nil)
    )
  )