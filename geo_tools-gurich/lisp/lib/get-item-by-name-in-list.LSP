(defun get-item-by-name-in-list (input-list name  / temp)
  ;��������� �������� ��������� � ������������� ������
  (if (=(length(setq temp(cdr(assoc name input-list))))1)
    (car temp)
    temp)
  )