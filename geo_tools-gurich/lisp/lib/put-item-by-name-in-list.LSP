(defun put-item-by-name-in-list (input-list name item / )
  ;��������� �������� ��������� � ������������� ������
  (if item
    (if (assoc name input-list)
    (subst(list name item)(assoc name input-list)input-list)
      (append input-list (list (list name item))))
    (vl-remove (assoc name input-list) input-list))
  )