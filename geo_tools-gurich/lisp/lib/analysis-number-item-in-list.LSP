(defun analysis-number-item-in-list (item input-list /)
  ; ��������� ���������(�����) �������� � ������, ������ � 0
  ; ���� �� ������ nil
  (if (member item input-list)
  (-(length input-list)(length(member item input-list)))
    nil)
  )