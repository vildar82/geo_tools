(defun get-item-deep-list (list-num-items input-list / flag n item)
  ; ���������� ���������� �������� ������ � ������������ ����������� �� �������
  ; ������ (get-item-deep-list (list 0 1 10) big-list)
  ; � ������������ ������ ������ ������� 0 � ������� ������ ������� 1
  ; � ������� ������� 10 ������� � ������ ���� ������ (��������� ������� �� ������
  ; ��� �������� � ����� ������� �� ����������) ������ nil
  (setq n 0)
  (while (not flag)
    (if (= n (length list-num-items))
      (setq flag T)
      (if (= (type input-list) 'list)
	(progn
	  (setq item (nth n list-num-items))
	  (setq input-list (nth item input-list))
	  )
	(setq flag T
	      input-list nil)
	)
      )
    (setq n (1+ n))
    )
  input-list
  )