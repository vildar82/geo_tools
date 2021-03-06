(defun conclusion ( / n ent PN P P0 a)
  ;|������� ����������� ������ (2D-��������� �� ��������) � ����� P.
���� ������ ������� � ����� �� ��������� �� ��� �������,
  � ������ �������, ������ ���������� T. ����� - nil.
  ������� ����� �������� � ��������� ���� ��������������� ���������.
  ����� ����������� ��������, ���� ��������� �������� ����
  http://forum.dwg.ru/showthread.php?t=60792&page=2|;
  
  (setq n 0) ;��������� �������� ����� ����������� �������� ��� PX.
  (princ "\n������� ������ (2D-���������).")
  (and
    (setq ent (ssget "_:S" '((0 . "LWPOLYLINE"))))
    (setq PN (vl-remove-if '(lambda (x) (/= (car x) 10))
	       (setq ent (entget (ssname ent 0)))
	       )         )
    (or (= (cdr (assoc 70 ent)) 1) (prompt "\n������ �� �������."))
    (setq P (getpoint "\n������� �����: "))
    (or
      (not (setq P0 (ssget P)))
      (not (ssmemb (cdar ent) P0))
      (prompt "\n����� �� ������� �������.")
      )
    (setq PN (mapcar '(lambda (x) (angle P (cdr x))) PN)
	  P0 (last PN))
    (foreach P PN
      (setq a (- P P0) P0 P)
      (or (<= (abs a) Pi) (setq n (1+ n)))
      )
    (or (/= (rem n 2) 0) (prompt "\n����� ������� �������."))
    (princ "\n����� ������ �������.")
    t
    )
  )