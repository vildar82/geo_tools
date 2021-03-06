(defun C:edit_text_move_to_center_building ( / t1 t2 t3 t4 t5 c1 d longa long tx1 ttt txy t6 h) ; ���������� ������������ ������� � ������ ������
(geo_tools-help "edit_text_move_to_center_building")
  (setq tx1 (get_text_only))
  (initget)(setq t1(getpoint "\n����� ������ �����:")); �������� �����
  (initget)(setq t2(getpoint t1 "\n����� ������ �����:")); �������� ������ �����
  (initget)(setq t3(getpoint t2 "\n����� ������ �����:")); �������� ������ �����
  (setq �1(+(angle t2 t1) (* 90 (/ pi 180)))); �������� ���� �� t4
  (setq d(*(distance t2 t3) (cos (-(* 180 (/ pi 180)) (- (- (angle t2 t1)(angle t2 t3)) (* 90 (/ pi 180))))))); �������� ���������� �� t4
  (setq t4(polar t2 �1 d)); �������� ���������� �� t4
  (setq t5(polar t4 (angle t2 t1) (distance t1 t2))); �������� ���������� �� t5
  (setq t6
	 (polar
	   (polar t1 (angle t1 t2) (/ (distance t1 t2) 2))
	   (angle t1 t5)
	   (/ (distance t1 t5) 2)
	   ); polar
	); setq t6
  (if (> (distance t1 t2)
	 (distance t1 t5)
	 ); ����� �������
    (progn
      (setq longa (angle t1 t2))
      (setq shotd (distance t1 t5))
      ); progn
    (progn
      (setq longa (angle t1 t5))
      (setq shotd (distance t1 t2))
      ); progn
    ); if
  (setq long (analysis_angle_be_long_distance longa))
  (setq ttt (entget tx1))
  (cond
    ((eq (cdr (assoc 7 ttt)) "OTI")
     (if
       (>
	 (+ (/ 1 (getvar "CANNOSCALEVALUE"))(/ (/ 1.6 (getvar "CANNOSCALEVALUE")) 1.07372))
	 shotd); ������� �� ����������� ������ �����
       (progn
	 (setq h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.07372))
	 (setq long 0)
	 (princ "������� �� ������ � ������, �������� ���������� �����.")
	 )
       (progn
	 (setq h (- shotd (/ 1 (getvar "CANNOSCALEVALUE"))))
	 (if
	   (> h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.07372)); ������� �� ������������ ������ �����
	   (setq h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.07372))
	   ); if
	 ); progn
       ); if
     )
    ((eq (cdr (assoc 7 ttt)) "OTI")
     (if
       (>
	 (+ (/ 1 (getvar "CANNOSCALEVALUE"))(/ (/ 1.6 (getvar "CANNOSCALEVALUE")) 1.05994))
	 shotd); ������� �� ����������� ������ �����
       (progn
	 (setq h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.05994))
	 (setq long 0)
	 (princ "������� �� ������ � ������, �������� ���������� �����.")
	 )
       (progn
	 (setq h (- shotd (/ 1 (getvar "CANNOSCALEVALUE"))))
	 (if
	   (> h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.05994)); ������� �� ������������ ������ �����
	   (setq h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.05994))
	   ); if
	 ); progn
       ); if
     )
    ((eq (cdr (assoc 7 ttt)) "OTI")
     (if
       (>
	 (+ (/ 1 (getvar "CANNOSCALEVALUE"))(/ 1.6 (getvar "CANNOSCALEVALUE")))
	 shotd); ������� �� ����������� ������ �����
       (progn
	 (setq h (/ 2 (getvar "CANNOSCALEVALUE")))
	 (setq long 0)
	 (princ "������� �� ������ � ������, �������� ���������� �����.")
	 )
       (progn
	 (setq h (- shotd (/ 1 (getvar "CANNOSCALEVALUE"))))
	 (if
	   (> h (/ 2 (getvar "CANNOSCALEVALUE"))); ������� �� ������������ ������ �����
	   (setq h (/ 2 (getvar "CANNOSCALEVALUE")))
	   ); if
	 ); progn
       ); if
     )
    (T
     (if
       (>
	 (+ (/ 1 (getvar "CANNOSCALEVALUE"))(/ 1.6 (getvar "CANNOSCALEVALUE")))
	 shotd); ������� �� ����������� ������ �����
       (progn
	 (setq h (/ 3 (getvar "CANNOSCALEVALUE")))
	 (setq long 0)
	 (princ "������� �� ������ � ������, �������� ���������� �����.")
	 )
       (progn
	 (setq h (- shotd (/ 1 (getvar "CANNOSCALEVALUE"))))
	 (if
	   (> h (/ 3 (getvar "CANNOSCALEVALUE"))); ������� �� ������������ ������ �����
	   (setq h (/ 3 (getvar "CANNOSCALEVALUE")))
	   ); if
	 ); progn
       ); if
     )
    ); cond
  (setq txy (calculation_coord_ins_text_from_centre t6 long ttt))
  (entmod (list (cons -1 tx1) (cons 10 txy) (cons 40 h) (cons 50 long)))
  (setq ttt (entget tx1))
  (setq txy (calculation_coord_ins_text_from_centre t6 long ttt))
  (entmod (list (cons -1 tx1) (cons 10 txy) (cons 40 h) (cons 50 long)))
  (entupd tx1)
  (princ)
  )