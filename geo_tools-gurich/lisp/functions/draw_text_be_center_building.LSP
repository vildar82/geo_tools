(defun C:draw_text_be_center_building ( / t1 t2 t3 t4 t5 c1 d longa long str tx1 ttt txy t6 h)
  ; ����������� � ���������� ������� � ������ ������
(geo_tools-help "draw_text_be_center_building")
  (terpri)
  (format_check_text_style)
  (setq d(/ 1 (getvar "CANNOSCALEVALUE")))
  (initget)(setq t1(getpoint "����� ������ �����:"))(terpri); �������� �����
  (initget)(setq t2(getpoint t1 "����� ������ �����:"))(terpri); �������� ������ �����
  (initget)(setq t3(getpoint t2 "����� ������ �����:"))(terpri); �������� ������ �����
  (initget)(setq str (getstring nil "����� �������������� ������: "))
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
  (if
    (>
      (+ (/ 1 (getvar "CANNOSCALEVALUE"))(/ (/ 1.6 (getvar "CANNOSCALEVALUE")) 1.07372))
      shotd); �������
    (progn
      (setq h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.07372))
      (setq long 0)
      (princ "������� �� ������ � ������, �������� ���������� �����.")
      )
    (progn
      (setq h (- shotd (/ 1 (getvar "CANNOSCALEVALUE"))))
      (if
	(> h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.07372))
	(setq h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.07372))
	); if
      ); progn
    ); if
  (setq ttt (list '(0 . "TEXT") (cons 10 t1) (cons 40 h) (cons 1 str) (cons 50 long) '(7 . "OTI")))
  (setq txy (calculation_coord_ins_text_from_centre t6 long ttt))
  (entmake (list '(0 . "TEXT") (cons 10 txy) (cons 40 h) (cons 1 str) (cons 50 long) '(7 . "OTI")))
  (princ)
  )
