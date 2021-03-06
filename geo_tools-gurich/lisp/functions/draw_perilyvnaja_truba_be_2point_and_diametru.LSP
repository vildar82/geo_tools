(defun C:draw_perilyvnaja_truba_be_2point_and_diametru (/ d t1 t2 t11 t12 t21 t22 t111 t121 t211 t221); ���������� ����� �� ������� ������ � ��������
(geo_tools-help "draw_perilyvnaja_truba_be_2point_and_diametru")
  (setq d(/ 1 (getvar "CANNOSCALEVALUE")))
  (initget)(setq t1(getpoint "\n����� ������ �����:")); �������� �����
  (initget)(setq t2(getpoint t1 "\n����� ������ �����:")); �������� ������ �����
  (initget 6)(setq d(getreal(strcat "\n����� ������� ����� <" (rtos d 2 1) ">:"))); �������� �������
  (if (or (= d nil)(< d (/ 1 (getvar "CANNOSCALEVALUE")))) (setq d(/ 1 (getvar "CANNOSCALEVALUE"))))
  (setq t11(polar t1 (-(angle t1 t2) (/ pi 2)) (/ d 2)))
  (setq t12(polar t1 (+(angle t1 t2) (/ pi 2)) (/ d 2)))
  (setq t21(polar t2 (+(angle t2 t1) (/ pi 2)) (/ d 2)))
  (setq t22(polar t2 (-(angle t2 t1) (/ pi 2)) (/ d 2)))
  (setq t111(polar t11 (-(angle t11 t21) (* 135 (/ pi 180))) (/ 1 (getvar "CANNOSCALEVALUE"))))
  (setq t121(polar t12 (+(angle t12 t22) (* 135 (/ pi 180))) (/ 1 (getvar "CANNOSCALEVALUE"))))
  (setq t211(polar t21 (+(angle t21 t11) (* 135 (/ pi 180))) (/ 1 (getvar "CANNOSCALEVALUE"))))
  (setq t221(polar t22 (-(angle t22 t12) (* 135 (/ pi 180))) (/ 1 (getvar "CANNOSCALEVALUE"))))
  (entmake (list '(0 . "LINE") '(6 . "�����_1.5-1.5") (cons 10 t11) (cons 11 t21)))
  (entmake (list '(0 . "LINE") '(6 . "�����_1.5-1.5") (cons 10 t12) (cons 11 t22)))
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
		 '(90 . 4) (cons 43 (* 0.3 (getvar "LTSCALE")))
		 (cons 10 t111) (cons 10 t11) (cons 10 t12) (cons 10 t121)))
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
		 '(90 . 4) (cons 43 (/ 0.3 (getvar "CANNOSCALEVALUE")))
		 (cons 10 t211) (cons 10 t21) (cons 10 t22) (cons 10 t221)))
  (princ)
  )
