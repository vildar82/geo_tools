(defun C:draw_bridge_be_3point ( / t1 t2 t3 t4 t5 c1 d t111 t121 t211 t221); ������ �������� ���� �����
(geo_tools-help "draw_bridge_be_3point")
  (terpri)
  (initget)(setq t1(getpoint "����� ������ �����:"))(terpri)
  (initget)(setq t2(getpoint t1 "����� ������ �����:"))(terpri)
  (entmake (list '(0 . "LINE") '(8 . "��������") '(6 . "Continuous") (append '(10) t1) (append '(11) t2)))
  (initget)(setq t3(getpoint t2 "����� ������ �����:"))(terpri)
  (setq c1 (+(angle t2 t1) (/ pi 2))); �������� ���� �� t4
  (setq d(*(distance t2 t3) (cos (- pi (- (- (angle t2 t1)(angle t2 t3)) (/ pi 2)))))); �������� ���������� �� t4
  (setq t4(polar t2 c1 d)); �������� ���������� �� t4
  (setq t5(polar t4 (angle t2 t1) (distance t1 t2))); �������� ���������� �� t5
  (if
    (> d 0)
    (progn
      (setq t111(polar t1 (+(angle t1 t2) (* 135 (/ pi 180))) (/ 1 (getvar "CANNOSCALEVALUE"))));
      (setq t121(polar t2 (-(angle t2 t1) (* 135 (/ pi 180))) (/ 1 (getvar "CANNOSCALEVALUE")))); ��������
      (setq t211(polar t4 (+(angle t4 t5) (* 135 (/ pi 180))) (/ 1 (getvar "CANNOSCALEVALUE"))));  "���"
      (setq t221(polar t5 (-(angle t5 t4) (* 135 (/ pi 180))) (/ 1 (getvar "CANNOSCALEVALUE"))));
      )
    (progn
      (setq t111(polar t1 (-(angle t1 t2) (* 135 (/ pi 180))) (* 1 (getvar "CANNOSCALEVALUE"))));
      (setq t121(polar t2 (+(angle t2 t1) (* 135 (/ pi 180))) (* 1 (getvar "CANNOSCALEVALUE")))); ��������
      (setq t211(polar t4 (-(angle t4 t5) (* 135 (/ pi 180))) (* 1 (getvar "CANNOSCALEVALUE"))));  "���"
      (setq t221(polar t5 (+(angle t5 t4) (* 135 (/ pi 180))) (* 1 (getvar "CANNOSCALEVALUE"))));
      )
    )
  (entmake (list '(0 . "LINE") '(8 . "��������") '(6 . "Continuous") (append '(10) t4) (append '(11) t5)))
  (entmake (list '(0 . "LINE") '(8 . "��������") '(6 . "Continuous") (append '(10) t1) (append '(11) t111)))
  (entmake (list '(0 . "LINE") '(8 . "��������") '(6 . "Continuous") (append '(10) t2) (append '(11) t121)))
  (entmake (list '(0 . "LINE") '(8 . "��������") '(6 . "Continuous") (append '(10) t5) (append '(11) t221)))
  (entmake (list '(0 . "LINE") '(8 . "��������") '(6 . "Continuous") (append '(10) t4) (append '(11) t211)))
  (princ)
  )
