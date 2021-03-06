(defun C:draw_rectangle_be_3_points ( / t1 t2 t3 t4 t5 c1 d)
  ; ������������� �� ���� ������
  (geo_tools-help "draw_rectangle_be_3_points")
  (setvar "CMDECHO" 0)
  (initget)(setq t1(getpoint "\n����� ������ ����� ������� �������<�����>:"))
  (while t1
    (initget 1)(setq t2(getpoint t1 "\n����� ������ ����� ������� �������:"))
    (command "_.UNDO" "_begin")
    (entmake (list '(0 . "LINE") (cons 10 t1) (cons 11 t2)))
    (initget 1)(setq t3(getpoint t2 "\n����� ������ ����� (������ ��������������):"))
    (setq c1(+(angle t2 t1) (/ pi 2))); �������� ���� �� t4
    (setq d(*(distance t2 t3) (cos (- pi (- (- (angle t2 t1)(angle t2 t3)) (/ pi 2)))))); �������� ���������� �� t4
    (setq t4(polar t2 c1 d)); �������� ���������� �� t4
    (setq t5(polar t4 (angle t2 t1) (distance t1 t2))); �������� ���������� �� t5
    (entmake (list '(0 . "LINE") (cons 10 t2) (cons 11 t4)))
    (entmake (list '(0 . "LINE") (cons 10 t4) (cons 11 t5)))
    (entmake (list '(0 . "LINE") (cons 10 t5) (cons 11 t1)))
    (command "_.UNDO" "_end")
    (initget)(setq t1(getpoint "\n����� ������ ����� ������� �������<�����>:"))
    )
  (setvar "CMDECHO" 1)
  (princ)
  )
