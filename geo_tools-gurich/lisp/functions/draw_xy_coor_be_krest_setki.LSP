(defun C:draw_xy_coor_be_krest_setki ( / t1 yt xt x1 y1 x2 y2 y x option temp flag); ����������� ���������� ���������� ��������
  (geo_tools-help "draw_xy_coor_be_krest_setki")
  (terpri)
  
  (setq option(file-dat-xml-read "draw_xy_coor_be_krest_setki" "���������"))
  
    
  (while (not flag)
    (initget"���������")(setq t1(getpoint "����� �����[���������]>: ")); �������� �����
    (cond
      ((= t1 "���������")
       (initget"��������� ������")(setq temp(getkword (strcat "������� �����������[���������/������]<"option">: ")))
       (if temp (setq option temp))
       (file-dat-xml-write "draw_xy_coor_be_krest_setki" option)
       )
      ((eq (type t1) 'LIST)
       (setq flag T)
       )
      (T
       (exit)
       )
      )
    )
  (princ option)
  (setq m-k (getvar "CANNOSCALEVALUE"))
  (setq E-text (strcat "Y=" (rtos (nth 0 t1) 2 0))); ��������� ����� Y
  (setq N-text (strcat "X=" (rtos (nth 1 t1) 2 0))); ��������� ����� x
  (if (= option "���������")
    (progn
      (setq x (mapcar '+ t1 (list (/ 0.5 m-k) (/ 0.5 m-k)))); �������� ���������� ������ ��� X=
      (setq y (mapcar '+ t1 (list (/ 0.5 m-k) (* -0.5 m-k)))); �������� ���������� ������ ��� Y=
      (setq aligment 0)
      )
    (progn
      (setq x (mapcar '+ t1 (list (/ 3.0 m-k) 0.0))); �������� ���������� ������ ��� X=
      (setq y (mapcar '+ t1 (list 0.0 (* -3.0 m-k)))); �������� ���������� ������ ��� Y=
      (setq aligment 9)
      )
    )
  (draw-text-vla-by-list-property
    (list
      (cons "spece" model_spece)
      (cons "TextString" E-text)
      (cons "InsertionPoint" y)
      (cons "Height" (/ (/ 2 m-k) 1.07372))
      (cons "Rotation" (* pi 1.5))
      (cons "StyleName" "OTI")
      (cons "Alignment" aligment)
      ))
  (draw-text-vla-by-list-property
    (list
      (cons "spece" model_spece)
      (cons "TextString" N-text)
      (cons "InsertionPoint" x)
      (cons "Height" (/ (/ 2 m-k) 1.07372))
      (cons "Rotation" 0.0)
      (cons "StyleName" "OTI")
      (cons "Alignment" aligment)
      ))
  (princ)
  )
