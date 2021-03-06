(defun C:draw_depth_be_profile_text (/ text1 text2 t1 H1 H2 h ht hs styl); ��������� ������� �� ������� ���� �������
(geo_tools-help "draw_depth_be_profile_text")
  (setq mg (file_dat_r_g_w "r" "mg" nil))
  (initget)(setq text1 (car (entsel "\n������ ������ ����� � ��������: ")))
  (while
    (not (eq text1 nil))
    (initget)(setq text2 (car (entsel "\n������ ������ ����� � ��������: ")))
    (setq H1 (atof (cdr (assoc 1 (entget text1)))))
    (setq H2 (atof (cdr (assoc 1 (entget text2)))))
    (setq h (rtos(abs(- H1 H2))2 2))
    (setq styl (getvar "TEXTSTYLE"))
    (setq t1(getpoint "\n����� ���� �������� ������� "))
    
    (setq ht (/ 3.5 (/ 1000 mg)))
    (setq hs (/ 10 (/ 1000 mg)))
    (entmake (list '(0 . "TEXT")
		   (cons 10 t1)
		   '(72 . 1)
		   '(73 . 1)
		   (cons 11 (mapcar '+ t1 (list 0 (/ hs 2) )))
		   (cons 40 ht)
		   (cons 50 (/ pi 2))
		   (cons 1 h)
		   (cons 7 styl)))
    (initget)(setq text1 (car (entsel "\n������ ������ ����� � ��������: ")))
    )
  )
