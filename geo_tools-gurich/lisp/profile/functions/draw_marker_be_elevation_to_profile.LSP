(defun C:draw_marker_be_elevation_to_profile (/ t2 t3 t4 z text mv mg usl_z); ������ ������ �� ������ ������ �� �������
(geo_tools-help "draw_marker_be_elevation_to_profile")
  (setq mv (file_dat_r_g_w "r" "mv" nil))
  (setq mg (file_dat_r_g_w "r" "mg" nil))
  (setq usl_z (file_dat_r_g_w "r" "usl_z" nil))
  (defun get_z (/ z text)
    (while (not (eq 'REAL (type z)))
      (princ (strcat "\n�������� 1:" (rtos mv 2 0) " �������� 1:" (rtos mg 2 0) " ������� ��������� ���������: " (rtos usl_z 2 2) "�"))
      (initget "�� �� �������� ���")(setq z (getreal "\n����� ������� [��/��/�������� ��������/���] <������ �����>: "))
      (cond
	((= z "��")
	 (setq mv (file_dat_r_g_w "g" "mv" nil)))
	((= z "��")
	 (setq mg (file_dat_r_g_w "g" "mg" nil)))
	((= z "��������")
	 (setq usl_z (file_dat_r_g_w "g" "usl_z" nil)))
	((= z "���")
	 (setq mv (file_dat_r_g_w "g" "mv" nil))
	 (setq mg (file_dat_r_g_w "g" "mg" nil))
	 (setq usl_z (file_dat_r_g_w "g" "usl_z" nil)))
	((eq z nil)
	 (initget)(setq text (car (entsel "\n������ ����� � ��������:")))
	 (if (/= text nil)
	   (setq z(atof(edit_text_clean_for_number(cdr(assoc 1 (entget text))))))
	   ))))
    z)
  (setq z (get_z))
  (while
    (not (eq z nil))
    (setq t1! (get_pt1!))
    (setq t2 (mapcar '+ t1! (list 0 (/ (- z usl_z) (/ (* mv (/ 1000 mg)) 1000)))))
    (setq t3 (mapcar '+ t2 (list -1.25 -2.165)))
    (setq t4 (mapcar '+ t2 (list 1.25 -2.165)))
    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
		   '(90 . 3) '(70 . 1) (cons 43 0)
		   (cons 10 t3) (cons 10 t2) (cons 10 t4)))
    (setq z nil)
    (setq z (get_z))
    )
  (princ)
  )
