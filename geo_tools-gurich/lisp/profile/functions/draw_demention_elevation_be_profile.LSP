(defun C:draw_demention_elevation_be_profile (/ t2 t3 ht hs styl list_points pt_array mleader_obj)
  ; ������ ������� � �������
(geo_tools-help "draw_demention_elevation_be_profile")
  (load_global_variable)
  (setq mv (file_dat_r_g_w "r" "mv" nil))
  (setq mg (file_dat_r_g_w "r" "mg" nil))
  (setq usl_z (file_dat_r_g_w "r" "usl_z" nil))
  (princ (strcat "\n�������� 1:" (rtos mv 2 0) " �������� 1:" (rtos mg 2 0) " ������� ��������� ���������: " (rtos usl_z 2 2) "�"))
  (setq t1! (get_pt1!))
  (setq t2(get_p_usl_z_m_mg "\n����� ���������� ����� �� ������� "))
  (while
    (not (and (eq mv nil)(eq usl_z nil)(eq t1! nil)(eq t2 nil)))
    (setq z (rtos (+(*(- (nth 1 t2) (nth 1 t1!))(/ (* mv (/ 1000 mg)) 1000))usl_z)2 2))
    (setq ht (/ 3.5 (/ 1000 mg)))
    (setq hs (/ 15 (/ 1000 mg)))
    (setq styl (getvar "TEXTSTYLE"))
    (initget)(setq t3(getpoint (strcat "\n������� = " z " ����� ���� ����� ��������� ����� <����������>: ")))
    (if
      (not (eq t3 nil))
      (if
	(< (nth 1 t3)(nth 1 t1!))
	(entmake (list '(0 . "TEXT") (cons 10 t2) '(72 . 1) '(73 . 1)(cons 11 (mapcar '+ t3 (list 0 (/ hs 2) ))) (cons 40 ht) (cons 50 (/ pi 2)) (cons 1 z) (cons 7 styl)))
	(progn
	  (if
	    (< (nth 1 t3)(nth 1 t2))
	    (if (< (distance (list 0(nth 1 t3))(list 0(nth 1 t2))) (/ 4 (/ 1000 mg)))
	      (setq list_points (list t2(mapcar '- t2 (list 0 (/ 6 (/ 1000 mg))0))))
	      (setq list_points (list t2(mapcar '- t2 (list 0 (distance (list 0(nth 1 t3))(list 0(nth 1 t2)))0))))
	      ); ����
	    (if (< (distance (list 0(nth 1 t3))(list 0(nth 1 t2))) (/ 4 (/ 1000 mg)))
	      (setq list_points (list t2(mapcar '+ t2 (list 0 (/ 4 (/ 1000 mg))0))))
	      (setq list_points (list t2(mapcar '+ t2 (list 0 (distance (list 0(nth 1 t3))(list 0(nth 1 t2)))0))))
	      ); �����
	    )
	  (setq pt_array (convert_coords_list_from_safearray_mleader list_points))
	  (setvar "CMLEADERSTYLE" "�������")
	  (setq mleader_obj (vla-addmleader model_spece pt_array 0))
	  (vla-put-StyleName mleader_obj "�������")
	  (vla-put-textstring mleader_obj z)
	  (vla-put-TextHeight mleader_obj ht)
	  (vla-put-ArrowheadType mleader_obj 9)
	  (vla-put-ArrowheadSize mleader_obj (/ 2 (/ 1000 mg)))
	  (vla-put-ArrowheadBlock mleader_obj "_Open90")
	  (if
	    (> (nth 0 t3)(nth 0 t2))
	    (progn
	      (vla-Mirror mleader_obj (vlax-3D-point t2)(vlax-3D-point (list (nth 0 t2) 1 0)))
	      (vla-Delete mleader_obj)
	      )
	    )
	  )
	)
      )
    (setq t2(get_p_usl_z_m_mg "\n����� ���������� ����� �� ������� "))
    )
  (princ)
  )
