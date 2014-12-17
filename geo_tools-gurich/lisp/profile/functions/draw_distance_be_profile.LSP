(defun C:draw_distance_be_profile (/ t1 t2 t3 S ht hs styl list_points pt_array mleader_obj ang); подписывает расстояние
(geo_tools-help "draw_distance_be_profile")
  (initget "1 2")(setq Y_N (getkword "\nКакой набор? [1/2] <1>: "))
  (if (= Y_N nil)
    (setq Y_N "1")
    )
  (load_global_variable)
  (setq mv (file_dat_r_g_w "r" "mv" nil))
  (setq mg (file_dat_r_g_w "r" "mg" nil))
  (setq usl_z (file_dat_r_g_w "r" "usl_z" nil))
  (setq t1(get_p_usl_z_m_mg "\nУкажи точку #1 "))
  (initget)(setq t2(getpoint "\nУкажи точку #2: "))
  (while
    (not (and (eq Y_N nil)(eq t1 nil)(eq t2 nil)))
    (setq S (rtos(distance t1 t2) 2 2))
    (setq ht (/ 3.5 (/ 1000 mg)))
    (setq hs (/ 10 (/ 1000 mg)))
    (setq styl (getvar "TEXTSTYLE"))
    (if
      (>
	(+ (/ 1.6 (/ 1000 mg))(/ (* 1.6 (/ 2 3)) (/ 1000 mg)))
	(distance t1 t2)); условие
      (progn
	(setq t3 (mapcar '+ t1 (list (/(distance t1 t2)2) (/ hs 4) )))
	(setq list_points (list t3 (mapcar '+ t3 (list (/ 2 (/ 1000 mg)) (/ -7 (/ 1000 mg))))))
	(setq pt_array (convert_coords_list_from_safearray_mleader list_points))
	(setvar "CMLEADERSTYLE" "Подписи")
	(setq mleader_obj (vla-addmleader model_spece pt_array 0))
	(vla-put-StyleName mleader_obj "Подписи")
	(vla-put-textstring mleader_obj S)
	(vla-put-TextHeight mleader_obj (/ 2.5 (/ 1000 mg)))
	)
      (progn
	(if
	  (< (+(calculation_str_long_be_h_stile S ht styl)(/ (* 1.6(/ 2 3)) (/ 1000 mg))) (distance t1 t2))
	  (progn
	    (setq ht (/ 3.5 (/ 1000 mg)))
	    (setq ang 0)
	    )
	  (progn
	    (setq ang (/ pi 2))
	    (if
	      (< (+(/ 3.5 (/ 1000 mg))(/ (* 3.5 0.6) (/ 1000 mg)))(distance t1 t2))
	      (setq ht (/ 3.5 (/ 1000 mg)))
	      (setq ht (* (distance t1 t2) 0.6))
	      )
	    )
	  ); if
	(entmake (list '(0 . "TEXT")
		       (cons 10 t1)
		       '(72 . 4)
		       (cons 11 (mapcar '+ t1 (list (/(distance t1 t2)2) (/ hs 2) )))
		       (cons 40 ht)
		       (cons 50 ang)
		       (cons 1 S)
		       (cons 7 styl)
		       )
		 )
	); progn
      ); if
    (setq t1(get_p_usl_z_m_mg "\nУкажи точку #1 "))
    (initget)(setq t2(getpoint "\nУкажи точку #2: "))
    )
  (princ)
  )
