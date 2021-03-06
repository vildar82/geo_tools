(defun draw_eledation_to_profile (2Dpline list_granic t1! t1_ins_z t2_ins_z ht /
				  styl os_linii_podpisey spisokv p1 p2 g1 g2 n Dgranicy t1 htz z granica)
  (setq styl (getvar "TEXTSTYLE"))
  (setq os_linii_podpisey  (+(/(- (nth 1 t2_ins_z) (nth 1 t1_ins_z))2)(nth 1 t1_ins_z)))
  (setq spisokv (extract_coord_lwpolyline (entget 2Dpline)))
  (setq p1 (car spisokv))
  (setq spisokv (cdr spisokv))
  (setq granica (car list_granic))
  (setq g1 (car granica))
  (setq g2 (cadr granica))
  (if (not(and(>(nth 0 p1)g1)(<(nth 0 p1)g2))); ��������� ������ �����/������� � ������
    
    (progn
      (setq z (+(*(- (nth 1 p1) (nth 1 t1!))(/ (* mv (/ 1000 mg)) 1000))usl_z))
      (entmake (list '(0 . "TEXT")
		     (cons 10 p1)
		     '(72 . 1) '(73 . 1)
		     (cons 11 (list(nth 0 p1)(+(nth 1 t1_ins_z)(/(- (nth 1 t2_ins_z)(nth 1 t1_ins_z))2))))
		     (cons 40 ht)
		     (cons 50 (/ pi 2))
		     (cons 1 (rtos z 2 2))
		     (cons 7 styl)))
      )
    (progn
      (setq n (analisis_kol-vo_elevation_in_limit g1 g2 2Dpline))
      (setq Dgranicy (* n(/ (* 2 (/ 4.0 3.0)) (/ 1000 mg))))
      (setq t1 (mapcar '+ (list g1 os_linii_podpisey) (list(/(/ (* 2 (/ 4.0 3.0)) (/ 1000 mg))2) 0)))
      (repeat (1- n)
	(setq z (+(*(- (nth 1 p1) (nth 1 t1!))(/ (* mv (/ 1000 mg)) 1000))usl_z))
	(entmake (list '(0 . "TEXT")
		       (cons 10 t1)
		       '(72 . 4) '(73 . 0)
		       (cons 11 t1)
		       (cons 40 (/ 2 (/ 1000 mg)))
		       (cons 50 (/ pi 2))
		       (cons 1 (rtos z 2 2))
		       (cons 7 styl)))
	(entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
		       '(90 . 4)
		       (cons 10 (list (nth 0 p1) (nth 1 t2_ins_z)))
		       (cons 10 (list (nth 0 p1) (-(nth 1 t2_ins_z)(/ 1 (/ 1000 mg)))))
		       (cons 10 (list (nth 0 t1) (-(nth 1 t2_ins_z)(/ 2.4 (/ 1000 mg)))))
		       (cons 10 (list (nth 0 t1) (-(nth 1 t2_ins_z)(/ 3.4 (/ 1000 mg)))))))
	(entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
		       '(90 . 4)
		       (cons 10 (list (nth 0 t1) (+(nth 1 t1_ins_z)(/ 3.4 (/ 1000 mg)))))
		       (cons 10 (list (nth 0 t1) (+(nth 1 t1_ins_z)(/ 2.4 (/ 1000 mg)))))
		       (cons 10 (list (nth 0 p1) (+(nth 1 t1_ins_z)(/ 1 (/ 1000 mg)))))
		       (cons 10 (list (nth 0 p1) (nth 1 t1_ins_z)))))
	(edit_cleaning_line_profil p1 t1_ins_z t2_ins_z)
	(setq t1 (mapcar '+ t1 (list(/ (* 2 (/ 4.0 3.0)) (/ 1000 mg)) 0)))
	(setq p1 (car spisokv))
	(setq spisokv (cdr spisokv))
	)
      (setq z (+(*(- (nth 1 p1) (nth 1 t1!))(/ (* mv (/ 1000 mg)) 1000))usl_z))
      (entmake (list '(0 . "TEXT")
		     (cons 10 t1)
		     '(72 . 4) '(73 . 0)
		     (cons 11 t1)
		     (cons 40 (/ 2 (/ 1000 mg)))
		     (cons 50 (/ pi 2))
		     (cons 1 (rtos z 2 2))
		     (cons 7 styl)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
		     '(90 . 4)
		     (cons 10 (list (nth 0 p1) (nth 1 t2_ins_z)))
		     (cons 10 (list (nth 0 p1) (-(nth 1 t2_ins_z)(/ 1 (/ 1000 mg)))))
		     (cons 10 (list (nth 0 t1) (-(nth 1 t2_ins_z)(/ 2.4 (/ 1000 mg)))))
		     (cons 10 (list (nth 0 t1) (-(nth 1 t2_ins_z)(/ 3.4 (/ 1000 mg)))))))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
		     '(90 . 4)
		     (cons 10 (list (nth 0 t1) (+(nth 1 t1_ins_z)(/ 3.4 (/ 1000 mg)))))
		     (cons 10 (list (nth 0 t1) (+(nth 1 t1_ins_z)(/ 2.4 (/ 1000 mg)))))
		     (cons 10 (list (nth 0 p1) (+(nth 1 t1_ins_z)(/ 1 (/ 1000 mg)))))
		     (cons 10 (list (nth 0 p1) (nth 1 t1_ins_z)))))
      (edit_cleaning_line_profil p1 t1_ins_z t2_ins_z)
      (setq p1 (list g2 os_linii_podpisey))
      (setq list_granic (cdr list_granic))
      )
    )
  (while (/=(length spisokv)0); ��������� ��������� �����
    (setq p2 (car spisokv))
    (setq spisokv (cdr spisokv))
    (setq granica (car list_granic))
    (setq g1 (car granica))
    (setq g2 (cadr granica))
    (if
      (not(and(>(nth 0 p2)g1)(<(nth 0 p2)g2)))
      (progn
	(cond
	  ((> (*(-(nth 0 p2)(nth 0 p1))0.6) ht)
	   (setq htz ht))
	  ((< (*(-(nth 0 p2)(nth 0 p1))0.6) (/ 1.6 (/ 1000 mg)))
	   (setq htz (/ 1.6 (/ 1000 mg))))
	  (T
	   (setq htz (*(-(nth 0 p2)(nth 0 p1))0.6)))
	  )
	(setq z (+(*(- (nth 1 p2) (nth 1 t1!))(/ (* mv (/ 1000 mg)) 1000))usl_z))
	(entmake (list '(0 . "TEXT")
		       (cons 10 p2)
		       '(72 . 1) '(73 . 1)
		       (cons 11 (list(nth 0 p2)os_linii_podpisey))
		       (cons 40 htz)
		       (cons 50 (/ pi 2))
		       (cons 1 (rtos z 2 2))
		       (cons 7 styl)))
	(setq p1 p2)
	
	)
      (progn
	(setq n (analisis_kol-vo_elevation_in_limit g1 g2 2Dpline))
	(setq Dgranicy (* n(/ (* 2 (/ 4.0 3.0)) (/ 1000 mg))))
	(setq t1 (mapcar '+ (list g1 os_linii_podpisey) (list(/(/ (* 2 (/ 4.0 3.0)) (/ 1000 mg))2) 0)))
	(repeat (1- n)
	  (setq z (+(*(- (nth 1 p2) (nth 1 t1!))(/ (* mv (/ 1000 mg)) 1000))usl_z))
	  (entmake (list '(0 . "TEXT")
			 (cons 10 t1)
			 '(72 . 4) '(73 . 0)
			 (cons 11 t1)
			 (cons 40 (/ 2 (/ 1000 mg)))
			 (cons 50 (/ pi 2))
			 (cons 1 (rtos z 2 2))
			 (cons 7 styl)))
	  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
			 '(90 . 4)
			 (cons 10 (list (nth 0 p2) (nth 1 t2_ins_z)))
			 (cons 10 (list (nth 0 p2) (-(nth 1 t2_ins_z)(/ 1 (/ 1000 mg)))))
			 (cons 10 (list (nth 0 t1) (-(nth 1 t2_ins_z)(/ 2.4 (/ 1000 mg)))))
			 (cons 10 (list (nth 0 t1) (-(nth 1 t2_ins_z)(/ 3.4 (/ 1000 mg)))))))
	  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
			 '(90 . 4)
			 (cons 10 (list (nth 0 t1) (+(nth 1 t1_ins_z)(/ 3.4 (/ 1000 mg)))))
			 (cons 10 (list (nth 0 t1) (+(nth 1 t1_ins_z)(/ 2.4 (/ 1000 mg)))))
			 (cons 10 (list (nth 0 p2) (+(nth 1 t1_ins_z)(/ 1 (/ 1000 mg)))))
			 (cons 10 (list (nth 0 p2) (nth 1 t1_ins_z)))))
	  (edit_cleaning_line_profil p2 t1_ins_z t2_ins_z)
	  (setq p2 (car spisokv))
	  (setq spisokv (cdr spisokv))
	  (setq t1 (mapcar '+ t1 (list(/ (* 2 (/ 4.0 3.0)) (/ 1000 mg)) 0)))
	  )
	(setq z (+(*(- (nth 1 p2) (nth 1 t1!))(/ (* mv (/ 1000 mg)) 1000))usl_z))
	(entmake (list '(0 . "TEXT")
		       (cons 10 t1)
		       '(72 . 4) '(73 . 0)
		       (cons 11 t1)
		       (cons 40 (/ 2 (/ 1000 mg)))
		       (cons 50 (/ pi 2))
		       (cons 1 (rtos z 2 2))
		       (cons 7 styl)))
	(entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
		       '(90 . 4)
		       (cons 10 (list (nth 0 p2) (nth 1 t2_ins_z)))
		       (cons 10 (list (nth 0 p2) (-(nth 1 t2_ins_z)(/ 1 (/ 1000 mg)))))
		       (cons 10 (list (nth 0 t1) (-(nth 1 t2_ins_z)(/ 2.4 (/ 1000 mg)))))
		       (cons 10 (list (nth 0 t1) (-(nth 1 t2_ins_z)(/ 3.4 (/ 1000 mg)))))))
	(entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
		       '(90 . 4)
		       (cons 10 (list (nth 0 t1) (+(nth 1 t1_ins_z)(/ 3.4 (/ 1000 mg)))))
		       (cons 10 (list (nth 0 t1) (+(nth 1 t1_ins_z)(/ 2.4 (/ 1000 mg)))))
		       (cons 10 (list (nth 0 p2) (+(nth 1 t1_ins_z)(/ 1 (/ 1000 mg)))))
		       (cons 10 (list (nth 0 p2) (nth 1 t1_ins_z)))))
	(edit_cleaning_line_profil p2 t1_ins_z t2_ins_z)
	(setq p1 (list g2 os_linii_podpisey))
	(setq list_granic (cdr list_granic))
	)
      )
    )
  )
