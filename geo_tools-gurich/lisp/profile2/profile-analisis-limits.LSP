(defun profile-analisis-limits (2dpline mg /
				dgranicy flag g1 g2 granica granica1 granica2 temp
				kontrol_list list_granic new_list_granic p1 p11 p12 p2 p21 p22 spisokv)
  (if (eq(type 2dpline)'LIST)
    (setq spisokv 2dpline)
    (setq spisokv (extract_coord_lwpolyline (entget 2Dpline)))
    )
  (setq flag T)
  (while (eq flag T)
    ; модуль создания границ занимаемых отметками
    (if (eq list_granic nil)
      (progn
	(setq temp spisokv)
	(repeat (1-(length temp))
	  (setq p1 (car temp))
	  (setq temp (cdr temp))
	  (setq p2 (car temp))
	  (if (<(- (nth 0 p2) (nth 0 p1))(/ (* 2 (/ 4.0 3.0)) (/ 1000.0 mg)))
	    (setq list_granic (append list_granic (list (list (nth 0 p1)(nth 0 p2)))))
	    )
	  )
	)
      )
    ; модуль расширения границ занимаемых отметками
    (repeat (length list_granic)
      (setq granica (car list_granic))
      (setq list_granic (cdr list_granic))
      (setq p1 (car granica))
      (setq p2 (cadr granica))
      (setq Dgranicy (*(analisis-kol-vo-elevation-in-limit p1 p2 spisokv mg)(/ (* 2 (/ 4.0 3.0)) (/ 1000.0 mg))))
      (setq g1 (-(analysis-centr-limit-be-elevation p1 p2 spisokv mg)(/ Dgranicy 2)))
      (setq g2 (+(analysis-centr-limit-be-elevation p1 p2 spisokv mg)(/ Dgranicy 2)))
      (setq new_list_granic (append new_list_granic(list (list g1 g2))))
      )
    (setq list_granic new_list_granic
	  new_list_granic nil)

    
    ; модуль объединения границ занимаемых отметками
    (if (=(length list_granic)1)
      (setq new_list_granic list_granic)
      (while (not(<(length list_granic)2))
	(setq granica1 (car list_granic))
	(setq list_granic (cdr list_granic))
	(setq granica2 (car list_granic))
	(setq p11 (car granica1))
	(setq p12 (cadr granica1))
	(setq p21 (car granica2))
	(setq p22 (cadr granica2))
	(while (and(or(> p12 p21)(= p12 p21))(not(and (eq p21 nil)(eq p22 nil))))
	  (if (or(> p12 p22)(= p12 p22))
	    (if (/=(length list_granic)0)
	      (setq list_granic (cdr list_granic))
	      )
	    (progn
	      (setq p12 p22)
	      (if (/=(length list_granic)0)
		(setq list_granic (cdr list_granic))
		)
	      )
	    )
	  (if (/=(length list_granic)0)
	    (progn
	      (setq granica2 (car list_granic))
	      (setq p21 (car granica2))
	      (setq p22 (cadr granica2))
	      )
	    (progn
	      (setq p21 nil)
	      (setq p22 nil)
	      )
	    )
	  );while
	(setq new_list_granic (append new_list_granic(list (list p11 p12))))
	(if (=(length list_granic)1); обработка последней одинокой границы
	  (progn
	    (setq granica2 (car list_granic))
	    (setq new_list_granic (append new_list_granic(list granica2)))
	    )
	  )
	)
      )
    
    (setq list_granic new_list_granic
	  new_list_granic nil
	  temp list_granic)
    
    
    ; контроль на повтор цикла
    (repeat (length temp)
      (setq granica (car temp))
      (setq temp (cdr temp))
      (setq p1 (car granica))
      (setq p2 (cadr granica))
      (setq Dgranicy (*(analisis-kol-vo-elevation-in-limit p1 p2 spisokv mg)(/ (* 2 (/ 4.0 3.0)) (/ 1000.0 mg))))
      (if
	(and
	  (equal (- p2 p1)Dgranicy 0.0001)
	  (equal(+ p1(/(- p2 p1)2))(analysis-centr-limit-be-elevation p1 p2 spisokv mg)0.0001)
	  )
	(setq kontrol_list (append kontrol_list(list T)))
	(setq kontrol_list (append kontrol_list(list nil)))
	)
      )
    (if (eq (vl-member-if 'null kontrol_list)nil)
      (setq flag nil)
      (setq flag T)
      )
    (setq kontrol_list nil)
    )
  list_granic
  )

(defun analisis-kol-vo-elevation-in-limit ( g1 g2 spisokv mg / pline spisokv n p)
  (setq n 0)
  (repeat (length spisokv)
    (setq p (car spisokv))
    (setq spisokv (cdr spisokv))
    (if (and(or(>(nth 0 p)g1)(=(nth 0 p)g1)) (<(nth 0 p)(+ g2(/ (* 1.6 (/ 5.0 3.0)) (/ 1000.0 mg)))))
      (setq n (1+ n))
      )
    )
  n
  );defun

(defun analysis-centr-limit-be-elevation ( g1 g2 spisokv mg / pline spisokv point_list p)
  
  (repeat (length spisokv)
    (setq p (car spisokv))
    (setq spisokv (cdr spisokv))
    (if (and(or(>(nth 0 p)g1)(=(nth 0 p)g1)) (<(nth 0 p)(+ g2(/ (* 1.6 (/ 5.0 3.0)) (/ 1000.0 mg)))))
      (setq point_list (append point_list(list p)))
      )
    )
  (+(/(-(nth 0 (last point_list))(nth 0 (car point_list)))2)(nth 0 (car point_list)))
  );defun
