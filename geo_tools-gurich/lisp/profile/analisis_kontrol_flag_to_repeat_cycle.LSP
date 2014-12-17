(defun analisis_kontrol_flag_to_repeat_cycle (list_granic 2Dpline / p1 p2 kontrol_list Dgranicy)
  (repeat (length list_granic) ; контроль на повтор цикла
    (setq granica (car list_granic))
    (setq list_granic (cdr list_granic))
    (setq p1 (car granica))
    (setq p2 (cadr granica))
    (setq Dgranicy (*(analisis_kol-vo_elevation_in_limit p1 p2 2Dpline)(/ (* 2 (/ 4.0 3.0)) (/ 1000 mg))))
    (if
      (and
	(equal (- p2 p1)Dgranicy 0.0001)
	(equal(+ p1(/(- p2 p1)2))(analysis_centr_limit_be_elevation p1 p2 2Dpline)0.0001)
	)
      (setq kontrol_list (append kontrol_list(list T)))
      (setq kontrol_list (append kontrol_list(list nil)))
      )
    )
  (if (eq (vl-member-if 'null kontrol_list)nil)
    nil
    T
    )
  );defun
