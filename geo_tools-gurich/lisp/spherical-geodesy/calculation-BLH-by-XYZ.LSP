(defun calculation-BLH-by-XYZ (point a b / flag Bcoor c D e2 Hcoor k Lcoor L_ N num-i p t0 ti ti-new ti-old X Y Z)
  ; вычисление геодезических координат BLH в десятичных градусах по прямоугольным XYZ в метрах
  ; пример вызова (calculation-BLH-by-XYZ (list 45.0 55.0) 6378245.0 6356863.0)

  (setq X (nth 0 point))
  (setq Y (nth 1 point))
  (setq Z (nth 2 point))
  (if (= Z nil) (setq Z 0.000))
  (setq L_ (atan (abs(/ Y X))))
  (cond
    ((and (not(minusp Y)) (not(minusp X))); 1 четверть
      (setq Lcoor L_)
      )
    ((and (not(minusp Y)) (minusp X)); 2 четверть
      (setq Lcoor (- pi L_))
      )
    ((and (minusp Y) (minusp X)); 3 четверть
      (setq Lcoor (+ pi L_))
      )
    ((and (minusp Y) (not(minusp X))); 4 четверть
      (setq Lcoor (- (* pi 2) L_))
      )
    )
  (setq D (sqrt (+ (expt X 2.0)(expt Y 2.0))))
  (setq e2 (/(- (expt a 2.0)(expt b 2.0))(expt a 2.0)))
  (setq c (/ a (sqrt (- 1.0 e2))))
  (if (< D Z)
    (progn; с '
      (setq t0 (/ D Z))
      (setq p (/ (*(* -1.0 a)e2) Z))
      (setq k (- 1.0 e2))
      )
    (progn
      (setq t0 (/ Z D))
      (setq p (/ (* c e2) D))
      (setq k (/ 1.0 (- 1.0 e2)))
      )
    )
  (setq num-i 1)
  (setq ti-old t0)
  (while (not flag)
    (setq ti-new (+ t0 (/ (* p ti-old)(sqrt (+ k (expt ti-old 2.0))))))
    (if (or (= ti-new ti-old) (> num-i 10))
      (setq flag T)
      (setq ti-old ti-new))
    (setq num-i (1+ num-i))
    )
  
  (setq Bcoor
	 (if (< D Z)
	   (- (/ pi 2) (atan ti-new))
	   (atan ti-new)
	   )
	)
  (setq N (/ (* (if (< D Z) a c) (sqrt (+ 1.0 (expt ti-new 2)))) (sqrt (+ k (expt ti-new 2)))))
  
  (setq Hcoor
	 (if (< D Z)
	   (- (* Z  (sqrt (+ 1.0 (expt ti-new 2.0)))) (* k N))
	   (* D (sqrt (+ 1.0 (expt ti-new 2.0))))
	   )
	)
  (list (convert-rad-grad Bcoor)
	(convert-rad-grad Lcoor)
	(convert-rad-grad Hcoor))
  )