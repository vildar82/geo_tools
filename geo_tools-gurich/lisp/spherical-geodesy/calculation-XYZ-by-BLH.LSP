(defun calculation-XYZ-by-BLH (point a b / Bcoor Lcoor Hcoor E2 N X Y Z)
  ; вычисление геодезических прямоугольных координат по криволинейным
  ; BL криволинейные координаты задаются в десятичных градусах H в метрах
  ; пример вызова (calculation-XYZ-by-BLH (list 45.0 55.0) 6378245.0 6356863.0)
  (setq Bcoor (nth 0 point))
  (setq Lcoor (nth 1 point))
  (setq Hcoor (nth 2 point))
  (if (= Hcoor nil) (setq Hcoor 0.0))
  (setq Bcoor (* Bcoor (/ pi 180))); перевод десятичных градусов в радианы
  (setq Lcoor (* Lcoor (/ pi 180))); для правильного вычисления sin и cos 
  (setq e2 (/(- (expt a 2)(expt b 2))(expt a 2)))
  (setq N(/ a (sqrt (- 1.0 (* e2 (expt (sin Bcoor) 2))))))
  (setq X (* (+ N Hcoor)(cos Bcoor)(cos Lcoor)))
  (setq Y (* (+ N Hcoor)(cos Bcoor)(sin Lcoor)))
  (setq Z (* (-(+ N Hcoor)(* N e2))(sin Bcoor)))
  (list X Y Z)
  )
;(calculation-BLH-by-XYZ (calculation-XYZ-by-BLH (list (+ 50.0 (/ 7.0 60.0)(/ 40.97 3600.0)) (+ 23.0 (/ 45.0 60.0)(/ 13.43 3600.0))) 6377397.155 6356078.963) 6377397.155 6356078.963)
;(calculation-XYZ-by-BLH (list (+ 50.0 (/ 7.0 60.0)(/ 40.97 3600.0)) (+ 23.0 (/ 45.0 60.0)(/ 13.43 3600.0))) 6377397.155 6356078.963);Эллипсоид Бесселя, 1841