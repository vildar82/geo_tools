(defun C:draw_text_be_center_building ( / t1 t2 t3 t4 t5 c1 d longa long str tx1 ttt txy t6 h)
  ; подписывает и центрирует подпись в центре здани€
(geo_tools-help "draw_text_be_center_building")
  (terpri)
  (format_check_text_style)
  (setq d(/ 1 (getvar "CANNOSCALEVALUE")))
  (initget)(setq t1(getpoint "”кажи первую точку:"))(terpri); указываю точку
  (initget)(setq t2(getpoint t1 "”кажи вторую точку:"))(terpri); указываю вторую точку
  (initget)(setq t3(getpoint t2 "”кажи третью точку:"))(terpri); указываю третью точку
  (initget)(setq str (getstring nil "¬веди характеристику здани€: "))
  (setq с1(+(angle t2 t1) (* 90 (/ pi 180)))); вычисл€ю угол на t4
  (setq d(*(distance t2 t3) (cos (-(* 180 (/ pi 180)) (- (- (angle t2 t1)(angle t2 t3)) (* 90 (/ pi 180))))))); вычисл€ю рассто€ние на t4
  (setq t4(polar t2 с1 d)); вычисл€ю координаты на t4
  (setq t5(polar t4 (angle t2 t1) (distance t1 t2))); вычисл€ю координаты на t5
  (setq t6
	 (polar
	   (polar t1 (angle t1 t2) (/ (distance t1 t2) 2))
	   (angle t1 t5)
	   (/ (distance t1 t5) 2)
	   ); polar
	); setq t6
  (if (> (distance t1 t2)
	 (distance t1 t5)
	 ); конец услови€
    (progn
      (setq longa (angle t1 t2))
      (setq shotd (distance t1 t5))
      ); progn
    (progn
      (setq longa (angle t1 t5))
      (setq shotd (distance t1 t2))
      ); progn
    ); if
  (setq long (analysis_angle_be_long_distance longa))
  (if
    (>
      (+ (/ 1 (getvar "CANNOSCALEVALUE"))(/ (/ 1.6 (getvar "CANNOSCALEVALUE")) 1.07372))
      shotd); условие
    (progn
      (setq h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.07372))
      (setq long 0)
      (princ "ѕодпись не входит в контур, попробуй разместить р€дом.")
      )
    (progn
      (setq h (- shotd (/ 1 (getvar "CANNOSCALEVALUE"))))
      (if
	(> h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.07372))
	(setq h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.07372))
	); if
      ); progn
    ); if
  (setq ttt (list '(0 . "TEXT") (cons 10 t1) (cons 40 h) (cons 1 str) (cons 50 long) '(7 . "OTI")))
  (setq txy (calculation_coord_ins_text_from_centre t6 long ttt))
  (entmake (list '(0 . "TEXT") (cons 10 txy) (cons 40 h) (cons 1 str) (cons 50 long) '(7 . "OTI")))
  (princ)
  )
