(defun C:draw_ramka_planshet_m500_coor_system_OMS ( / t1 t2 t3 t4 yt1 yt2 yt3 yt4 x1 x2 x3 x4 y1 y2 y3 y4 o tn te ts tw nam namn name nams namw xy xyn xye xys xyw nx ny n1)
(geo_tools-help "draw_ramka_planshet_m500_coor_system_OMS")
  (terpri)
  (initget)(setq t1(getpoint "”кажи левый нижний угол планшета: ")); указываю точку отсчета
  (if (and (not (minusp (nth 0 t1)))
	   (not (minusp (nth 1 t1)))
	   (or (= (strlen (rtos (nth 0 t1) 2 0)) 7)
	       (= (strlen (rtos (nth 0 t1) 2 0)) 6)
	       ); end or
	   (or (= (strlen (rtos (nth 1 t1) 2 0)) 7)
	       (= (strlen (rtos (nth 1 t1) 2 0)) 6)
	       ); end or
	   ); end and
    (progn
      (if (and (zerop (rem (atof (rtos (nth 0 t1) 2 0)) 250))
	       (zerop (rem (atof (rtos (nth 1 t1) 2 0)) 250))
	       ); end and
	(progn
	  (setq t2 (mapcar '+ t1 '(0 250)))
	  (setq t3 (mapcar '+ t1 '(250 250)))
	  (setq t4 (mapcar '+ t1 '(250 0)))
	  (setq ny
		 (cond
		   ((= (strlen (rtos (nth 0 t1) 2 0)) 7) 2)
		   ((= (strlen (rtos (nth 0 t1) 2 0)) 6) 1)
		   ); end cond
		); end setq ny
	  (setq nx
		 (cond
		   ((= (strlen (rtos (nth 1 t1) 2 0)) 7) 2)
		   ((= (strlen (rtos (nth 1 t1) 2 0)) 6) 1)
		   ); end cond
		); end setq nx
	  (setq yt1 (substr (rtos (nth 0 t1) 2 0)nx)); определ€ю число Y
	  (setq xt1 (substr (rtos (nth 1 t1) 2 0)ny)); определ€ю число x
	  (setq yt2 (substr (rtos (nth 0 t2) 2 0)nx)); определ€ю число Y
	  (setq xt2 (substr (rtos (nth 1 t2) 2 0)ny)); определ€ю число x
	  (setq yt3 (substr (rtos (nth 0 t3) 2 0)nx)); определ€ю число Y
	  (setq xt3 (substr (rtos (nth 1 t3) 2 0)ny)); определ€ю число x
	  (setq yt4 (substr (rtos (nth 0 t4) 2 0)nx)); определ€ю число Y
	  (setq xt4 (substr (rtos (nth 1 t4) 2 0)ny)); определ€ю число x
	  (setq x1 (mapcar '+ t1 '(-5.9 0.5 0))); вычисл€ю координаты текста дл€ X=
	  (setq y1 (mapcar '+ t1 '(1.5 -5.9 0))); вычисл€ю координаты текста дл€ Y=
	  (setq x2 (mapcar '+ t2 '(-5.9 -1.5 0))); вычисл€ю координаты текста дл€ X=
	  (setq y2 (mapcar '+ t2 '(1.5 0.5 0))); вычисл€ю координаты текста дл€ Y=
	  (setq x3 (mapcar '+ t3 '(0.5 -1.5 0))); вычисл€ю координаты текста дл€ X=
	  (setq y3 (mapcar '+ t3 '(-0.5 0.5 0))); вычисл€ю координаты текста дл€ Y=
	  (setq x4 (mapcar '+ t4 '(0.5 0.5 0))); вычисл€ю координаты текста дл€ X=
	  (setq y4 (mapcar '+ t4 '(-0.5 -5.9 0))); вычисл€ю координаты текста дл€ Y=
	  (setq tn (mapcar '+ t1 '(0 250)))
	  (setq te (mapcar '+ t1 '(250 0)))
	  (setq ts (mapcar '+ t1 '(0 -250)))
	  (setq tw (mapcar '+ t1 '(-250 0)))
	  (setq nam (analisis_nomenclature_full_for_m500_sis_coor_oms t1))
	  (setq namn (analisis_nomenclature_full_for_m500_sis_coor_oms tn))
	  (setq name (analisis_nomenclature_full_for_m500_sis_coor_oms te))
	  (setq nams (analisis_nomenclature_full_for_m500_sis_coor_oms ts))
	  (setq namw (analisis_nomenclature_full_for_m500_sis_coor_oms tw))
	  (setq xy (mapcar '+ t1 '(114.883 259.25 0)))
	  (setq xyn (mapcar '+ t1 '(119.468 254.774 0)))
	  (setq xye (mapcar '+ t1 '(256.495 119.416 0)))
	  (setq xys (mapcar '+ t1 '(119.468 -6.44 0)))
	  (setq xyw (mapcar '+ t1 '(-4.841 119.416 0)))
	  (command "_insert" "«арамочное планшет 500_OMS" t1 "1" "1" "0")
	  (command "_explode" (entlast))
	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 x1) (cons 40 1) (cons 1 xt1) (cons 50 0) '(7 . "OTI"))); пишу текст координаты x
	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 y1) (cons 40 1) (cons 1 yt1) (cons 50 (/ pi 2)) '(7 . "OTI"))); пишу текст координаты y
	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 x2) (cons 40 1) (cons 1 xt2) (cons 50 0) '(7 . "OTI"))); пишу текст координаты x
	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 y2) (cons 40 1) (cons 1 yt2) (cons 50 (/ pi 2)) '(7 . "OTI"))); пишу текст координаты y
	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 x3) (cons 40 1) (cons 1 xt3) (cons 50 0) '(7 . "OTI"))); пишу текст координаты x
	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 y3) (cons 40 1) (cons 1 yt3) (cons 50 (/ pi 2)) '(7 . "OTI"))); пишу текст координаты y
	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 x4) (cons 40 1) (cons 1 xt4) (cons 50 0) '(7 . "OTI"))); пишу текст координаты x
	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 y4) (cons 40 1) (cons 1 yt4) (cons 50 (/ pi 2)) '(7 . "OTI"))); пишу текст координаты y
	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 xy) (cons 40 3) (cons 1 nam) (cons 50 0) '(7 . "OTI"))); пишу наменклатуру планшета
	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 xyn) (cons 40 1.5) (cons 1 namn) (cons 50 0) '(7 . "OTI"))); пишу наменклатуру планшета северной рамки
	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 xye) (cons 40 1.5) (cons 1 name) (cons 50 (/ pi 2)) '(7 . "OTI"))); пишу наменклатуру планшета восточной рамки
	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 xys) (cons 40 1.5) (cons 1 nams) (cons 50 0) '(7 . "OTI"))); пишу наменклатуру планшета южной рамки
	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 xyw) (cons 40 1.5) (cons 1 namw) (cons 50 (/ pi 2)) '(7 . "OTI"))); пишу наменклатуру планшета западной рамки
	  (princ)
	  ); end progn
	(progn
	  (terpri)
	  (princ "ќшибка! Ќе точно указан угол планшета!")
	  (princ)
	  )
	); end if
      ); end progn
    (progn
      (terpri)
      (princ "ќшибка! —истема координат не ќћ—!")
      (princ)
      )
    ); end if
  (princ)
  )
