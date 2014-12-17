(defun C:draw_ramka_planshet_m500_coor_system_ostashkov ( / t1 t2 t3 t4 yt1 yt2 yt3 yt4 x1 x2 x3 x4 y1 y2 y3 y4 o tn te ts tw nam namn name nams namw xy xyn xye xys xyw nx ny n1)
(geo_tools-help "draw_ramka_planshet_m500_coor_system_ostashkov")
  (terpri)
  (initget)(setq t1(getpoint "”кажи левый нижний угол планшета: ")); указываю точку
  (if; контроль попадани€ в —  г.ќсташков
    (and
      (and
	(or (and (< 0 (nth 0 t1))(< (nth 0 t1) 8000))
	    (= (nth 0 t1) 0)
	    ); end or
	); end and
      (and
	(or (and (< -3000 (nth 1 t1))(< (nth 1 t1) 6000))
	    (= (nth 1 t1) -3000)
	    ); end or
	); end end
      ); end and и услови€
    (progn; ќсновное тело
      (setq t2 (mapcar '+ t1 '(0 250)))
      (setq t3 (mapcar '+ t1 '(250 250)))
      (setq t4 (mapcar '+ t1 '(250 0)))
      (setq yt1 (rtos (nth 0 t1) 2 0)); определ€ю число Y
      (setq xt1 (rtos (nth 1 t1) 2 0)); определ€ю число x
      (setq yt2 (rtos (nth 0 t2) 2 0)); определ€ю число Y
      (setq xt2 (rtos (nth 1 t2) 2 0)); определ€ю число x
      (setq yt3 (rtos (nth 0 t3) 2 0)); определ€ю число Y
      (setq xt3 (rtos (nth 1 t3) 2 0)); определ€ю число x
      (setq yt4 (rtos (nth 0 t4) 2 0)); определ€ю число Y
      (setq xt4 (rtos (nth 1 t4) 2 0)); определ€ю число x
      (setq x1 (mapcar '+ t1 (list (+ -3.2 (*(strlen xt1) -0.45)) 0.5 0))); вычисл€ю координаты текста дл€ X=
      (setq y1 (mapcar '+ t1 (list 1.5 (+ -3.2 (* (strlen yt1) -0.45)) 0))); вычисл€ю координаты текста дл€ Y=
      (setq x2 (mapcar '+ t2 (list (+ -3.2 (*(strlen xt2) -0.45)) -1.5 0))); вычисл€ю координаты текста дл€ X=
      (setq y2 (mapcar '+ t2 (list 1.5 (+ 3.2 (*(strlen yt2) -0.45)) 0))); вычисл€ю координаты текста дл€ Y=
      (setq x3 (mapcar '+ t3 (list (+ 3.2 (*(strlen xt3) -0.45)) -1.5 0))); вычисл€ю координаты текста дл€ X=
      (setq y3 (mapcar '+ t3 (list -0.5 (+ 3.2 (*(strlen yt3) -0.45)) 0))); вычисл€ю координаты текста дл€ Y=
      (setq x4 (mapcar '+ t4 (list (+ 3.2 (*(strlen xt4) -0.45))0.5  0))); вычисл€ю координаты текста дл€ X=
      (setq y4 (mapcar '+ t4 (list -0.5 (+ -3.2 (*(strlen yt4) -0.45)) 0))); вычисл€ю координаты текста дл€ Y=
      (setq tn (mapcar '+ t1 '(0 250)))
      (setq te (mapcar '+ t1 '(250 0)))
      (setq ts (mapcar '+ t1 '(0 -250)))
      (setq tw (mapcar '+ t1 '(-250 0)))
      (setq nam (analisis_nomenclature_full_for_m500_sis_coor_ost t1))
      (setq namn (analisis_nomenclature_full_for_m500_sis_coor_ost tn))
      (setq name (analisis_nomenclature_full_for_m500_sis_coor_ost te))
      (setq nams (analisis_nomenclature_full_for_m500_sis_coor_ost ts))
      (setq namw (analisis_nomenclature_full_for_m500_sis_coor_ost tw))
      (setq xy (mapcar '+ t1 '(118.645 259.25 0)))
      (setq xyn (mapcar '+ t1 '(121.41 254.774 0)))
      (setq xye (mapcar '+ t1 '(256.495 121.607 0)))
      (setq xys (mapcar '+ t1 '(121.41 -6.44 0)))
      (setq xyw (mapcar '+ t1 '(-4.841 121.607 0)))
      (command "_insert" "«арамочное планшет 500_ќсташков" t1 "1" "1" "0")
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
      ); end progn ќсновное тело
    (progn
      (terpri)
      (princ "ѕланшет за пределами месной системы координат г.ќсташков")
      (princ)
      );progn nil
    ); end if
  )
