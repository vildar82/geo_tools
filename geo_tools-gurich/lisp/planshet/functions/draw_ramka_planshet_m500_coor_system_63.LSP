(defun C:draw_ramka_planshet_m500_coor_system_63 ( / t1 t2 t3 t4 yt1 yt2 yt3 yt4 x1 x2 x3 x4 y1 y2 y3 y4 o tn te ts tw nam namn name nams namw xy xyn xye xys xyw nx ny n1)
(geo_tools-help "draw_ramka_planshet_m500_coor_system_OMS")
  (terpri)
  (initget)(setq t1(getpoint "”кажи левый нижний угол планшета: ")); указываю точку отсчета
  (if (and (not (minusp (nth 0 t1)))
	   (not (minusp (nth 1 t1)))
	   (= (strlen (rtos (nth 0 t1) 2 0)) 7)
	   (= (strlen (rtos (nth 1 t1) 2 0)) 7)
	   ); end and
    (progn
      (if (and (zerop (rem (atof (rtos (nth 0 t1) 2 0)) 250))
	       (zerop (rem (atof (rtos (nth 1 t1) 2 0)) 250))
	       ); end and
	(progn
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
	  (setq x1 (mapcar '+ t1 '(-3.2 0.0 0))); вычисл€ю координаты текста дл€ X=
	  (setq y1 (mapcar '+ t1 '(0.5 -3.2 0))); вычисл€ю координаты текста дл€ Y=
	  (setq x2 (mapcar '+ t2 '(-3.2 -0.5 0))); вычисл€ю координаты текста дл€ X=
	  (setq y2 (mapcar '+ t2 '(0.5 3.2 0))); вычисл€ю координаты текста дл€ Y=
	  (setq x3 (mapcar '+ t3 '(3.2 -0.5 0))); вычисл€ю координаты текста дл€ X=
	  (setq y3 (mapcar '+ t3 '(0.0 3.2 0))); вычисл€ю координаты текста дл€ Y=
	  (setq x4 (mapcar '+ t4 '(3.2 0.0 0))); вычисл€ю координаты текста дл€ X=
	  (setq y4 (mapcar '+ t4 '(0.0 -3.2 0))); вычисл€ю координаты текста дл€ Y=
	  (setq tn (mapcar '+ t1 '(0 250)))
	  (setq te (mapcar '+ t1 '(250 0)))
	  (setq ts (mapcar '+ t1 '(0 -250)))
	  (setq tw (mapcar '+ t1 '(-250 0)))
	  (setq nam (analisis_nomenclature_full_for_m500_sis_coor_63 t1))
	  (setq namn (analisis_nomenclature_full_for_m500_sis_coor_63 tn))
	  (setq name (analisis_nomenclature_full_for_m500_sis_coor_63 te))
	  (setq nams (analisis_nomenclature_full_for_m500_sis_coor_63 ts))
	  (setq namw (analisis_nomenclature_full_for_m500_sis_coor_63 tw))
	  (setq xy (mapcar '+ t1 '(125.0 260.6 0)))
	  (setq xyn (mapcar '+ t1 '(125.0 256.5 0)))
	  (setq xye (mapcar '+ t1 '(257.00 125.0 0)))
	  (setq xys (mapcar '+ t1 '(125.0 -7.0 0)))
	  (setq xyw (mapcar '+ t1 '(-6.5 125.0 0)))
	  (command "_insert" "«арамочное планшет 500_63" t1 "1" "1" "0")
	  (command "_explode" (entlast))
	  (setq h-coor 0.9)
;;;	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 x1) (cons 40 1) (cons 1 xt1) (cons 50 0) '(7 . "OTI"))); пишу текст координаты x
(draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" xt1)
	      (cons "InsertionPoint" x1)
	      (cons "Height" h-coor)
	      (cons "Rotation" 0)
	      (cons "StyleName" "OTI")
;;;	      (cons "Layer" "‘ормат")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 13)
	      ))
;;;	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 y1) (cons 40 1) (cons 1 yt1) (cons 50 (/ pi 2)) '(7 . "OTI"))); пишу текст координаты y
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" yt1)
	      (cons "InsertionPoint" y1)
	      (cons "Height" h-coor)
	      (cons "Rotation" (/ pi 2))
	      (cons "StyleName" "OTI")
;;;	      (cons "Layer" "‘ормат")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 7)
	      ))
;;;	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 x2) (cons 40 1) (cons 1 xt2) (cons 50 0) '(7 . "OTI"))); пишу текст координаты x
(draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" xt2)
	      (cons "InsertionPoint" x2)
	      (cons "Height" h-coor)
	      (cons "Rotation" 0)
	      (cons "StyleName" "OTI")
;;;	      (cons "Layer" "‘ормат")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 7)
	      ))
;;;	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 y2) (cons 40 1) (cons 1 yt2) (cons 50 (/ pi 2)) '(7 . "OTI"))); пишу текст координаты y
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" yt2)
	      (cons "InsertionPoint" y2)
	      (cons "Height" h-coor)
	      (cons "Rotation" (/ pi 2))
	      (cons "StyleName" "OTI")
;;;	      (cons "Layer" "‘ормат")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 7)
	      ))
;;;	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 x3) (cons 40 1) (cons 1 xt3) (cons 50 0) '(7 . "OTI"))); пишу текст координаты x
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" xt3)
	      (cons "InsertionPoint" x3)
	      (cons "Height" h-coor)
	      (cons "Rotation" 0)
	      (cons "StyleName" "OTI")
;;;	      (cons "Layer" "‘ормат")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 7)
	      ))
;;;	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 y3) (cons 40 1) (cons 1 yt3) (cons 50 (/ pi 2)) '(7 . "OTI"))); пишу текст координаты y
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" yt3)
	      (cons "InsertionPoint" y3)
	      (cons "Height" h-coor)
	      (cons "Rotation" (/ pi 2))
	      (cons "StyleName" "OTI")
;;;	      (cons "Layer" "‘ормат")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 13)
	      ))
;;;	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 x4) (cons 40 1) (cons 1 xt4) (cons 50 0) '(7 . "OTI"))); пишу текст координаты x
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" xt4)
	      (cons "InsertionPoint" x4)
	      (cons "Height" h-coor)
	      (cons "Rotation" 0)
	      (cons "StyleName" "OTI")
;;;	      (cons "Layer" "‘ормат")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 13)
	      ))
;;;	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 y4) (cons 40 1) (cons 1 yt4) (cons 50 (/ pi 2)) '(7 . "OTI"))); пишу текст координаты y
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" yt4)
	      (cons "InsertionPoint" y4)
	      (cons "Height" h-coor)
	      (cons "Rotation" (/ pi 2))
	      (cons "StyleName" "OTI")
;;;	      (cons "Layer" "‘ормат")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 13)
	      ))
;;;	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 xy) (cons 40 3) (cons 1 nam) (cons 50 0) '(7 . "OTI"))); пишу наменклатуру планшета
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" nam)
	      (cons "InsertionPoint" xy)
	      (cons "Height" 3.0)
	      (cons "Rotation" 0)
	      (cons "StyleName" "OTI")
;;;	      (cons "Layer" "‘ормат")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 4)
	      ))
;;;	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 xyn) (cons 40 1.5) (cons 1 namn) (cons 50 0) '(7 . "OTI"))); пишу наменклатуру планшета северной рамки
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" namn)
	      (cons "InsertionPoint" xyn)
	      (cons "Height" 1.5)
	      (cons "Rotation" 0)
	      (cons "StyleName" "OTI")
;;;	      (cons "Layer" "‘ормат")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 7)
	      ))
;;;	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 xye) (cons 40 1.5) (cons 1 name) (cons 50 (/ pi 2)) '(7 . "OTI"))); пишу наменклатуру планшета восточной рамки
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" name)
	      (cons "InsertionPoint" xye)
	      (cons "Height" 1.5)
	      (cons "Rotation" (/ pi 2))
	      (cons "StyleName" "OTI")
;;;	      (cons "Layer" "‘ормат")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 13)
	      ))
;;;	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 xys) (cons 40 1.5) (cons 1 nams) (cons 50 0) '(7 . "OTI"))); пишу наменклатуру планшета южной рамки
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" nams)
	      (cons "InsertionPoint" xys)
	      (cons "Height" 1.5)
	      (cons "Rotation" 0)
	      (cons "StyleName" "OTI")
;;;	      (cons "Layer" "‘ормат")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 13)
	      ))
;;;	  (entmake (list '(0 . "TEXT") '(8 . "‘ормат") '(6 . "Continuous") (cons 10 xyw) (cons 40 1.5) (cons 1 namw) (cons 50 (/ pi 2)) '(7 . "OTI"))); пишу наменклатуру планшета западной рамки
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" namw)
	      (cons "InsertionPoint" xyw)
	      (cons "Height" 1.5)
	      (cons "Rotation" (/ pi 2))
	      (cons "StyleName" "OTI")
;;;	      (cons "Layer" "‘ормат")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 7)
	      ))
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
      (princ "ќшибка! —истема координат не 63 года(в координате - должно быть 7 значений)!")
      (princ)
      )
    ); end if
  (princ)
  )