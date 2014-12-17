(defun C:draw_ramka_planshet_m500_easy ( / t1 t2 t3 t4 yt1 yt2 yt3 yt4 x1 x2 x3 x4 y1 y2 y3 y4)
(geo_tools-help "draw_ramka_planshet_m500_easy")
  
  (initget)(setq t1(getpoint "\n”кажи левый нижний угол планшета:")); указываю точку
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

	  (command "_insert" "«арамочное планшет 500" t1 "1" "1" "0")
	  (command "_explode" (entlast))
	  (setq h-coor 0.9)
; пишу текст координаты x
(draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" xt1)
	      (cons "InsertionPoint" x1)
	      (cons "Height" h-coor)
	      (cons "Rotation" 0)
	      (cons "StyleName" "OTI")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 13)
	      ))
; пишу текст координаты y
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" yt1)
	      (cons "InsertionPoint" y1)
	      (cons "Height" h-coor)
	      (cons "Rotation" (/ pi 2))
	      (cons "StyleName" "OTI")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 7)
	      ))
; пишу текст координаты x
(draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" xt2)
	      (cons "InsertionPoint" x2)
	      (cons "Height" h-coor)
	      (cons "Rotation" 0)
	      (cons "StyleName" "OTI")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 7)
	      ))
; пишу текст координаты y
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" yt2)
	      (cons "InsertionPoint" y2)
	      (cons "Height" h-coor)
	      (cons "Rotation" (/ pi 2))
	      (cons "StyleName" "OTI")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 7)
	      ))
; пишу текст координаты x
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" xt3)
	      (cons "InsertionPoint" x3)
	      (cons "Height" h-coor)
	      (cons "Rotation" 0)
	      (cons "StyleName" "OTI")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 7)
	      ))
; пишу текст координаты y
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" yt3)
	      (cons "InsertionPoint" y3)
	      (cons "Height" h-coor)
	      (cons "Rotation" (/ pi 2))
	      (cons "StyleName" "OTI")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 13)
	      ))
; пишу текст координаты x
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" xt4)
	      (cons "InsertionPoint" x4)
	      (cons "Height" h-coor)
	      (cons "Rotation" 0)
	      (cons "StyleName" "OTI")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 13)
	      ))
; пишу текст координаты y
	  (draw-text-vla-by-list-property
	    (list
	      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	      (cons "TextString" yt4)
	      (cons "InsertionPoint" y4)
	      (cons "Height" h-coor)
	      (cons "Rotation" (/ pi 2))
	      (cons "StyleName" "OTI")
	      (cons "Linetype" "Continuous")
	      (cons "Alignment" 13)
	      ))
  )
