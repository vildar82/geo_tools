(defun analisis_nomenclature_full_for_m500_sis_coor_ost (t1 / ty tx stb1 y1 str1 x1 stb2 str2)
(setq ty (nth 0 t1)); определяю число Y
(setq tx (nth 1 t1)); определяю число x
  (if; контроль попадания в СК г.Осташков
    (and
     (and
       (or (and (< 0 (nth 0 t1))(< (nth 0 t1) 8000))
   	   (= (nth 0 t1) 0)
	   ); end or
       ); end and
     (and
       (or (and (< -2000 (nth 1 t1))(< (nth 1 t1) 6000))
	   (= (nth 1 t1) -2000)
	   ); end or
       ); end end
     ); end and и условия
  (progn ; вычисление если попал

  (if (or (and (< 0 ty)(< ty 2000))(= 0 ty))
     (progn(setq stb1 1)(setq y1 0)))
  (if (or (and (< 2000 ty)(< ty 4000))(= 2000 ty))
     (progn(setq stb1 2)(setq y1 2000)))
  (if (or (and (< 4000 ty)(< ty 6000))(= 4000 ty))
     (progn(setq stb1 3)(setq y1 4000)))
  (if (or (and (< 6000 ty)(< ty 8000))(= 6000 ty))
     (progn(setq stb1 4)(setq y1 6000)))

  (if (or (and (> 6000 tx)(> tx 4000))(= 4000 tx))
     (progn(setq str1 24)(setq x1 4000)))
  (if (or (and (> 4000 tx)(> tx 2000))(= 2000 tx))
     (progn(setq str1 34)(setq x1 2000)))
  (if (or (and (> 2000 tx)(> tx 0))(= 0 tx))
     (progn(setq str1 44)(setq x1 0)))
    (if (or (and (> 0 tx)(> tx -2000))(= -2000 tx))
     (progn(setq str1 54)(setq x1 -2000)))
; конец вычисления строк и столбцов для первой части наменклатуры (25-28,35-38,45-48,55-58)
  (if (or (and (< y1 ty)(< ty (+ y1 1000)))(= y1 ty))
    (setq stb2 1))
  (if (or (and (< (+ y1 1000) ty)(< ty (+ y1 2000)))(= ty (+ y1 1000)))
    (setq stb2 2))
  
  (if (or (and (< (+ x1 1000) tx)(< tx (+ x1 2000)))(= tx (+ x1 1000)))
    (setq str2 0))
  (if (or (and (< x1 tx)(< tx (+ x1 1000)))(= x1 tx))
    (setq str2 2))
; конец вычисления строк и столбцов для второй части наменклатуры (А-Г)
  (setq num1 (itoa (+ stb1 str1)))
    (setq num2 (cond
	       ((= (+ stb2 str2) 1) "А")
	       ((= (+ stb2 str2) 2) "Б")
	       ((= (+ stb2 str2) 3) "В")
	       ((= (+ stb2 str2) 4) "Г")
	       ); cond
	); setq
  (setq num3 (rtos (analisis_nomenclature_3ch_for_m500_sis_coor_sel_or_ost t1) 2 0))
  (strcat num1 "-" num2 "-" num3)
   );progn T
  ); конец if
  ); defun analisis_nomenclature_full_for_m500_sis_coor_ost
