(defun analisis_nomenclature_3ch_for_m500_sis_coor_oms1 (stx sty / x y stb str ny nx)
 (setq ny
	(cond
	  ((= (strlen sty) 7) 5)
	  ((= (strlen sty) 6) 4)
	  ); end cond
       ); end setq ny
  (setq nx
	(cond
	  ((= (strlen stx) 7) 5)
	  ((= (strlen stx) 6) 4)
	  ); end cond
       ); end setq nx
(setq x (atoi (substr stx nx 3)))
(setq y (atoi (substr sty ny 3)))
(setq stb (cond
	    ((= x 750) 4)
	    ((= x 500) 3)
	    ((= x 250) 2)
	    ((= x 0) 1)
	    ); end cond
      );setq stb
(setq str (cond
	    ((= y 750) 0)
	    ((= y 500) 4)
	    ((= y 250) 8)
	    ((= y 0) 12)
	    ); end cond
      ); end setq str
(itoa (+ stb str))
  ); end defun analisis_nomenclature_3ch_for_m500_sis_coor_oms1
