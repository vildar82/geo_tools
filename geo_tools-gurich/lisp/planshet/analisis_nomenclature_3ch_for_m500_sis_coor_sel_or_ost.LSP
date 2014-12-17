(defun analisis_nomenclature_3ch_for_m500_sis_coor_sel_or_ost ( t1 / stx sty stb str nx ny x y)
  (setq stx (rtos (nth 0 t1) 2 0))
  (setq sty (rtos (nth 1 t1) 2 0))
  (if
    (minusp (atof stx))
    (progn
      (setq nx
      (cond
	((= (strlen stx)5)3)
	((= (strlen stx)4)2)
	((= (strlen stx)1)1)
	);cond
	    );setq nx
      (setq x
	     (atof
	     (substr stx nx)
	    );atof
	    );setq x
      (setq stb
	     (cond
	       ((= x 0)1)
	       ((= x 750)2)
	       ((= x 500)3)
	       ((= x 250)4)
	       );cond
	    );setq stb
      );prong
    (progn
      (setq nx
      (cond
	((= (strlen stx)4)2)
	((= (strlen stx)3)1)
	((= (strlen stx)1)1)
	);cond
	    );setq nx
      (setq x
	     (atof
	     (substr stx nx)
	    );atof
	    );setq x
      (setq stb
	     (cond
	       ((= x 0)1)
	       ((= x 250)2)
	       ((= x 500)3)
	       ((= x 750)4)
	       );cond
	    );setq stb
      );prong
    ); if
  (if
    (minusp (atof sty))
    (progn
      (setq ny
      (cond
	((= (strlen sty)5)3)
	((= (strlen sty)4)2)
	((= (strlen sty)1)1)
	);cond
	    );setq ny
      (setq y
	     (atof
	     (substr sty ny)
	    );atof
	    );setq y
      (setq str
	     (cond
	       ((= y 0)12)
	       ((= y 750)8)
	       ((= y 500)4)
	       ((= y 250)0)
	       );cond
	    );setq str
      );prong
    (progn
      (setq ny
      (cond
	((= (strlen sty)4)2)
	((= (strlen sty)3)1)
	((= (strlen sty)1)1)
	);cond
	    );setq ny
      (setq y
	     (atof
	     (substr sty ny)
	    );atof
	    );setq y
      (setq str
	     (cond
	       ((= y 0)12)
	       ((= y 250)8)
	       ((= y 500)4)
	       ((= y 750)0)
	       );cond
	    );setq str
      );prong
    ); if
  (+ stb str)
  ); defun analisis_nomenclature_3ch_for_m500_sis_coor_sel_or_ost
