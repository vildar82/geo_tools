(defun C:draw_rectangle_text_description_and_number_to_building ( / t0
								 t1 t2 t3 t4 t5 t6
								 c1 d
								 shotd
								 longa
								 longd
								 long
								 str
								 tx1 ttt txy h obj_0 obj_1 obj_n obj_x sp_obj obj_next
								 l_obj_1 l_obj_n l_obj hE dE key
								 long_d
								 h h_min h_min+ h_max h_1 h_n)
(geo_tools-help "draw_rectangle_text_description_and_number_to_building")
;;;  (format_check_text_style)
  (initget 1)(setq t1(getpoint "\nУкажи первую точку:")); указываю точку
  (initget 1)(setq t2(getpoint t1 "\nУкажи вторую точку:")); указываю вторую точку
  (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t1) (append '(11) t2)))
  (initget 1)(setq t3(getpoint t2 "\nУкажи третью точку:")); указываю третью точку
  (setq с1(+(angle t2 t1) (* 90 (/ pi 180)))); вычисляю угол на t4
  (setq d(*(distance t2 t3) (cos (-(* 180 (/ pi 180)) (- (- (angle t2 t1)(angle t2 t3)) (* 90 (/ pi 180))))))); вычисляю расстояние на t4
  (setq t4(polar t2 с1 d)); вычисляю координаты на t4
  (setq t5(polar t4 (angle t2 t1) (distance t1 t2))); вычисляю координаты на t5
  (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t2) (append '(11) t4)))
  (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t4) (append '(11) t5)))
  (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t5) (append '(11) t1)))
  (setq t6
	 (polar
	   (polar t1 (angle t1 t2) (/ (distance t1 t2) 2))
	   (angle t1 t5)
	   (/ (distance t1 t5) 2)
	   ); polar
	); setq t6 центральная точка прямоугольника
  (if (> (distance t1 t2)
	 (distance t1 t5)
	 ); конец условия
    (progn
      (setq longa (angle t1 t2))
      (setq shotd (distance t1 t5))
      (setq longd (distance t1 t2))
      ); progn
    (progn
      (setq longa (angle t1 t5))
      (setq shotd (distance t1 t2))
      (setq longd (distance t1 t5))
      ); progn
    ); if
  (setq long (analysis_angle_be_long_distance longa)
	long_t (analysis_angle_be_long_distance longa))
  (if
    (>
      (+ (/ 1.6(getvar"CANNOSCALEVALUE"))(*(/ (/ 1.6(getvar"CANNOSCALEVALUE")) 3)2))shotd); условие
    (progn
      (setq h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.07372))
      (setq long_t 0)
      (princ "\nПодпись не входит в контур, попробуй разместить рядом.")
      )
    (progn
      (setq h (- shotd (/ 1 (getvar "CANNOSCALEVALUE"))))
      (if
	(> h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.07372))
	(setq h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.07372))
	); if
      ); progn
    ); if
  (setq longn (+(- (* 2 pi) long) (/ pi 2)))
  (if (> longn (* 2 pi))
    (setq longn (- longn (* 2 pi)))
    )
  (setq long_d (calculation_radiany_to_gms longn))
  (setq obj_0 (entlast))
  (terpri)
  (princ "Введи этажность и буквенный индекс: ")
  (setvar "CMDECHO" 0)
  (command "_dtext" "_s" "Arial" "в" "сц" t6 h long_d)
  (setvar "CMDECHO" 1)
  (setq obj_n (entlast)); последний объект
  (setq obj_1 (entnext obj_0)) ; объект с характеристикой здания
  (if
    (not (eq obj_1 obj_n))
    (progn
      (if (not(eq (entnext obj_1) obj_n))
	(progn
	  (setq obj_next (entnext obj_1))
	  (while (not (eq obj_next obj_n))
	    (setq sp_obj (append sp_obj(list obj_next)))
	    (setq obj_next (entnext obj_next))
	    (if
	      (eq obj_next obj_n)
	      (setq sp_obj (append sp_obj(list obj_next)))
	      )
	    )
	  )
	(setq sp_obj (append sp_obj(list obj_n))); sp_obj список объектов вспомогательной надписи
	)
      (setq h_min (+ (/ 1.6(getvar"CANNOSCALEVALUE"))
		     (*(length sp_obj)(/ 1.6(getvar"CANNOSCALEVALUE")))
		     (*(+(length sp_obj)2)(/(/ 1.6(getvar"CANNOSCALEVALUE"))3))))
      (setq h_min+ (+ (/ 2.25(getvar"CANNOSCALEVALUE"))
		      (*(length sp_obj)(/ 1.6(getvar"CANNOSCALEVALUE")))
		      (*(+(length sp_obj)2)(/(/ 1.6(getvar"CANNOSCALEVALUE"))3))))
      (setq h_max (+ (/ 3(getvar"CANNOSCALEVALUE"))
		     (*(length sp_obj)(/ 2(getvar"CANNOSCALEVALUE")))
		     (*(+(length sp_obj)2)(/(/ 2(getvar"CANNOSCALEVALUE"))3))))
      (if
	(< shotd h_min)
	(setq h_1 (/ 3(getvar"CANNOSCALEVALUE"))
	      h_n (/ 2(getvar"CANNOSCALEVALUE"))
	      long_t 0)
	(if
	  (< shotd h_min+)
	  (setq h_n (/ 1.6(getvar"CANNOSCALEVALUE"))
		h_1 (- shotd
		       (*(length sp_obj)(/ 1.6(getvar"CANNOSCALEVALUE")))
		       (* (+(length sp_obj)2) (/ (/ 1.6(getvar"CANNOSCALEVALUE")) 3)))
		)
	  (if
	    (< shotd h_max)
	    (progn
	      (setq h_1 (/(/(* 9 shotd)(+ 13 (* 8 (length sp_obj))))(getvar"CANNOSCALEVALUE")))
	      (setq h_n (/(/(* 6 shotd)(+ 13 (* 8 (length sp_obj))))(getvar"CANNOSCALEVALUE")))
	      )
	    (progn
	      (setq h_1 (/ 3(getvar"CANNOSCALEVALUE")))
	      (setq h_n (/ 2(getvar"CANNOSCALEVALUE")))
	      )
	    )
	  )
	)
      (setq hE (+ h_1 (*(length sp_obj)h_n) (* (length sp_obj) (/ h_n 3))))
      (setq t0(polar t6 (+ long_t (/ pi 2)) (- (/ hE 2) (/ h_1 2))))
      (entmod(list(cons -1 obj_1)
		  (cons 40 (/ h_1 1.07372))
		  (cons 11 t0)
		  (cons 50 long_t)
		  )
	     )
      (entupd obj_1)
      (setq t0(polar t0(- long_t (/ pi 2))(+ (/ h_n 3)(/ h_n 2)(/ h_1 2))))
      (mapcar '(lambda (obj_x)
		 (entmod(list (cons -1 obj_x)
			      (cons 40 (/ h_n 1.05994))
			      (cons 11 t0)
			      '(7 . "OTI")
			      (cons 50 long_t)
			      )
			)
		 (entupd obj_x)
		 (setq t0(polar t0 (- long_t (/ pi 2)) (+ (/(cdr (assoc 40 (entget obj_x)))3) (cdr (assoc 40 (entget obj_x))))))
		 )
	      sp_obj)
      (if; обработка одной подписи типа К гар.
	(and (eq (entnext obj_1) obj_n)
	     (< (+(calculation_str_long_be_h_stile (cdr (assoc 1 (entget obj_1)))(/ 1.6(getvar"CANNOSCALEVALUE")) "OTI")
		  (calculation_str_long_be_h_stile (cdr (assoc 1 (entget obj_n)))(/ 1.6(getvar"CANNOSCALEVALUE")) "OTI")
		  (/ (* h_n 3) 3))longd)
	     (<(+(/ 1.6(getvar"CANNOSCALEVALUE"))(*(/(/ 1.6(getvar"CANNOSCALEVALUE"))3)2))shotd)
	     )
	(progn
	  (initget "Да Нет")(setq key (getkword "\nПоменять расположение слов? [Да/Нет] <Да>"))
	  (if (eq key nil)(setq key "Да"))
	  (if (eq key "Да")
	    (progn
	      (setq h_1 (- shotd (/(*(/ h_n 3)2)(getvar "CANNOSCALEVALUE"))))
	      (if
		(> h_1 (/ 3(getvar "CANNOSCALEVALUE")))
		(setq h_1 (/ 3(getvar "CANNOSCALEVALUE")))
		); if
	      (entmod (subst (cons 40 h_1)(assoc 40 (entget obj_1)) (entget obj_1)))
	      (setq h_n (* h_1 0.66))
	      (if
		(< h_n (/ 1.6(getvar "CANNOSCALEVALUE")))
		(setq h_n (/ 1.6(getvar "CANNOSCALEVALUE")))
		)
	      (entmod (subst (cons 40 h_n)(assoc 40 (entget obj_n)) (entget obj_n)))
	      (setq dE(+(calculation_text_height_be_object obj_1)(calculation_text_height_be_object obj_n)(/ h_n 3)))calculation_str_long_be_h_stile stroka ht styl
	      (setq t0(polar t6 (+ long pi)(-(/ dE 2)(/(calculation_text_height_be_object obj_1)2))))
	      (entmod
		(list(cons -1 obj_1)
		     (cons 11 t0)
		     (cons 40 (/ h_1 1.07372))
		     (cons 50 long)
		     )
		)
	      (entupd obj_1)
	      (setq t0(polar t0 long(+(/(calculation_text_height_be_object obj_1)2)(/(calculation_text_height_be_object obj_n)2)(/ h_n 3))))
	      (entmod
		(list(cons -1 obj_n)
		     (cons 11 t0)
		     (cons 40 (/ h_n 1.05994))
		     (cons 50 long)
		     )
		)
	      (entupd obj_n)
	      )
	    )
	  )
	); обработка одной подписи типа К гар.
      )
    ); обработка если есть описательные надписи
  (initget)(setq p_num(getpoint "\nУкажи угол для подписи номера дома <Enter - пропуск>:"))
  (if (not(eq p_num nil))
    (progn
      (setq h_num (/ 2(getvar"CANNOSCALEVALUE")))
      (cond
	((equal p_num t1 1)
	 (setq ps_num (polar (polar t1 (angle t1 t5) (/ (/ h_num 3) (getvar "CANNOSCALEVALUE"))) (angle t1 t2) (/ (/ h_num 3) (getvar "CANNOSCALEVALUE"))))
	 )
	((equal p_num t2 1)
	 (setq ps_num (polar (polar t2 (angle t2 t1) (/ (/ h_num 3) (getvar "CANNOSCALEVALUE"))) (angle t2 t4) (/ (/ h_num 3) (getvar "CANNOSCALEVALUE"))))
	 
	 )
	((equal p_num t4 1)
	 (setq ps_num (polar (polar t4 (angle t4 t2) (/ (/ h_num 3) (getvar "CANNOSCALEVALUE"))) (angle t4 t5) (/ (/ h_num 3) (getvar "CANNOSCALEVALUE"))))
	 
	 )
	((equal p_num t5 1)
	 (setq ps_num (polar (polar t5 (angle t5 t1) (/ (/ h_num 3) (getvar "CANNOSCALEVALUE"))) (angle t5 t4) (/ (/ h_num 3) (getvar "CANNOSCALEVALUE"))))
	 
	 )
	(T
	 (setq p_num nil)
	 (alert "Указана неверная точка!")
	 )
	)
      (if (not (eq p_num nil))
	(progn
	  (setq long_d (calculation_radiany_to_gms longn))
	  (setq text_ug (analysis_what_angle_if_rectangle t1 t2 t4 t5 p_num long))
	  (if (eq text_ug "")
	    (progn
	      (setq osn (getvar "OSMODE"))
	      (setvar "OSMODE" 0)
	      (setvar "CMDECHO" 0)
	      (terpri)
	      (princ "Введи номер дома: ")
	      (command "_dtext" "_s" "OTI" ps_num h_num long_d)
	      (setvar "CMDECHO" 1)
	      (setvar "OSMODE" osn)
	      )
	    (progn
	      (setq osn (getvar "OSMODE"))
	      (setvar "OSMODE" 0)
	      (setvar "CMDECHO" 0)
	      (terpri)
	      (princ "Введи номер дома: ")
	      (command "_dtext" "_s" "OTI" "в" text_ug ps_num h_num long_d)
	      (setvar "CMDECHO" 1)
	      (setvar "OSMODE" osn)
	      )
	    )
	  )
	)
      )
    )
  (princ)
  )
