(defun draw_profil_elevation (q_chek_v_pr q_sel_v_pr t0! mg mv q_optim_z q_sel_str_z 2Dpline / t1_ins_z t2_ins_z ht flag list_granic n p1 p2 ht styl htz spisokv styl z)
	 (if (= q_chek_v_pr 1)
	   (cond
	     ((= q_sel_v_pr 0) ; В канале
	      (cond
		((= q_sel_str_z 0); Факт.зем(люк)
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 25 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 10 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 1); Верх кан
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 40 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 25 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 2); Пол кан
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 55 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 40 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 3); Ось трубы
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 80 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 65 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		)
	      )
	     ((= q_sel_v_pr 1) ; ППУ
	      (cond
		((= q_sel_str_z 0); Факт.зем(люк)
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 25 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 10 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 1); Верх тр
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 40 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 25 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 2); Низ тр
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 55 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 40 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 3); Ось трубы
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 80 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 65 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		)
	      )
	     ((= q_sel_v_pr 2) ; Водопровод
	      (cond
		((= q_sel_str_z 0); Факт.зем
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 15 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 0 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 1); Проектн отм зем
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 30 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 15 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 2); Верх тр
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 45 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 30 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		)
	      )
	     ((= q_sel_v_pr 3) ; Канализация
	      (cond
		((= q_sel_str_z 0); Факт.зем
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 15 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 0 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 1); Проектн отм зем
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 30 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 15 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 2); отметка обич колодцев
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 45 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 30 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 3); отметка лотка труб
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 60 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 45 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		)
	      )
	     ((= q_sel_v_pr 4) ; Газопровод
	      (cond
		((= q_sel_str_z 0); Факт.зем
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 15 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 0 (/ 1000 mg)))))
		 (setq ht (/ 2.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 1); Отм дна траншеи
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 30 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 15 (/ 1000 mg)))))
		 (setq ht (/ 2.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 2); Верх тр
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 45 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 30 (/ 1000 mg)))))
		 (setq ht (/ 2.5 (/ 1000 mg)))
		 )
		)
	      )
	     ((= q_sel_v_pr 5) ; В канале + дренаж
	      (cond
		((= q_sel_str_z 0); Факт.зем(люк)
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 25 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 10 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 1); Верх кан
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 40 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 25 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 2); Пол кан
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 55 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 40 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 3); Ось трубы
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 80 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 65 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 4); обеч кол
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 225 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 210 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		((= q_sel_str_z 5); отметка лотка труб
		 (setq t1_ins_z (mapcar '- t0! (list 0 (/ 240 (/ 1000 mg)))))
		 (setq t2_ins_z (mapcar '- t0! (list 0 (/ 225 (/ 1000 mg)))))
		 (setq ht (/ 3.5 (/ 1000 mg)))
		 )
		)
	      )
	     )
	   (progn
	     (initget)(setq t1_ins_z(getpoint "\nУкажи нижнюю линию строки отметок: "))
	     (initget)(setq t2_ins_z(getpoint "\nУкажи верхнюю линию строки отметок: "))
	     (setq ht (/ 3.5 (/ 1000 mg)))
	     )
	   )
	 (if (= q_optim_z 1)
	   (progn
	     (setq flag T)
	     (while (eq flag T)
	       (if (eq list_granic nil)
		   (setq list_granic (analysis_limit_create 2Dpline))
		 )
	       (setq list_granic (analysis_limit_resizing list_granic 2Dpline))
	       (setq list_granic (analisis_granic_joining list_granic))
	       (setq flag (analisis_kontrol_flag_to_repeat_cycle list_granic 2Dpline))
	       );while
	     (draw_eledation_to_profile 2Dpline list_granic t0! t1_ins_z t2_ins_z ht)
	     )
	   (progn
	     (setq styl (getvar "TEXTSTYLE"))
	     (setq spisokv (extract_coord_lwpolyline (entget 2Dpline)))
	     (setq p1 (car spisokv))
	     (setq z (+(*(- (nth 1 p1) (nth 1 t0!))(/ (* mv (/ 1000 mg)) 1000))usl_z))
	     (entmake (list '(0 . "TEXT")
			    (cons 10 p1)
			    '(72 . 1) '(73 . 1)
			    (cons 11 (list(nth 0 p1)(+(nth 1 t1_ins_z)(/(- (nth 1 t2_ins_z)(nth 1 t1_ins_z))2))))
			    (cons 40 ht)
			    (cons 50 (/ pi 2))
			    (cons 1 (rtos z 2 2))
			    (cons 7 styl)))
	     (setq n (1-(length spisokv)))
	     (repeat n
	       (setq p1 (car spisokv))
	       (setq spisokv (cdr spisokv))
	       (setq p2 (car spisokv))
	       (cond
		 ((> (*(-(nth 0 p2)(nth 0 p1))0.6) ht)
		  (setq htz ht))
		 ((< (*(-(nth 0 p2)(nth 0 p1))0.6) (/ 1.6 (/ 1000 mg)))
		  (setq htz (/ 1.6 (/ 1000 mg))))
		 (T
		  (setq htz (*(-(nth 0 p2)(nth 0 p1))0.6)))
		 )
	       (setq z (+(*(- (nth 1 p2) (nth 1 t0!))(/ (* mv (/ 1000 mg)) 1000))usl_z))
	       (entmake (list '(0 . "TEXT")
			      (cons 10 p2)
			      '(72 . 1) '(73 . 1)
			      (cons 11 (list(nth 0 p2)(+(nth 1 t1_ins_z)(/(- (nth 1 t2_ins_z)(nth 1 t1_ins_z))2))))
			      (cons 40 htz)
			      (cons 50 (/ pi 2))
			      (cons 1 (rtos z 2 2))
			      (cons 7 styl)))
	       )
	     )
	   )
	 )
