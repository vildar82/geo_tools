(defun C:probba ( / nabor n t1 t2 line str)
  (setq nabor(ssget))
  (setq n 0)
  (setq str "")
  (repeat (sslength nabor)
    (setq line (ssname nabor n))
    (setq t1 (cdr (assoc 10 (entget line))))
    (setq t2 (cdr (assoc 11 (entget line))))
    (setq str (strcat str" (list'("
		      (rtos(nth 0 t1)2 3)" "
		      (rtos(nth 1 t1)2 3)" "
		      (rtos(nth 2 t1)2 3)") '("
		      (rtos(nth 0 t2)2 3)" "
		      (rtos(nth 1 t2)2 3)" "
		      (rtos(nth 2 t2)2 3)"))"
		      )
	  )
    (setq n (1+ n))
    )
  str
  )
(defun C:prob ( / nabor n t1 vis shir text str)
  (setq nabor(ssget))
  (setq n 0)
  (setq str "")
  (repeat (sslength nabor)
    (setq text (ssname nabor n))
    (setq t1 (cdr (assoc 10 (entget text))))
    (setq vis (cdr (assoc 40 (entget text))))
    (setq a (cdr (assoc 50 (entget text))))
    (setq num (cdr (assoc 1 (entget text))))
    (setq str (strcat str" (list " num (rtos vis 2 0)" "
		      (rtos shir 2 0)
		      " '("
		      (rtos(nth 0 t1)2 3)" "
		      (rtos(nth 1 t1)2 3)" "
		      (rtos(nth 2 t1)2 3)")5)"
		      )
	  )
    (setq n (1+ n))
    )
  str
  )

(defun C:polyotmetka (/ pline stroka_ p y0 spisokv n p z t1 t2 t3 ht styl); снимает отметки с вершин полилинии на профиле
  (setq pline (get_pline "\nВыбери ПОЛИЛИНИЮ: "))
  (setq m (get_m))
  (setq t1! (get_t1!))
  (setq usl_z (get_usl_z))
  (initget)(setq t1(getpoint "\nУкажи нижнюю линию строки: "))
  (initget)(setq t2(getpoint "\nУкажи верхнюю линию строки: "))
  (initget "Да Нет")(setq yn (getkword "\nЛинии рисуем? [Да/Нет] <Нет>: "))
  (if (= yn nil) (setq yn "Нет"))
  (if (= yn "Да")
    (progn
      (initget)(setq t3(getpoint "\nУкажи до куда: "))
      )
    )
  (setq spisokv (extract_coord_lwpolyline pline))
  (setq n (length spisokv))
  (repeat n
    (setq p (car spisokv))
    (setq spisokv (cdr spisokv))
    (setq z (+(*(- (nth 1 p) (nth 1 t1!))(/ (* m (getvar "CANNOSCALEVALUE")) 1000))usl_z))
    (setq ht (/ 3.5 (getvar "CANNOSCALEVALUE")))
    (setq styl (getvar "TEXTSTYLE"))
    (entmake (list '(0 . "TEXT")
		   (cons 10 (list (-(nth 0 p) (/ ht 4)) (+(nth 1 t1)(-(/(- (nth 1 t2)(nth 1 t1))2)(/(numxHS (rtos z 2 2) ht styl)2)))))
		   (cons 40 ht)
		   (cons 50 (/ pi 2))
		   (cons 1 (rtos z 2 2))
		   (cons 7 styl)))
    (if (= yn "Да")
      (entmake (list '(0 . "LINE") '(6 . "Continuous") (cons 10 p) (cons 11 (list (nth 0 p)(nth 1 t3)))))
      )
    )
  (princ)
  )

(defun C:polyrasstoianie (/ pline spisokv n p1 p2 s t1 t2 styl ht); снимает расстояние между вершинами полилинии на профиле
  (setq pline (get_pline "\nВыбери ПОЛИЛИНИЮ: "))
  (initget)(setq t1(getpoint "\nУкажи нижнюю линию строки: "))
  (initget)(setq t2(getpoint "\nУкажи верхнюю линию строки: "))
  (initget "Да Нет")(setq yn (getkword "\nЛинии рисуем? [Да/Нет] <Да>: "))
  (if (= yn nil) (setq yn "Да"))
  (setq spisokv (extract_coord_lwpolyline pline))
  (setq n (1- (length spisokv)))
  (repeat n
    (setq p1 (car spisokv))
    (setq spisokv (cdr spisokv))
    (setq p2 (car spisokv))
    (setq s (rtos (- (nth 0 p2) (nth 0 p1)) 2 2))
    (setq ht (/ 3.5 (getvar "CANNOSCALEVALUE")))
    (setq styl (getvar "TEXTSTYLE"))
    (entmake (list '(0 . "TEXT")
		   (cons 10 (list (+(nth 0 p1)(-(/(- (nth 0 p2)(nth 0 p1))2)(/(numxHS s ht styl)2))) (+(nth 1 t1)(-(/(-(nth 1 t2)(nth 1 t1))2)(/ ht 2))) ))
		   (cons 40 ht)
		   (cons 1 s)
		   (cons 7 styl)))
    )
  (if (= yn "Да")
    (progn
      (setq spisokv (extract_coord_lwpolyline pline))
      (setq n (length spisokv))
      (repeat n
	(setq p (car spisokv))
	(setq spisokv (cdr spisokv))
	(entmake (list '(0 . "LINE") '(6 . "Continuous") (cons 10 (list (nth 0 p)(nth 1 t2))) (cons 11 (list (nth 0 p)(nth 1 t1)))))
	)
      )
    )
  (princ)
  )

(defun C:polyuklon (/ pline spisokv n p1 p2 ht hs styl S pro t1 t2 t1 t2t11 t1- t22 t2- H1 H2)
  (setq pline (get_pline "\nВыбери ПОЛИЛИНИЮ: "))
  (setq m (get_m))
  (initget)(setq t01(getpoint "\nУкажи нижнюю линию строки: "))
  (initget)(setq t02(getpoint "\nУкажи верхнюю линию строки: "))
  (initget "Да Нет")(setq yn (getkword "\nЛинии рисуем? [Да/Нет] <Нет>: "))
  (if (= yn nil) (setq yn "Нет"))
  (setq spisokv (extract_coord_lwpolyline pline))
  (setq n (1- (length spisokv)))
  (repeat n
    (setq p1 (car spisokv))
    (setq spisokv (cdr spisokv))
    (setq p2 (car spisokv))
    (if
      (/= (- (nth 0 p1) (nth 0 p2)) 0)
      (progn
	(setq ht (/ 3.5 (getvar "CANNOSCALEVALUE")))
	(setq hs (- (nth 1 t02)(nth 1 t01)))
	(setq styl (getvar "TEXTSTYLE"))
	(setq S (rtos (- (nth 0 p2) (nth 0 p1)) 2 2))
	(setq pro (rtos (abs(*(/ (* (- (nth 1 p1) (nth 1 p2))(/ (* m (getvar "CANNOSCALEVALUE")) 1000)) (- (nth 0 p1) (nth 0 p2))) 1)) 2 3))
	(setq t1 (list (nth 0 p1)(nth 1 t01)))
	(setq t2 (list (nth 0 p2)(nth 1 t01)))
	(setq H1 (nth 1 p1))
	(setq H2 (nth 1 p2))
	(setq t11 (mapcar '+ (list(nth 0 p1)(nth 1 t1)) (list 0 hs)))
	(setq t1- (mapcar '+ (list(nth 0 p1)(nth 1 t1)) (list 0 (/ hs 2))))
	(setq t22 (mapcar '+ (list(nth 0 p2)(nth 1 t1)) (list 0 hs)))
	(setq t2- (mapcar '+ (list(nth 0 p2)(nth 1 t1)) (list 0 (/ hs 2))))
	(cond
	  ((= H1 H2)
	   (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t1-) (append '(11) t2-)))
	   (entmake (list '(0 . "TEXT")  (cons 10 (mapcar '+ t1 (list (- (/(distance t1 t2)2)(/ (numxHS "0.00" ht styl)2)) (* ht 2.5)))) (cons 40 ht) (cons 1 "0.0") (cons 7 styl)))
	   (entmake (list '(0 . "TEXT")  (cons 10 (mapcar '+ t1 (list (- (/(distance t1 t2)2)(/ (numxHS S ht styl)2)) (/ ht 2)))) (cons 40 ht) (cons 1 S) (cons 7 styl)))
	   )
	  ((> H1 H2)
	   (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t11) (append '(11) t2)))
	   (entmake (list '(0 . "TEXT")  (cons 10 (mapcar '- t22 (list (+ (numxHS pro ht styl) (/ ht 2)) (* ht 1.5)))) (cons 40 ht) (cons 1 pro) (cons 7 styl)))
	   (entmake (list '(0 . "TEXT")  (cons 10 (mapcar '+ t1 (list (/ ht 2) (/ ht 2)))) (cons 40 ht) (cons 1 S) (cons 7 styl)))
	   )
	  ((< H1 H2)
	   (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t1) (append '(11) t22)))
	   (entmake (list '(0 . "TEXT")  (cons 10 (mapcar '+ t11 (list (/ ht 2) (* ht -1.5)))) (cons 40 ht) (cons 1 pro) (cons 7 styl)))
	   (entmake (list '(0 . "TEXT")  (cons 10 (mapcar '- t2 (list (+ (numxHS S ht styl) (/ ht 2)) (/ ht -2)))) (cons 40 ht) (cons 1 S) (cons 7 styl)))
	   )
	  )
	)
      )
    )
  (if (= yn "Да")
    (progn
      (setq spisokv (extract_coord_lwpolyline pline))
      (setq n (length spisokv))
      (repeat n
	(setq p (car spisokv))
	(setq spisokv (cdr spisokv))
	(entmake (list '(0 . "LINE") '(6 . "Continuous") (cons 10 (list (nth 0 p)(nth 1 t02))) (cons 11 (list (nth 0 p)(nth 1 t01)))))
	)
      )
    )
  (princ)
  )

(defun C:TES ()
  (setq *acad* (vlax-get-acad-object))
  (setq C3D (vla-getinterfaceobject *acad* "AeccXUiLand.AeccApplication.5.0"))
  (setq C3Ddoc (vla-get-activedocument C3D))
  (vlax-dump-object C3Ddoc)
  (setq pnts (vlax-get C3Ddoc 'points))
  ;;;ask to select points
  (princ "\nSelect points to query the elevations of: ")
  (setq selectedPnts (vlax-invoke C3Ddoc 'SelectPoints))
  (foreach pnt selectedPnts
    (setq elev (vlax-get (vlax-invoke pnts 'find pnt) 'elevation))
    (princ (strcat "\nElevation of Pt#" (itoa pnt) " = " (rtos elev 2 2)))
    )
  (foreach pnt selectedPnts
    (vlax-dump-object pnt T)
    )
  )



(defun C:TES (/ dcl_id)
  (setq dcl_id (load_dialog "d_okno.dcl"))
  (new_dialog "d_okno" dcl_id)
  (start_dialog)
  (unload_dialog dcl_id)
  )


(defun C:TESTS (/ file)
      (setq object (car (entsel "\nВыбери текст: ")))
  (setq ttt (entget object))
  (setq ttt (entget (entnext(entnext object))))
  (setq file (open "C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\data.txt" "w"))
  (prin1 "\nстрочка 123" file)

  (princ ttt file)
  (princ(read-line file))
  (princ(strcat "\n"(read-line file)))
  (princ(read-line file))
  (princ(read-line file))
   
  (initget "Да Нет")(setq t1 (getpoint "\nУкажи точку #1 [Да/Нет]: "))
  (initget "Да Нет")(setq yn (getkword "\nЛинии рисуем? [Да/Нет] <Да>: "))
(close file)
  
  )




(defun C:point_masck (/ object t10 t11 t12 ttt ooo)
  (while (= object nil)
    (setq object (car (entsel "\nВыбери пикет: ")))
    (if (or (= object nil) (not (eq (cdr (assoc 0 (entget object))) "AECC_POINT")))
      (progn
      (princ "\nОШИБКА! Объект не выбран или выбран не пикет!")
      (setq object nil)
      )
      ); if
    ); while
  (setq ttt (list '(0 . "TEXT") (cons 10 (cdr (assoc 10 (entget object))))
		 (cons 40 (cdr (assoc 140 (entget object))))
		 (cons 1 (rtos
			   (caddr (cdr (assoc 11 (entget object))))
			   2 2)
		       )
		 (cons 50 (cdr (assoc 142 (entget object))))
		 '(7 . "OTI")
		 )
	)
  (setq t10(polar
	     (polar (cdr (assoc 10 (entget object))) (cdr (assoc 142 (entget object))) (-(cdr (assoc 140 (entget object)))(/ 0.2 (m_kof))))
	     (- (cdr (assoc 142 (entget object))) (/ pi 2)) (+(/ (cdr (assoc 140 (entget object))) 2)(/ 0.2 (m_kof)))))
  (setq t11(polar '(0.0 0.0 0.0) (cdr (assoc 142 (entget object))) (+ (numx ttt) (/ 0.4 (m_kof))))); вычисляю координаты на t11
  (setq t12(polar '(0.0 0.0 0.0) (+ (cdr (assoc 142 (entget object))) (/ pi 2)) (+ (numy ttt) (/ 0.4 (m_kof))))); вычисляю координаты на t12
  (entmake (list '(0 . "WIPEOUT") '(100 . "AcDbEntity")  '(67 . 0) '(100 . "AcDbWipeout") '(90 . 0)
		 (cons 10 t10)
		 (cons 11 t11)
		 (cons 12 t12)
		 '(13 1.0 1.0 0.0)
		 '(70 . 7)
		 '(280 . 1)
		 '(281 . 50) '(282 . 50) '(283 . 0)
		 '(71 . 2)
		 '(91 . 5)
		 '(14 -0.5 -0.5 0.0)
		 '(14 0.5 -0.5 0.0)
		 '(14 0.5 0.5 0.0)
		 '(14 -0.5 0.5 0.0)
		 '(14 -0.5 -0.5 0.0)))
  (setq ooo (entget object))
  (entdel object)
  (entmake ooo)
  (princ)
  )

(defun C:point_unmasck ( / object t10)
    (while (= object nil)
    (setq object (car (entsel "\nВыбери пикет: ")))
    (if (or (= object nil) (not (eq (cdr (assoc 0 (entget object))) "AECC_POINT")))
      (progn
      (princ "\nОШИБКА! Объект не выбран или выбран не пикет!")
      (setq object nil)
      )
      ); if
    ); while
  (setq t10(polar
	     (polar (cdr (assoc 10 (entget object))) (cdr (assoc 142 (entget object))) (-(cdr (assoc 140 (entget object)))(/ 0.2 (m_kof))))
	     (- (cdr (assoc 142 (entget object))) (/ pi 2)) (+(/ (cdr (assoc 140 (entget object))) 2)(/ 0.2 (m_kof)))))
  (setq nabor (ssget "_X" (list '(0 . "WIPEOUT") (cons 10 t10))))
  (if (not (eq nabor nil))
    (progn
      (setq masck (ssname nabor 0))
      (entdel masck)
      )
    )
  (princ)
  )





(defun C:testpl ()
  (terpri)
  (initget)(setq t1(getpoint "Insert point/Укажи точку:")); указываю точку
  (princ (nampltur t1))
  (princ)
  )

(defun C:napravlenie (/ LINE t1 text ang t2); рисует линии в заданом направлении от севера
  (initget)(setq LINE (car (entsel "\nВыбери линию: ")))
  (setq t1 (cdr (assoc 10 (entget LINE))))
  (setq t2 (cdr (assoc 11 (entget LINE))))
  (setq ang (gms))
  (setq r (angle t1 t2))
  (setq a
	 (if
	   (< ang r)
	   (- r ang)
	   (- (+ r (* pi 2)) ang)
	   )
	)
  (while (not (= a nil))
    (setq t3 (polar t1 a 1000))
    (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t1) (append '(11) t3)))
  (initget)(setq text (getstring "\nВведи угол в формате 180.0035 = 180 градусов 00 минут 35 секунд: "))
  (setq ang (gms))
  (setq r (angle t1 t2))
  (setq a
	 (if
	   (< ang r)
	   (- r ang)
	   (- (+ r (* pi 2)) ang)
	   )
	)  
    )
  (princ)
  )


(defun gms ( / grad minu sek); переводит градусы минуты секунды в радианы
  (initget)(setq grad (getreal "\nВведи число градусов: "))
  (initget)(setq minu (getreal "\nВведи число минут: "))
  (initget)(setq sek (getreal "\nВведи число секунд: "))
  (* (/ pi 180)(+ (float grad) (/ (+ (float minu) (/ (float sek) 60)) 60)))
    )



(inters a b c d)

(defun C:srtreug ()
  (initget)(setq t1(getpoint "\nУкажи точку #1: "))
  (initget)(setq t2(getpoint "\nУкажи точку #2: "))
  (initget)(setq t3(getpoint "\nУкажи точку #3: "))
  (setq a12 (angle t1 t2))
  (setq a13 (angle t1 t3))
  (setq a21 (angle t2 t1))
  (setq a23 (angle t2 t3))
  (setq a31 (angle t3 t1))
  (setq a32 (angle t3 t2))
  (setq r1 (+ (/ (- a12 a13) 2) a12))
  (setq r2 (+ (/ (- a23 a21) 2) a21))
  (setq r3 (+ (/ (- a32 a31) 2) a31))
  (setq t11 (polar t1 r1 2))
  (setq t22 (polar t2 r2 2))
  (setq t33 (polar t3 r3 2))
  (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t1) (append '(11) t11)))
  (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t2) (append '(11) t22)))
  (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t3) (append '(11) t33)))
  (princ)
  )




	(if (eq(ang3points t4 t2 t1) pi)
	  (list
	    '(t1 (angle t1 t2) 0 3)
	    '(t1 (angle t2 t1) 2 0)
	    '(t1 (angle t1 t5) 0 0)
	    '(t1 (angle t5 t1) 2 3)
	    
	    '(t2 (angle t1 t2) 2 3)
	    '(t2 (angle t2 t1) 0 0)
	    '(t2 (angle t1 t5) 0 3)
	    '(t2 (angle t5 t1) 2 0)
	    
	    '(t4 (angle t1 t2) 2 0)
	    '(t4 (angle t2 t1) 0 3)
	    '(t4 (angle t1 t5) 2 3)
	    '(t4 (angle t5 t1) 0 0)
	    
	    '(t5 (angle t1 t2) 0 0)
	    '(t5 (angle t2 t1) 2 3)
	    '(t5 (angle t1 t5) 2 0)
	    '(t5 (angle t5 t1) 0 3)
	    )
	  (list
	    '(t1 (angle t1 t2) 0 0)
	    '(t1 (angle t2 t1) 2 3)
	    '(t1 (angle t1 t5) 0 3)
	    '(t1 (angle t5 t1) 2 0)
	    
	    '(t2 (angle t1 t2) 2 0)
	    '(t2 (angle t2 t1) 0 3)
	    '(t2 (angle t1 t5) 0 0)
	    '(t2 (angle t5 t1) 2 3)
	    
	    '(t4 (angle t1 t2) 2 3)
	    '(t4 (angle t2 t1) 0 0)
	    '(t4 (angle t1 t5) 2 0)
	    '(t4 (angle t5 t1) 0 3)
	    
	    '(t5 (angle t1 t2) 0 3)
	    '(t5 (angle t2 t1) 2 0)
	    '(t5 (angle t1 t5) 2 3)
	    '(t5 (angle t5 t1) 0 0)
	    )
	  )

(defun C:probas ()
  (initget 1)(setq p1(getpoint "\nУкажи первую точку:")); указываю точку
  (initget 1)(setq p2(getpoint p1 "\nУкажи вторую точку:")); указываю вторую точку
  (entmake(list
	    '(0 . "LWPOLYLINE")
	    '(100 . "AcDbEntity")
	    '(100 . "AcDbPolyline")
	    (cons 90 2)
	    (cons 43 0.0)
	    (cons 70 0)
	    (cons 10 p1)
	    (cons 10 p2)
	    )
	  )
  (setq pline (entlast))
  (entupd pline)
  (initget 1)(setq p3(getpoint p2"\nУкажи следующую точку:"))
  (while (not(eq p3 nil))
    (setq dxf (entget pline))
    (setq dxf (subst (cons 90 (1+ (cdr(assoc 90 dxf))))(assoc 90 dxf) dxf))
    (entmod (append dxf (list (cons 10 p3))))
    (entupd pline)
    (initget)(setq p3(getpoint p3"\nУкажи следующую точку:"))
    )
  )


(defun C:proba ()
  (begin_activex)
  (setq C3D_points (vlax-get C3D_active_document 'points))
  (setq objekt (car (entsel)))
  (setq objekt_vla (vlax-ename->vla-object objekt))
  (setq selectedPnts (vlax-invoke C3D_active_document 'SelectPoints))
  (princ selectedPnts)
  (setq elev (vla-get elevation objekt_vla))
  
  (setq elev
	 (vlax-get objekt_vla
	   (vlax-invoke C3D_points 'find (car selectedPnts))
		   'elevation)
	)
  (princ elev)
  )
  (vla-get-StyleName objekt_vla "Подписи")
  
  (setq C3D_points (vlax-get C3D_active_document 'points))
  (setq elev (vlax-get (vlax-invoke C3D_points 'find objekt) 'elevation))
  (princ (strcat "\nElevation of Pt#" " = " (rtos elev 2 2)))

(defun C:proba ()
  (begin_activex)
	 (setq C3D_points (vlax-get C3D_active_document 'points))
	 (setq list_num_points (vlax-invoke C3D_active_document 'SelectPoints))
	 (setq list_add_z nil)
	 (foreach num_point list_num_points
	   (setq list_add_z(append list_add_z (list(vlax-get (vlax-invoke C3D_points 'find num_point) 'elevation))))
	   )
  (princ list_add_z)
  (princ)
  )
(defun C:proba (/ list_add_z)
  (begin_activex)
  (setq nabor (ssget '((0 . "TEXT"))))
  (setq n 0)
  (repeat (sslength nabor)
    (setq list_add_z (append list_add_z (list(atof(cdr(assoc 1(entget(ssname nabor n))))))))
    (setq n (1+ n))
		    )
  (princ list_add_z)
  (princ)
  )GetLeaderIndex

(defun C:proba ()
  (begin_activex)
  (setq objekt (car (entsel)))
  (setq objekt_vla (vlax-ename->vla-object objekt))
  (vlax-invoke-method objekt_vla 'GetLeaderIndex )
  )

(defun C:probka ()
  (initget)(setq n(getreal(strcat "Укажи число: ")))
  (while (< n 100)
    (princ n)
    (princ "\n")
    (setq n (1+ n))
    )
  )




(defun C:num ( / t1 t2 t3 t4 t5 c1 d longa long str tx1 ttt txy t6 number t10 t11 h)
  ;Отрисовка здания, буквенного индекса и номера
  
  (terpri)
  (checkstyle)
  (setq d(/ 1 (getvar "CANNOSCALEVALUE")))
  (initget)(setq t1(getpoint "Укажи первую точку:"))(terpri); указываю точку
  (initget)(setq t2(getpoint t1 "Укажи вторую точку:"))(terpri); указываю вторую точку
  (entmake (list '(0 . "LINE") '(6 . "Continuous") '(8 . "Ситуация") (cons 10 t1) (cons 11 t2)))
  (initget)(setq t3(getpoint t2 "Укажи третью точку:"))(terpri); указываю третью точку
  (setq с1(+(angle t2 t1) (* 90 (/ pi 180)))); вычисляю угол на t4
  (setq d(*(distance t2 t3) (cos (-(* 180 (/ pi 180)) (- (- (angle t2 t1)(angle t2 t3)) (* 90 (/ pi 180))))))); вычисляю расстояние на t4
  (setq t4(polar t2 с1 d)); вычисляю координаты на t4
  (setq t5(polar t4 (angle t2 t1) (distance t1 t2))); вычисляю координаты на t5
  (entmake (list '(0 . "LINE") '(6 . "Continuous") '(8 . "Ситуация") (cons 10 t2) (cons 11 t4)))
  (entmake (list '(0 . "LINE") '(6 . "Continuous") '(8 . "Ситуация") (cons 10 t4) (cons 11 t5)))
  (entmake (list '(0 . "LINE") '(6 . "Continuous") '(8 . "Ситуация") (cons 10 t5) (cons 11 t1)))
  (initget)(setq str (getstring nil "Введи характеристику здания: "))
  (setq t6
	 (polar
	   (polar t1 (angle t1 t2) (/ (distance t1 t2) 2))
	   (angle t1 t5)
	   (/ (distance t1 t5) 2)
	   ); polar
	); setq t6
  (if (> (distance t1 t2)
	 (distance t1 t5)
	 ); конец условия
    (progn
      (setq longa (angle t1 t2))
      (setq shotd (distance t1 t5))
      ); progn
    (progn
      (setq longa (angle t1 t5))
      (setq shotd (distance t1 t2))
      ); progn
    ); if
  (setq long (aglong longa))
  (if
    (>
      (+ (/ 1 (getvar "CANNOSCALEVALUE"))(/ (/ 1.6 (getvar "CANNOSCALEVALUE")) 1.07372))
      shotd); условие
    (progn
      (setq h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.07372))
      (setq long 0)
      (princ "Подпись не входит в контур, попробуй разместить рядом.")
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
  (setq txy (textxy t6 long ttt))
  (entmake (list '(0 . "TEXT") (cons 10 txy) (cons 40 h) (cons 1 str) (cons 50 long) '(7 . "OTI")))
  (initget)(setq number (getstring nil "Введи номер дома: "))(terpri)
  (initget)(setq t10 (getpoint "Укажи угол для номера:"))(terpri)
  (setq text (list '(0 . "TEXT") (cons 10 t10) (cons 40 (/ 2 (getvar "CANNOSCALEVALUE"))) (cons 1 number) (cons 50 long) '(7 . "OTI")))
  (setq t11 (point11 long text t1 t2 t4 t5))
  (entmake (list '(0 . "TEXT") (cons 10 t11) (cons 40 (/ 2 (getvar "CANNOSCALEVALUE"))) (cons 1 number) (cons 50 long) '(7 . "OTI")))
  (setq textred (entlast))
  (initget 6 "Да Нет") (setq povorot (getkword "Повернуть номер дома? [Да/Нет] <Нет>:"))
  (if (= povorot "Да")
    (progn
  (if (< (distance t1 t2)
	 (distance t1 t5)
	 ); конец условия
    (progn
      (setq longa (angle t1 t2))
      ); progn
    (progn
      (setq longa (angle t1 t5))
      ); progn
    ); if
  (setq long (aglong longa))
  (setq text (list '(0 . "TEXT") (cons 10 t10) (cons 40 (/ 2 (getvar "CANNOSCALEVALUE"))) (cons 1 number) (cons 50 long) '(7 . "OTI")))
(setq t11 (point11 long text t1 t2 t4 t5))
(entmod (list (cons -1 textred) (cons 10 t11) (cons 40 (/ 2 (getvar "CANNOSCALEVALUE"))) (cons 1 number) (cons 50 long) '(7 . "OTI")))
  (entupd textred)
     ); Да
    ); if
  (princ)
  ); defun C:num

(defun C:domprm
       ( / t1 t2 t3 t4 t5 c1 d longa long str tx1 ttt txy t6 h)
  ; рисует прямоугольник по трем точкам, подписывает характеристику Рубленым
  (terpri)
  (checkstyle)
 (setq d(/ 1 (getvar "CANNOSCALEVALUE")))
 (initget)(setq t1(getpoint "Укажи первую точку:"))(terpri); указываю точку
 (initget)(setq t2(getpoint t1 "Укажи вторую точку:"))(terpri); указываю вторую точку
 (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t1) (append '(11) t2)))
 (initget)(setq t3(getpoint t2 "Укажи третью точку:"))(terpri); указываю третью точку
 (setq с1(+(angle t2 t1) (* 90 (/ pi 180)))); вычисляю угол на t4
 (setq d(*(distance t2 t3) (cos (-(* 180 (/ pi 180)) (- (- (angle t2 t1)(angle t2 t3)) (* 90 (/ pi 180))))))); вычисляю расстояние на t4
 (setq t4(polar t2 с1 d)); вычисляю координаты на t4
 (setq t5(polar t4 (angle t2 t1) (distance t1 t2))); вычисляю координаты на t5
  (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t2) (append '(11) t4)))
  (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t4) (append '(11) t5)))
  (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t5) (append '(11) t1)))
  (initget)(setq str (getstring nil "Введи характеристику здания: "))
  (setq t6
	 (polar
	   (polar t1 (angle t1 t2) (/ (distance t1 t2) 2))
	   (angle t1 t5)
	   (/ (distance t1 t5) 2)
	   ); polar
	); setq t6
  (if (> (distance t1 t2)
	 (distance t1 t5)
	 ); конец условия
    (progn
      (setq longa (angle t1 t2))
      (setq shotd (distance t1 t5))
      ); progn
    (progn
      (setq longa (angle t1 t5))
      (setq shotd (distance t1 t2))
      ); progn
    ); if
  (setq long (aglong longa))
  (if
    (>
      (+ (/ 1 (getvar "CANNOSCALEVALUE"))(/ (/ 1.6 (getvar "CANNOSCALEVALUE")) 1.07372))
      shotd); условие
    (progn
      (setq h (/ (/ 3 (getvar "CANNOSCALEVALUE")) 1.07372))
      (setq long 0)
      (princ "Подпись не входит в контур, попробуй разместить рядом.")
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
  (setq txy (textxy t6 long ttt))
  (entmake (list '(0 . "TEXT") (cons 10 txy) (cons 40 h) (cons 1 str) (cons 50 long) '(7 . "OTI")))
  (princ)
  ); defun domprm


(defun C:install ( / Support_Paths acad_Path QNewTemplate_File ToolPalette_Path x file)
  (vl-load-com)
  (setq Support_Paths (vla-get-SupportPath  (vla-get-files (vla-get-Preferences (vlax-get-acad-object)))))
  (setq acad_Path (vla-get-Path (vlax-get-acad-object)))
  (mapcar '(lambda (x)
	     (if (not(vl-string-search (strcat acad_Path x)Support_Paths))
	       (setq Support_Paths (strcat Support_Paths ";" acad_Path x))
	       )
	     )
	  (list "\\MY AutoCad"
	    "\\MY AutoCad\\Fonts"
	    "\\MY AutoCad\\LISP"
	    "\\MY AutoCad\\Блоки"
	    )
	  )
  (vla-put-SupportPath  (vla-get-files (vla-get-Preferences (vlax-get-acad-object))) Support_Paths)
  
  (setq QNewTemplate_File (vla-get-QNewTemplateFile  (vla-get-files (vla-get-Preferences (vlax-get-acad-object)))))
  (setq ToolPalette_Path (vla-get-ToolPalettePath  (vla-get-files (vla-get-Preferences (vlax-get-acad-object)))))
  (setq file  (open (strcat acad_Path "\\MY AutoCad\\uninstall.dat") "w"))
  (write-line QNewTemplate_File file)
  (write-line ToolPalette_Path file)
  (close file)
  (if (not(vl-string-search
	    (strcat acad_Path "\\MY AutoCad\\_AutoCAD Civil 3D (Metric) Russian Geodesy.dwt")QNewTemplate_File))
    (setq QNewTemplate_File (strcat acad_Path "\\MY AutoCad\\_AutoCAD Civil 3D (Metric) Russian Geodesy.dwt"))
    )
  (vla-put-QNewTemplateFile  (vla-get-files (vla-get-Preferences (vlax-get-acad-object))) QNewTemplate_File)
  
  (if (not(vl-string-search
	    (strcat acad_Path "\\MY AutoCad\\ToolPalette")ToolPalette_Path))
	       (setq ToolPalette_Path (strcat acad_Path "\\MY AutoCad\\ToolPalette"))
	       )
  (vla-put-ToolPalettePath  (vla-get-files (vla-get-Preferences (vlax-get-acad-object))) ToolPalette_Path)
  
  )

(defun C:uninstall ( / Support_Paths acad_Path QNewTemplate_File ToolPalette_Path x file)
  (vl-load-com)
  (setq Support_Paths (vla-get-SupportPath  (vla-get-files (vla-get-Preferences (vlax-get-acad-object)))))
  (setq acad_Path (vla-get-Path (vlax-get-acad-object)))
  (mapcar '(lambda (x)
	     (if (vl-string-search (strcat acad_Path x)Support_Paths)
	       (progn
		 (setq n (vl-string-search (strcat ";" acad_Path x)Support_Paths))
		 (strlen ";" acad_Path x)
		 (setq Support_Paths (strcat (substr Support_Paths 1 n)(substr Support_Paths (+ (1+ n) (strlen ";" acad_Path x)) (strlen Support_Paths))))
		 )
	       )
	     )
	  (list "\\MY AutoCad"
		"\\MY AutoCad\\Fonts"
		"\\MY AutoCad\\LISP"
		"\\MY AutoCad\\Блоки"
		)
	  )
  (vla-put-SupportPath  (vla-get-files (vla-get-Preferences (vlax-get-acad-object))) Support_Paths)
  (setq file  (open (strcat acad_Path "\\MY AutoCad\\uninstall.dat") "r"))
  (setq QNewTemplate_File (read-line file))
  (setq ToolPalette_Path (read-line file))
  (close file)
  (vl-file-delete (strcat acad_Path "\\MY AutoCad\\uninstall.dat"))
  (setq QNewTemplate_File "C:\\Documents and Settings\\Ascetic\\Local Settings\\Application Data\\Autodesk\\C3D 2008\\rus\\Template\\acadiso.dwt")
  (vla-put-QNewTemplateFile  (vla-get-files (vla-get-Preferences (vlax-get-acad-object))) QNewTemplate_File)
  (vlax-dump-object (vla-get-ActiveViewport(vla-get-ActiveDocument(vlax-get-acad-object)))T)
  (vlax-dump-object (vla-get-ActiveDocument(vlax-get-acad-object))T)
  (setq ToolPalette_Path "C:\\Documents and Settings\\Ascetic\\Application Data\\Autodesk\\C3D 2008\\rus\\Support\\ToolPalette")
  (vla-put-ToolPalettePath  (vla-get-files (vla-get-Preferences (vlax-get-acad-object))) ToolPalette_Path)
  (vlax-dump-object (vlax-get-acad-object)T)
  )(vlax-dump-object (vla-get-Display(vla-get-Preferences acad_application))T)(vla-get-Layouts active_document)
(vlax-dump-object (vla-get-Output(vla-get-Preferences (vlax-get-acad-object))))
(vlax-dump-object(vla-get-files (vla-get-Preferences (vlax-get-acad-object))))
(vlax-dump-object(vla-get-Documents (vlax-get-acad-object)))
(vlax-dump-object(vlax-get-acad-object))
  (vla-put-SupportPath  (vla-get-files (vla-get-Preferences (vlax-get-acad-object))) Support_Paths)
  (vla-put-QNewTemplateFile  (vla-get-files (vla-get-Preferences (vlax-get-acad-object))) QNewTemplate_File)
  (vla-put-ToolPalettePath  (vla-get-files (vla-get-Preferences (vlax-get-acad-object))) ToolPalette_Path)
  "C:\\Documents and Settings\\Ascetic\\Local Settings\\Application Data\\Autodesk\\C3D 2008\\rus\\Template\\acadiso.dwt"
  "C:\\Documents and Settings\\Ascetic\\Application Data\\Autodesk\\C3D 2008\\rus\\Support\\ToolPalette"
  (setq QNewTemplate_File (vla-get-QNewTemplateFile  (vla-get-files (vla-get-Preferences (vlax-get-acad-object)))))
  (setq ToolPalette_Path (vla-get-ToolPalettePath  (vla-get-files (vla-get-Preferences (vlax-get-acad-object)))))
(vlax-dump-object (vla-get-Layouts (vla-get-ActiveDocument(vlax-get-acad-object)))T)
vla-addpviewport(vlax-dump-object (vla-get-TextStyles (vla-get-ActiveDocument(vlax-get-acad-object)))T)
(vlax-dump-object (vla-get-ActiveDocument(vlax-get-acad-object))T)
(vlax-dump-object (vlax-get-acad-object)T)
(vlax-dump-object (vla-get-ActiveViewport(vla-get-ActiveDocument(vlax-get-acad-object)))T)
(vlax-dump-object (vla-get-Layers(vla-get-ActiveDocument(vlax-get-acad-object)))T)
(vlax-dump-object (vla-GetExtensionDictionary(vla-get-Layers(vla-get-ActiveDocument(vlax-get-acad-object))))T)
(vlax-dump-object(vla-get-Blocks(vla-get-ActiveDocument(vlax-get-acad-object)))T)
(vlax-dump-object (vla-get-PaperSpace(vla-get-ActiveDocument(vlax-get-acad-object)))T)
(vlax-dump-object (vla-get-Layouts(vla-get-ActiveDocument(vlax-get-acad-object)))T)
(vlax-dump-object(vla-item(vla-get-Blocks(vla-get-ActiveDocument(vlax-get-acad-object))) 130 )T)
(vlax-dump-object (vla-get-Views(vla-get-ActiveDocument(vlax-get-acad-object)))T)
(vlax-dump-object (vla-get-Preferences (vlax-get-acad-object))T)
(vlax-dump-object (vla-get-Linetypes(vla-get-ActiveDocument(vlax-get-acad-object)))T)
(vlax-dump-object (vla-get-files (vla-get-Preferences (vlax-get-acad-object)))T)
(vlax-dump-object (vla-get-files (vla-get-Preferences (vlax-get-acad-object)))T)
(defun C:tt ( / pt)
  (setvar "TILEMODE" 0)
  (if (= (getvar "CVPORT") 2)
    (progn (setq pt (getpoint "\Select Point"))
      (print (trans pt 2 3)))
    (alert "\Go into Viewport's Model Space you Idiot and Start Over!"))
  );end
(defun TT (pt / cp cpl x0 x1 y0 y1)
  (setq	adoc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (= (vla-get-ActiveSpace adoc) 0)
    (setq vp (vla-get-ActivePViewport adoc)
          ptl (trans pt 2 3));setq
    (progn (vlax-for vp (vla-get-paperSpace adoc)
        (if (= (vla-get-ObjectName vp) "AcDbViewport")
	  (progn
            (setq cp (cdr (assoc 12 (entget (vlax-vla-object->ename vp))))
                  cpl (vla-get-Center vp)
		  x0 (- (car cp) (/ (vla-get-Width vp) (vla-get-CustomScale vp) 2))
                  x1 (+ (car cp) (/ (vla-get-Width vp) (vla-get-CustomScale vp) 2))
                  y0 (- (cadr cp) (/ (vla-get-Height vp) (vla-get-CustomScale vp) 2))
                  y1 (+ (cadr cp) (/ (vla-get-Height vp) (vla-get-CustomScale vp) 2)));setq
            (if (and (<= x0 (car pt) x1) (<= y0 (cadr pt) y1))
              (setq ptl (mapcar '(lambda (x y z) (+ x (* (- y z) (vla-get-CustomScale vp))))
                          (list cpl cp cpt))));if
              ));if
      ));progn
  );if
);end

(defun vla_AddText (spece str p h alig / text); создает текст
    (begin_activex)
    (setq text (vla-AddText spece str (vlax-3D-point p) h))
    (vla-put-Alignment text alig)
    (vla-put-TextAlignmentPoint text (vlax-3D-point p))
    )

Рисует полилинию по границам ВЭ в модели
ViewPort Outline
(defun C:VPO (/ adoc ss lst)
  (vl-load-com)
  (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
  (if (= (getvar "tilemode") 0)
    (progn
      (if (= (getvar "cvport") 1)
        (progn
          (if (setq ss nil
                    ss (ssget '((0 . "VIEWPORT")))
              ) ;_ end of setq
            (setq lst (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
            (setq lst nil)
          ) ;_ end of if
        ) ;_ end of progn
        (setq lst
               (list
                 (vlax-vla-object->ename (vla-get-activepviewport adoc))
               ) ;_ end of list
        ) ;_ end of setq
      ) ;_ end of if
      (mapcar
        '(lambda (y / x ret)
           (setq x (entget y))
           (if (cdr (assoc 340 x))
             (setq ret (mapcar 'cdr
                               (vl-remove-if-not
                                 '(lambda (b) (= (car b) 10))
                                 (entget (cdr (assoc 340 x)))
                               ) ;_ end of vl-remove-if-not
                       ) ;_ end of mapcar
             ) ;_ end of setq
             (setq ret
                    (list
                      (list
                        (- (cadr (assoc 10 x)) (/ (cdr (assoc 40 x)) 2.))
                        (- (caddr (assoc 10 x))
                           (/ (cdr (assoc 41 x)) 2.)
                        ) ;_ end of -
                      ) ;_ end of list
                      (list
                        (+ (cadr (assoc 10 x)) (/ (cdr (assoc 40 x)) 2.))
                        (- (caddr (assoc 10 x))
                           (/ (cdr (assoc 41 x)) 2.)
                        ) ;_ end of -
                      ) ;_ end of list
                      (list
                        (+ (cadr (assoc 10 x)) (/ (cdr (assoc 40 x)) 2.))
                        (+ (caddr (assoc 10 x))
                           (/ (cdr (assoc 41 x)) 2.)
                        ) ;_ end of +
                      ) ;_ end of list
                      (list
                        (- (cadr (assoc 10 x)) (/ (cdr (assoc 40 x)) 2.))
                        (+ (caddr (assoc 10 x))
                           (/ (cdr (assoc 41 x)) 2.)
                        ) ;_ end of +
                      ) ;_ end of list
                    ) ;_ end of list
             ) ;_ end of setq
           ) ;_ end of if
           (vla-display (vlax-ename->vla-object y) :vlax-true) ;_Вклбючаем показ ВЭ
           (vla-put-mspace adoc :vlax-true) ;_Переходим в модель. Аналог  (command "_.mspace")
           (setvar "cvport" (cdr (assoc 69 x)))
           (setq ret (mapcar '(lambda (pt)
                                (setq pt (trans pt 3 2)
                                      pt (trans pt 2 1)
                                      pt (trans pt 1 0)
                                ) ;_ end of setq
                              ) ;_ end of lambda
                             ret
                     ) ;_ end of mapcar
           ) ;_ end of setq
           (setq ret (mapcar '(lambda (zz) (mapcar '+ zz '(0 0))) ret)) ;_Удаляем координату Z
           (setq ret (apply 'append ret))
           (vla-put-closed
             (vla-addlightweightpolyline
               (vla-get-modelspace adoc)
               (vlax-make-variant
                 (vlax-safearray-fill
                   (vlax-make-safearray
                     vlax-vbdouble
                     (cons 0 (- (length ret) 1))
                   ) ;_ end of vlax-Make-SafeArray
                   ret
                 ) ;_ end of vlax-SafeArray-Fill
               ) ;_ end of vlax-Make-Variant
             ) ;_ end of vla-AddLightWeightPolyline
             :vlax-true
           ) ;_ end of vla-Put-Closed
         ) ;_ end of lambda
        lst
      ) ;_ end of mapcar
      (if ss
        (vla-put-mspace ad :vlax-false)
      ) ;_ Переходим в лист. Аналог (command "._pspace"))
      (setq ss nil)
    ) ;_ end of progn
    (alert "Перейди в лист")
  ) ;_ end of if
) ;_ end of defun

(defun get_mg (/ mg_ file) ; ввод горизонтального масштаба и запись его в файл
  (setq file (open
	       (strcat(cond
			((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
			((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"mg.txt")
	       "r")
	)
  (if
    (= file nil)
    (progn
      (setq file (open (strcat(cond
				((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
				((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"mg.txt")
		       "w"))
      (write-line "500" file)
      (close file)
      (setq file (open (strcat(cond
				((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
				((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"mg.txt")
		       "r"))
      )
    )
  (setq mg (atof(read-line file)))
  (close file)
  (setq mg_ mg)
  (initget)
  (setq mg (getreal (strcat "\nВведи числитель горизонтального масштаба <1:"(rtos mg 2 0)"> 1:")))
  (if (eq mg nil)
    (setq mg mg_)
    )
  (setq file (open (strcat(cond
			    ((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
			    ((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"mg.txt") "w"))
  (write-line (rtos mg 2 0) file)
  (close file)
  mg
  )
(defun get_mg_r (/ file) ; чтение горизонтального масштаба из файла
  (setq file (open (strcat(cond
			    ((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
			    ((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"mg.txt") "r"))
  (if
    (= file nil)
    (progn
      (setq file (open (strcat(cond
				((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
				((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"mg.txt") "w"))
      (write-line "500" file)
      (close file)
      (setq file (open (strcat(cond
				((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
				((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"mg.txt") "r"))
      )
    )
  (setq mg (atof(read-line file)))
  (close file)
  mg
  )

(defun set_mg (mv / file); запись горизонтального масштаба в файл
  (setq file (open (strcat(cond
			    ((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
			    ((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"mg.txt") "w"))
  (write-line (rtos mg 2 0) file)
  (close file)
  )

(defun get_m (/ m_ file); ввод вертикального масштаба и запись его в файл
  (setq file (open (strcat(cond
			    ((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
			    ((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"m.txt") "r"))
  (if
    (= file nil)
    (progn
      (setq file (open (strcat(cond
				((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
				((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"m.txt") "w"))
      (write-line "100" file)
      (close file)
      (setq file (open (strcat(cond
				((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
				((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"m.txt") "r"))
      )
    )
  (setq mv (atof(read-line file)))
  (close file)
  (setq m_ mv)
  (initget)
  (setq mv (getreal (strcat "\nВведи числитель вертикального масштаба <1:"(rtos mv 2 0)"> 1:")))
  (if (eq mv nil)
    (setq mv m_)
    )
  (setq file (open (strcat(cond
			    ((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
			    ((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"m.txt") "w"))
  (write-line (rtos mv 2 0) file)
  (close file)
  mv
  )

(defun get_m_r (/ file) ; чтение вертикального масштаба из файла
  (setq file (open (strcat(cond
			    ((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
			    ((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"m.txt") "r"))
  (if
    (= file nil)
    (progn
      (setq file (open (strcat(cond
				((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
				((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"m.txt") "w"))
      (write-line "100" file)
      (close file)
      (setq file (open (strcat(cond
				((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
				((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"m.txt") "r"))
      )
    )
  (setq mv (atof(read-line file)))
  (close file)
  mv
  )

(defun set_m (mv / file); запись вертикального масштаба в файл
  (setq file (open (strcat(cond
			((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
			((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"m.txt") "w"))
  (write-line (rtos mv 2 0) file)
  (close file)
  )

(defun get_usl_z (/ usl_z_); ввод отметки условного горизонта и запись его в файл
  (setq file (open (strcat(cond
			    ((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
			    ((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"usl_z.txt") "r"))
  (if
    (= file nil)
    (progn
      (setq file (open (strcat(cond
				((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
				((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"usl_z.txt") "w"))
      (write-line "100" file)
      (close file)
      (setq file (open (strcat(cond
				((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
				((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"usl_z.txt") "r"))
      )
    )
  (setq usl_z (atof(read-line file)))
  (close file)
  (setq usl_z_ usl_z)
  (initget)
  (setq usl_z (getreal (strcat "\nВведи отметку условного горизонта <"(rtos usl_z 2 2)">: ")))
  (if (eq usl_z nil)
    (setq usl_z usl_z_)
    )
  (setq file (open (strcat(cond
			    ((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
			    ((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"usl_z.txt") "w"))
  (write-line (rtos usl_z 2 2) file)
  (close file)
  usl_z
  )

(defun get_usl_z_r (/ file); чтение отметки условного горизонта из файла
  (setq file (open (strcat(cond
			    ((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
			    ((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"usl_z.txt") "r"))
  (if
    (= file nil)
    (progn
      (setq file (open (strcat(cond
				((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
				((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"usl_z.txt") "w"))
      (write-line "100" file)
      (close file)
      (setq file (open (strcat(cond
				((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
				((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"usl_z.txt") "r"))
      )
    )
  (setq usl_z (atof(read-line file)))
  (close file)
  usl_z
  )

(defun set_usl_z (usl_z / file); запись отметки условного горизонта в файл
  (setq file (open (strcat(cond
			    ((eq (getvar "ACADVER") "17.1s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2008\\MY AutoCad\\LISP\\")
			    ((eq (getvar "ACADVER") "17.2s (LMS Tech)")"C:\\Program Files\\AutoCAD Civil 3D 2009\\MY AutoCad\\LISP\\"))"usl_z.txt") "w"))
  (write-line (rtos usl_z 2 2) file)
  (close file)
  )




(command"mline" "st" "linestylename" "")
А если стили мультилиний содержаться не в файле ACAD.MLN :?: :shock: :D :D :shock: Тогда вот так, создать их:
(if (= nil (member (cons 3 "SC_06_CAB") ;- если нет нужного стиля мультилинии
		   (dictsearch (namedobjdict) "ACAD_MLINESTYLE"))) ;- в словаре типов мультилиний
  (progn
    (setq mlDict (cdr(assoc -1(dictsearch (namedobjdict) "ACAD_MLINESTYLE"))))
    (setq mlList '((0 . "MLINESTYLE")(102 ."{ACAD_REACTORS")(102 . "}") (100 . "AcDbMlineStyle") (2 . "SC_06_CAB") (70 . 0) (3 . "")
		   (62 . 256) (51 . 1.5708) (52 . 1.5708) (71 . 6) (49 . 200.0) (62 . 256)
		   (6 ."BYLAYER") (49 . 120.0) (62 . 256) (6 . "BYLAYER") (49 . 40.0) (62 . 256)
		   (6 ."BYLAYER") (49 . -40.0) (62 . 256) (6 . "BYLAYER") (49 . -120.0) (62 . 256)
		   (6 . "BYLAYER") (49 . -200.0) (62 . 256) (6 . "BYLAYER"))); end setq
    (dictadd mlDict "SC_06_CAB" (entmakex mlList))
    ); end progn
  ); end if
А если грузить из другого файла, о чем в общем то и был вопрос :shock: Тогда примерно так:
(defun load_mlinestyle (flnm stname reload / *error* lst_member answer fl fl2 ckl strmus strmus2 lst_style)
  ;flnm - имя файла стиля мультилинии
  ;stname - имя стиля мультилинии
  ;reload - если не nil, то перезагрузка стиля
  (defun *error* (msg)
    (vl-catch-all-apply 'close (list fl2))
    (princ (strcat "\nОшибка в файле стиля мультилинии " flnm))
    );defun
  (if (or (not (setq lst_member (member (cons 3 stname) (setq answer (dictsearch (namedobjdict) "ACAD_MLINESTYLE")))))
	  reload
	  );or
    (if (and (setq fl (findfile flnm))
	     (setq fl2 (open fl "r"))
	     );and
      (progn
	(read-line fl2)
	(setq ckl t)
	(while (and ckl
		    (setq strmus (read-line fl2))
		    (setq strmus2 (read-line fl2))
		    );and
	  (if (and (= (vl-string-trim " " strmus) "2")
		   (= (vl-string-trim " " strmus2) stname)
		   );and
	    (setq ckl nil)
	    );if
	  );while
	(while (and (setq strmus (read-line fl2))
		    (setq strmus2 (read-line fl2))
		    (/= (setq strmus (vl-string-trim " " strmus)) "0")
		    );and
	  (setq lst_style (cons
			    (cond
			      ((or (= strmus "3") (= strmus "6"))
			       (cons (atoi strmus) (vl-string-trim " " strmus2))
			       )
			      ((or (= strmus "51") (= strmus "52"))
			       (cons (atoi strmus) (* (atof (vl-string-trim " " strmus2)) (/ pi 180.0)))
			       )
			      (t
			       (read (strcat "(" strmus " . " (vl-string-trim " " strmus2) ")"))
			       )
			      );cond
			    lst_style))
	  );while
	(close fl2)
	(if lst_style
	  (progn
	    (setq lst_style (append
			      (list
				'(0 . "MLINESTYLE")
				(cons 330 (cdr (assoc -1 answer)))
				'(100 . "AcDbMlineStyle")
				(cons 2 stname))
			      (reverse lst_style)))
	    (if lst_member
	      (entmod (subst (cons 350 (entmakex lst_style)) (cadr lst_member) answer))
	      (entmod (append answer (list (cons 3 stname) (cons 350 (entmakex lst_style)))))
	      );if
	    );progn
	  );if
	);progn
      );if
    answer
    );if
  );defun



(defun C:probbba ( / nabor n t1 vis shir text str)
  (setq nabor(ssget))
  (setq n 0)
  (setq str "")
  (repeat (sslength nabor)
    (setq text (ssname nabor n))
    (setq t1 (cdr (assoc 10 (entget text))))
    (setq vis (cdr (assoc 40 (entget text))))
    (setq shir (cdr (assoc 41 (entget text))))
    (setq num (cdr (assoc 1 (entget text))))
    (setq str (strcat str" (list (nth (1- "
		       num ") list_cell)"
		      (rtos vis 2 0)" "
		      (rtos shir 2 0)
		      " '("
		      (rtos(nth 0 t1)2 3)" "
		      (rtos(nth 1 t1)2 3)" "
		      (rtos(nth 2 t1)2 3)")5)"
		      )
	  )
    (setq n (1+ n))
    )
  str
  )

(defun C:probka ( / list_Layouts)
  (setq list_Layouts(list "Вид экрана" "Вспомогательный" "Геология" "Горизонталь Вспомогательная" "Горизонталь Основная" "Лишние пикеты"
			  "Пикеты" "Поверхность" "Сводка" "Сетка" "Ситуация" "Формат"))
  (repeat (length list_Layouts)
    (princ(entget(tblobjname "layer" (car list_Layouts))))
    (princ "\n")
    (setq list_Layouts (cdr list_Layouts))
    )
  )

(defun C:m500 ()
   (command "_style" "OTI" "T132.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "D432.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "P131.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "DO431.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "BM431.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "D431.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "P151.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "ESKDU.shx" "0" "1" "0" "_n" "_n")
   (setvar "LTSCALE" 1.0)
   (setvar "CANNOSCALE" "1:500")
   (setvar "LUNITS" 2)
   (setvar "LUPREC" 3)
   (setvar "AUNITS" 1)
   (setvar "AUPREC" 4)
   (setvar "ANGDIR" 1)
   (setvar "ANGBASE" (/ pi 2))
   (command "_regenall")
   )

 (defun C:m1000 ()
   (command "_style" "OTI" "T132.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "D432.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "P131.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "DO431.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "BM431.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "D431.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "P151.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "ESKDU.shx" "0" "1" "0" "_n" "_n")
   (setvar "LTSCALE" 1.0)
   (setvar "CANNOSCALE" "1:1000")
   (setvar "LUNITS" 2)
   (setvar "LUPREC" 3)
   (setvar "AUNITS" 1)
   (setvar "AUPREC" 4)
   (setvar "ANGDIR" 1)
   (setvar "ANGBASE" (/ pi 2))
   (command "_regenall")
 )

 (defun C:m2000 ()
   (command "_style" "OTI" "T132.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "D432.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "P131.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "DO431.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "BM431.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "D431.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "P151.TTF" "0" "1" "0" "_n" "_n")
   (command "_style" "OTI" "ESKDU.shx" "0" "1" "0" "_n" "_n")
   (setvar "LTSCALE" 1.0)
   (setvar "CANNOSCALE" "1:2000")
   (setvar "LUNITS" 2)
   (setvar "LUPREC" 3)
   (setvar "AUNITS" 1)
   (setvar "AUPREC" 4)
   (setvar "ANGDIR" 1)
   (setvar "ANGBASE" (/ pi 2))
 (command "_regenall")
 )

(defun C:katkoor  ; чертит таблицу координат указываемых точек
       (/ pr1 pr2 q t0  t1 d num_xy d_xy x_xy y_xy t10 t11 t12 t13 t14 t01 t02 t03 t04 N_xy pp_xy t1_xy t2_xy tx_xy ty_xy dcl_id styl osn)
  (begin_activex)
  (setq dcl_id (load_dialog "Диалоговые окна.dcl"))
  (new_dialog "t_koor" dcl_id)
  (setq pr1 0)
  (setq pr2 0)
  (setq num 1)
  (action_tile "pr1" "(setq pr1 1)")
  (action_tile "pr2" "(setq pr2 1)")
  (action_tile "n" "(setq num (get_tile \"n\"))")
  (setq qit (start_dialog))
  (unload_dialog dcl_id)
  (if (eq qit 0) (exit))
  (if
    (not (or (eq (type num) 'INT)(eq (type num) 'REAL)))
    (setq num (atof num))
    )
  (setq styl (getvar "TEXTSTYLE"))
  (initget 1)(setq t0 (getpoint "\nУкажи точку вставки таблицы координат: "))
  (if (eq pr1 1)
    (progn
      (setq t10 (mapcar '+ t0 '(0 12)))
      (setq t11 (mapcar '+ t0 '(8 12)))
      (setq t12 (mapcar '+ t0 '(28 12)))
      (setq t13 (mapcar '+ t0 '(48 12)))
      (setq t14 (mapcar '+ t0 '(68 12)))
      (setq t01 (mapcar '+ t0 '(8 0)))
      (setq t02 (mapcar '+ t0 '(28 0)))
      (setq t03 (mapcar '+ t0 '(48 0)))
      (setq t04 (mapcar '+ t0 '(68 0)))
      (setq N_xy (mapcar '+ t0 '(1.959 7.087)))
      (setq pp_xy (mapcar '+ t0 '(1.959 3.046)))
      (setq t1_xy (mapcar '+ t0 '(11.126 7.087)))
      (setq t2_xy (mapcar '+ t0 '(11.126 3.046)))
      (setq tx_xy (mapcar '+ t0 '(37.254 4.276)))
      (setq ty_xy (mapcar '+ t0 '(57.254 4.276)))
      (entmake (list '(0 . "TEXT")  (cons 10 N_xy) (cons 40 2.4) (cons 1 "№") (cons 7 styl)))
      (entmake (list '(0 . "TEXT")  (cons 10 pp_xy) (cons 40 2.4) (cons 1 "п/п") (cons 7 styl)))
      (entmake (list '(0 . "TEXT")  (cons 10 t1_xy) (cons 40 2.4) (cons 1 "Наимено-") (cons 7 styl)))
      (entmake (list '(0 . "TEXT")  (cons 10 t2_xy) (cons 40 2.4) (cons 1 "вание") (cons 7 styl)))
      (entmake (list '(0 . "TEXT")  (cons 10 ty_xy) (cons 40 2.8) (cons 1 "Y") (cons 7 styl)))
      (entmake (list '(0 . "TEXT")  (cons 10 tx_xy) (cons 40 2.8) (cons 1 "X") (cons 7 styl)))
      (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t0) (append '(11) t10)))
      (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t01) (append '(11) t11)))
      (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t02) (append '(11) t12)))
      (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t03) (append '(11) t13)))
      (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t04) (append '(11) t14)))
      (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t10) (append '(11) t14)))
      (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t0) (append '(11) t04)))
      )
    )
  (initget)(setq t1(getpoint "\nУкажи точку: "))
  (while (not (eq t1 nil))
    (initget 1 )(setq d (getstring T "\nЧто это?: "))
    (if (eq pr2 1)
      (progn
	(setq osn (getvar "OSMODE"))
	(setvar "OSMODE" 0)
	(initget)(setq tt(getpoint "\nУкажи точку куда положить подпись?<ENTER - пропустить эту подпись>: "))
	(setvar "OSMODE" osn)
	))
    (setq num_xy (mapcar '+ t0 (list(- 4(/(numxHS (rtos num 2 0) 2.8 styl)2)) -5.4)))
    (setq d_xy (mapcar '+ t0 '(9.5 -5.4)))
    (setq x_xy (mapcar '+ t0 (list(- 38(/(numxHS (rtos (nth 1 t1) 2 2)2.8 styl)2)) -5.4)))
    (setq y_xy (mapcar '+ t0 (list(- 58(/(numxHS (rtos (nth 0 t1) 2 2)2.8 styl)2)) -5.4)))
    (entmake (list '(0 . "TEXT")  (cons 10 num_xy) (cons 40 2.8) (cons 1 (rtos num 2 0)) (cons 7 styl)))
    (if (and(eq pr2 1)(not (null tt)))
      (progn
	(setq list_points (list t1 tt))
	(setq pt_array (ru-mleader-coords-from-list list_points))
	(setq mleader_obj (vla-addmleader model_spece pt_array 0))
	(vla-put-StyleName mleader_obj "Подписи")
	(vla-put-textstring mleader_obj d)
	)
      )
    (entmake (list '(0 . "TEXT")  (cons 10 d_xy) (cons 40 2.4) (cons 1 d) (cons 7 styl)))
    (entmake (list '(0 . "TEXT")  (cons 10 x_xy) (cons 40 2.8) (cons 1 (rtos (nth 1 t1) 2 2)) (cons 7 styl)))
    (entmake (list '(0 . "TEXT")  (cons 10 y_xy) (cons 40 2.8) (cons 1 (rtos (nth 0 t1) 2 2)) (cons 7 styl)))
    (setq num (1+ num))
    (setq t10 (mapcar '+ t0 '(0 -8)))
    (setq t11 (mapcar '+ t0 '(8 -8)))
    (setq t12 (mapcar '+ t0 '(28 -8)))
    (setq t13 (mapcar '+ t0 '(48 -8)))
    (setq t14 (mapcar '+ t0 '(68 -8)))
    (setq t01 (mapcar '+ t0 '(8 0)))
    (setq t02 (mapcar '+ t0 '(28 0)))
    (setq t03 (mapcar '+ t0 '(48 0)))
    (setq t04 (mapcar '+ t0 '(68 0)))
    (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t0) (append '(11) t10)))
    (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t01) (append '(11) t11)))
    (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t02) (append '(11) t12)))
    (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t03) (append '(11) t13)))
    (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t04) (append '(11) t14)))
    (entmake (list '(0 . "LINE") '(6 . "Continuous") (append '(10) t10) (append '(11) t14)))
    (setq t0 (mapcar '+ t0 '(0 -8)))
    (initget)(setq t1(getpoint "\nУкажи точку: "))
    )
  )


(defun C:insertramka ( / block_name n_y); вставка блока типа рамка(не анототивный)
  (initget 1 )(setq block_name (getstring T "\nВведи имя блока: "))
  (initget 1 "Да Нет _Yes _No")(setq n_y (getkword "\nБедем поворачивать блок? [Да/Нет]: "))
  (princ "\nУкажи точку вставуи блока: ")
  (if (not (or (eq block_name nil) (eq n_y nil)))
    (progn
      (if
	(eq (getvar "CTAB") "Model")
	(setq block_name (strcat "_" block_name))
	)
      (if
	(not (tblobjname "block" block_name))
	(progn
	  (command "_insert" block_name "0,0,0" "1" "1" "0")
	  (entdel (entlast))
	  )
	)
      (command "_insert" block_name "_S" (/ 1 (getvar "CANNOSCALEVALUE")) pause (if (not(eq (getvar "CTAB") "Model"))0.0 pause))
      )
    (princ "Не верные данные!")
    )
  (princ)
  );C:insertramka

(defun C:proba ( / nabor ttt n) ;заменяет объект SOLID на за замкнутую полилинию
  (begin_activex)
  (setq nabor(ssget "_X"'((0 . "SOLID"))))
  (setq n 0)
  (repeat (sslength nabor)
    (setq ttt (entget(ssname nabor n)))
    (if (cdr (assoc 62 ttt))
      (entmake (list '(0 . "LWPOLYLINE")
		     '(100 . "AcDbEntity")
		     '(6 . "Continuous")
		     '(100 . "AcDbPolyline")
		     '(90 . 4) '(70 . 1)
		     (cons 8 (cdr (assoc 8 ttt)))
		     (cons 62 (cdr (assoc 62 ttt)))
		     (cons 10 (cdr (assoc 11 ttt))) (cons 10 (cdr (assoc 10 ttt)))
		     (cons 10 (cdr (assoc 12 ttt))) (cons 10 (cdr (assoc 13 ttt)))))
      (entmake (list '(0 . "LWPOLYLINE")
		     '(100 . "AcDbEntity")
		     '(6 . "Continuous")
		     '(100 . "AcDbPolyline")
		     '(90 . 4) '(70 . 1)
		     (cons 8 (cdr (assoc 8 ttt)))
		     (cons 10 (cdr (assoc 11 ttt))) (cons 10 (cdr (assoc 10 ttt)))
		     (cons 10 (cdr (assoc 12 ttt))) (cons 10 (cdr (assoc 13 ttt)))))
      )
    (entdel (ssname nabor n))
    (setq n (1+ n))
    )
  )

(defun C:proba (/ nabor n ttt dist_list x_list y_list spisokv p1 p2 centr_point radius_list mid_radius);заменяет объект LWPOLYLINE на за круг
  (begin_activex)
  (setq nabor(ssget "_X"'((0 . "LWPOLYLINE"))))
  (setq n 0)
  (repeat (sslength nabor)
    (setq ttt (ssname nabor n))
    ;;; (setq ttt (car(entsel "\nВыбери линию: ")))
    (setq spisokv (extract_coord_lwpolyline (entget ttt)))
    (setq p_first (car spisokv))
    (repeat (1-(length spisokv))
      (setq p1 (car spisokv))
      (setq spisokv (cdr spisokv))
      (setq p2 (car spisokv))
      (setq dist_list (append dist_list (list(distance p1 p2))))
      )
    (setq p_last (car spisokv))
    (setq dist_list (append dist_list (list(distance p_first p_last))))
    (if (and(equal (apply 'min dist_list)(apply 'max dist_list) 0.01)(>(length dist_list)3))
      (progn
	(setq spisokv (extract_coord_lwpolyline (entget ttt)))
	(repeat (length spisokv)
	  (setq x_list (append x_list (list(caar spisokv))))
	  (setq y_list (append y_list (list(cadar spisokv))))
	  (setq spisokv (cdr spisokv))
	  )
	(setq centr_point (list(/(apply '+ x_list)(length x_list))(/(apply '+ y_list)(length y_list))))
	(setq spisokv (extract_coord_lwpolyline (entget ttt)))
	(repeat (length spisokv)
	  (setq p1 (car spisokv))
	  (setq spisokv (cdr spisokv))
	  (setq radius_list (append radius_list (list(distance centr_point p1))))
	  )
	(setq mid_radius (/(apply '+ radius_list)(length radius_list)))
	(if (cdr (assoc 62 (entget ttt)))
	  (entmake (list '(0 . "CIRCLE")'(6 . "Continuous")'(100 . "AcDbCircle")
			 (cons 8 (cdr (assoc 8 (entget ttt))))
			 (cons 62 (cdr (assoc 62 (entget ttt))))
			 (cons 10 centr_point) (cons 40 mid_radius)))
	  (entmake (list '(0 . "CIRCLE")'(6 . "Continuous")'(100 . "AcDbCircle")
			 (cons 8 (cdr (assoc 8 (entget ttt))))
			 (cons 10 centr_point) (cons 40 mid_radius)))
	  )
	(entdel ttt)
	)
      )
    (setq n (1+ n))
    (setq dist_list nil)
    (setq x_list nil)
    (setq y_list nil)
    (setq radius_list nil)
    )
  (princ)
  )


  (setq n 0)
  (setq bad_dist_list nil)
  (repeat (length dist_point11_list)
    (setq item(nth n dist_point11_list))
    (if (and (or(< min_bad_dist item)(= min_bad_dist item))
	     (or(> max_bad_dist item)(= max_bad_dist item)))
      (setq bad_dist_list (append bad_dist_list (list item)))
      )
    (setq n (1+ n))
    )
  (princ "\n кол=во")
  (princ (length bad_dist_list))
  (setq dist_item min_bad_dist)
  (princ "\n dist_item ")
  (princ dist_item)
  (setq shag (/ (- max_bad_dist min_bad_dist) (length bad_dist_list)))
  (princ "\n шаг")
  (princ shag)
  (setq n 0)
  (repeat (length bad_dist_list)
    
    (setq line_odj (nth (vl-position (nth n bad_dist_list) dist_point11_list) list_line_obj))
    (princ "\n vl-position ")
    (princ (vl-position (nth n bad_dist_list) dist_point11_list))
    (entmod(subst (cons 11 (vlax-curve-getPointAtDist obj2 dist_item))(assoc 11 (entget line_odj))(entget line_odj)))
    (entupd line_odj)
    (setq dist_item (+ dist_item shag))
    (setq n (1+ n))
    )
  (entmake (list '(0 . "LINE") (cons 10 (vlax-curve-getPointAtDist obj2 min_bad_dist))(cons 11 (vlax-curve-getPointAtDist obj2 max_bad_dist))))


  (setq min_position_obj (vl-position min_bad_dist dist_point11_list))
  (setq max_position_obj (vl-position max_bad_dist dist_point11_list))
  (princ "\n min ")
  (princ min_position_obj)
  (princ "\n max ")
  (princ max_position_obj)
  (setq bad_dist_list nil)
  (setq shag (/ (-
		  (vlax-curve-getDistAtPoint obj2 (cdr(assoc 11 (entget (nth (1+ max_position_obj) list_line_obj)))))
		  (vlax-curve-getDistAtPoint obj2 (cdr(assoc 11 (entget (nth (1- min_position_obj) list_line_obj)))))
		  )
		(- max_position_obj min_position_obj)))
  (princ "\n shag ")
  (princ shag)
  (setq n (1- min_position_obj))
  (setq dist_item (vlax-curve-getDistAtPoint obj2 (cdr(assoc 11 (entget (nth (1- min_position_obj) list_line_obj))))))
  (repeat (1-(- max_position_obj min_position_obj))
    (setq line_odj (nth n list_line_obj))
    
    (entmod(subst (cons 11 (vlax-curve-getPointAtDist obj2 dist_item))
		  (assoc 11 (entget line_odj))(entget line_odj)))
    
    (entupd line_odj)
    (setq n (1+ n))
    (setq dist_item (+ dist_item shag)))

  (while (not(or(eq (1+ n)(1-(length dist_point11_list)))
		 (>(1+ n) (1-(length dist_point11_list)))
		 ))
    (if (> (nth n dist_point11_list)(nth (1+ n) dist_point11_list))
      (setq bad_dist_list (append bad_dist_list (list (nth (1+ n) dist_point11_list))))
      )
    (setq n (1+ n))
    )

  (setq n 0)
  (setq bad_dist_list nil)
  (repeat (length dist_point11_list)
    (setq item(nth n dist_point11_list))
    (if (and (or(< min_bad_dist item)(= min_bad_dist item))
	     (or(> max_bad_dist item)(= max_bad_dist item)))
      (setq bad_dist_list (append bad_dist_list (list item)))
      )
    (setq n (1+ n))
    )
  (princ "\n кол=во")
  (princ (length bad_dist_list))
  (setq dist_item min_bad_dist)
  (princ "\n dist_item ")
  (princ dist_item)
  (setq shag (/ (- max_bad_dist min_bad_dist) (length bad_dist_list)))
  (princ "\n шаг")
  (princ shag)
  (setq n 0)
  (repeat (length bad_dist_list)
    (setq line_odj (nth (vl-position (nth n bad_dist_list) dist_point11_list) list_line_obj))
    (princ "\n dist_item ")
    (princ dist_item)
    (entmod(subst (cons 11 (vlax-curve-getPointAtDist obj2 dist_item))(assoc 11 (entget line_odj))(entget line_odj)))
    (entupd line_odj)
    (setq dist_item (+ dist_item shag))
    (setq n (1+ n))
    )
  (entmake (list '(0 . "LINE") (cons 10 (vlax-curve-getPointAtDist obj2 min_bad_dist))(cons 11 (vlax-curve-getPointAtDist obj2 max_bad_dist))))





(defun C:pl_koor ( / a b c t0 t1 t2 p p1 p2 p3 num line pline spisok sm dl alfa beta); выводит каталог координат полилинии
  (begin_activex)
  (initget 1)(setq pline (car (entsel "\nВыбери полилинию трассы: ")))
  (cond
    ((eq pline nil)
     (alert "Ничего не выбрано!"))
    ((not(eq (cdr(assoc 0 (entget pline))) "LWPOLYLINE"))
     (alert "Выбрана не 2D полилиния!"))
    ((eq (cdr(assoc 0 (entget pline))) "LWPOLYLINE")
     (setq 2Dpline_vla (vlax-ename->vla-object pline))
     (begin_activex)
     (command "_-purge" "Б" "Координаты трассы" "Н")
     (setq block (vla-Add (vla-get-Blocks active_document) (vlax-3D-point '(0 0 0)) "Координаты трассы"))
     (setq t0 '(0 0 0))
     (setq tl (mapcar '- t0 '(-48 16)))
     (setq spisok (extract_coord_lwpolyline (entget pline)))
     (setq num 1)
     (vla-put-Linetype (vla-AddLine block (vlax-3D-point t0)(vlax-3D-point (mapcar '+ t0 '(68 0)))) "Continuous")
     (vla-put-Linetype (vla-AddLine block (vlax-3D-point (mapcar '- t0 '(0 12)))(vlax-3D-point (mapcar '+ t0 '(48 -12)))) "Continuous")
     (vla-put-Linetype (vla-AddLine block (vlax-3D-point (mapcar '+ t0 '(48 -16)))(vlax-3D-point (mapcar '+ t0 '(68 -16)))) "Continuous")
     (vla-put-Linetype (vla-AddLine block (vlax-3D-point t0)(vlax-3D-point (mapcar '- t0 (list 0 (+ 12 (* 8 (1+(length spisok)))))))) "Continuous")
     (vla-put-Linetype (vla-AddLine block (vlax-3D-point (mapcar '+ t0 '(8 0)))(vlax-3D-point  (mapcar '- t0 (list  -8 (+ 12 (* 8 (length spisok))))))) "Continuous")
     (vla-put-Linetype (vla-AddLine block (vlax-3D-point (mapcar '+ t0 '(28 0)))(vlax-3D-point (mapcar '- t0 (list  -28 (+ 12 (* 8 (length spisok))))))) "Continuous")
     (vla-put-Linetype (vla-AddLine block (vlax-3D-point (mapcar '+ t0 '(48 0)))(vlax-3D-point (mapcar '- t0 (list  -48 (+ 12 (* 8 (1+(length spisok)))))))) "Continuous")
     (vla-put-Linetype (vla-AddLine block (vlax-3D-point (mapcar '+ t0 '(68 0)))(vlax-3D-point (mapcar '- t0 (list  -68 (+ 12 (* 8 (1+(length spisok)))))))) "Continuous")
     (vla-put-Linetype (vla-AddLine block (vlax-3D-point (mapcar '- t0 (list 0 (+ 12 (* 8 (1+(length spisok)))))))(vlax-3D-point (mapcar '- t0 (list  -68 (+ 12 (* 8 (1+(length spisok)))))))) "Continuous")
     (vla_AddText block "№"(mapcar '+ t0 '(4 -3))2.8 4 T nil T T)
     (vla_AddText block  "п/п"(mapcar '+ t0 '(4 -9))2.8 4 T nil T T)
     (vla_AddText block  "X"(mapcar '+ t0 '(18 -6))2.8 4 T nil T T)
     (vla_AddText block  "Y"(mapcar '+ t0 '(38 -6))2.8 4 T nil T T)
     (vla_AddText block  "Длина"(mapcar '+ t0 '(58 -5))2.8 4 T nil T T)
     (vla_AddText block  "линии"(mapcar '+ t0 '(58 -11))2.8 4 T nil T T)
     (vla_AddText block  (strcat "Длина трассы = " (rtos (vlax-curve-getDistAtPoint 2Dpline_vla (vlax-curve-getEndPoint 2Dpline_vla)) 2 2)"м")
       (mapcar '- t0 (list -24 (+ 16 (* 8 (length spisok)))))2.8 4 T nil T T)
     (setq t0 (mapcar '- t0 '(0 12)))
     ;                          первая вершина
     (setq p1 (car spisok))
     (setq p2 (cadr spisok))
     (vla_AddText block (itoa num)(mapcar '+ t0 '(4 -4))2.4 4 T nil T T)
     (vla_AddText block (rtos (nth 1 p1) 2 2)(mapcar '+ t0 '(18 -4))2.4 4 T nil T T)
     (vla_AddText block (rtos (nth 0 p1) 2 2)(mapcar '+ t0 '(38 -4))2.4 4 T nil T T)
     (entmake (list '(0 . "TEXT") (cons 10 t0) '(72 . 4) (cons 11 (polar p1 (angle p2 p1)3.5)) (cons 40 3) (cons 1 (itoa num)) (cons 7 (getvar "TEXTSTYLE"))))
     (entmake (list '(0 . "CIRCLE") (cons 10 p1) '(40 . 0.75)))
     (setq t0 (mapcar '- t0 '(0 8)))
     (vla-put-Linetype (vla-AddLine block (vlax-3D-point t0)(vlax-3D-point (mapcar '+ t0 '(48 0)))) "Continuous")
     (setq num (1+ num))
     ;                          вершины посередине
     (repeat (- (length spisok) 2)
       (setq p1 (car spisok))
       (setq p2 (cadr spisok))
       (setq p3 (caddr spisok))
       (setq spisok (cdr spisok))
       (vla_AddText block (itoa num)(mapcar '+ t0 '(4 -4))2.4 4 T nil T T)
       (vla_AddText block (rtos (nth 1 p2) 2 2)(mapcar '+ t0 '(18 -4))2.4 4 T nil T T)
       (vla_AddText block (rtos (nth 0 p2) 2 2)(mapcar '+ t0 '(38 -4))2.4 4 T nil T T)
       (cond
	 ((> (ang3points p1 p2 p3) (ang3points p3 p2 p1))
	  (setq p (polar p2 (+(angle p2 p3) (/(ang3points p1 p2 p3)2)) 3.5))
	  )
	 ((< (ang3points p1 p2 p3) (ang3points p3 p2 p1))
	  (setq p (polar p2 (+(angle p2 p1) (/(ang3points p3 p2 p1)2)) 3.5))
	  )
	 ((= (ang3points p1 p2 p3) (ang3points p3 p2 p1))
	  (setq p (polar p2 (+(angle p2 p3) (/ pi 2)) 3.5))
	  )
	 )
       (entmake (list '(0 . "TEXT") (cons 10 t0) '(72 . 4) (cons 11 p) (cons 40 3) (cons 1 (itoa num)) (cons 7 (getvar "TEXTSTYLE"))))
       (entmake (list '(0 . "CIRCLE") (cons 10 p2) '(40 . 0.75)))
       (setq t0 (mapcar '- t0 '(0 8)))
       (vla-put-Linetype (vla-AddLine block (vlax-3D-point t0)(vlax-3D-point (mapcar '+ t0 '(48 0)))) "Continuous")
       (setq num (1+ num))
       )
     ;                          последняя вершина
     (setq p1 (car spisok))
     (setq p2 (cadr spisok))
     (vla_AddText block (itoa num)(mapcar '+ t0 '(4 -4))2.4 4 T nil T T)
     (vla_AddText block (rtos (nth 1 p2) 2 2)(mapcar '+ t0 '(18 -4))2.4 4 T nil T T)
     (vla_AddText block (rtos (nth 0 p2) 2 2)(mapcar '+ t0 '(38 -4))2.4 4 T nil T T)
     (entmake (list '(0 . "TEXT") (cons 10 t0) '(72 . 4) (cons 11 (polar p2 (angle p1 p2)3.5)) (cons 40 3) (cons 1 (itoa num)) (cons 7 (getvar "TEXTSTYLE"))))
     (entmake (list '(0 . "CIRCLE") (cons 10 p2) '(40 . 0.75)))
     (setq t0 (mapcar '- t0 '(0 8)))
     (vla-put-Linetype (vla-AddLine block (vlax-3D-point t0)(vlax-3D-point (mapcar '+ t0 '(48 0)))) "Continuous")
     (setq spisok (extract_coord_lwpolyline (entget pline)))
     (setq Ed 0)
     (repeat (1- (length spisok))
       (setq p1 (car spisok))
       (setq spisok (cdr spisok))
       (setq p2 (car spisok))
       (vla_AddText block (rtos (distance p1 p2) 2 2)(mapcar '+ tl '(10 -4))2.4 4 T nil T T)
       (setq tl (mapcar '- tl '(0 8)))
       (vla-put-Linetype (vla-AddLine block (vlax-3D-point tl)(vlax-3D-point (mapcar '+ tl '(20 0)))) "Continuous")
       (setq Ed (+ Ed (distance p1 p2)))
       )
     (vla_AddText block (strcat "\U+03A3L="(rtos Ed 2 2) "м")(mapcar '+ tl '(10 -6))2.4 4 T nil T T)
     (princ "\nУкажи точку вставки таблицы: ")
     (command "_insert" "Координаты трассы" pause "1" "1" 0.0)
     (command "_explode" (entlast))
     (command "_-purge" "Б" "Координаты трассы" "Н")
     (setq create_file (yes_no_dialog "Создаем SDR файл?"))
     (if create_file
       (progn
	 (setq putTXTfile(strcat(getvar "DWGPREFIX")(substr (getvar "DWGNAME") 1 (-(strlen (getvar "DWGNAME"))4)) "_" (vla-get-Layer 2Dpline_vla)))
	 (setq putSDR33file (getfiled "Имя сохраняемого файла..." putTXTfile "sdr" 1))
	 (initget)(setq perffix (getstring "\nЧто добавим перед номером точки?: "))
	 (if (eq perffix nil) (setq perffix ""))
	 (setq SDR33file (open putSDR33file "w"))
	 (setq spisok (extract_coord_lwpolyline (entget pline)))
	 (setq num 1)
	 (repeat (length spisok)
	   (setq p (car spisok))
	   (setq spisok (cdr spisok))
	   (write-line (strcat
			 "08TP"
			 (add_probel (strcat perffix (itoa num)) 16 T)
			 (add_probel (rtos (cadr p) 2 3) 16 nil)
			 (add_probel (rtos (car p) 2 3) 16 nil)
			 (add_probel "0.000" 16 nil)
			 (add_probel " " 16 nil)
			 ) SDR33file)
	   (setq num (1+ num))
	   )
	 (close SDR33file)
	 )
       )
     )
    )
  (princ)
  )


(defun C:insert_C3D_points_SDR33file (/ putSDR33file SDR33file putTXTfile TXTfile FileName osn)
  (setq osn (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setq putSDR33file (getfiled "Выбор файла SDR33" (getvar "DWGPREFIX") "txt;sdr" 0))
  (setq putTXTfile (strcat (substr putSDR33file 1 (-(strlen putSDR33file)4))"_" (substr putSDR33file (-(strlen putSDR33file)3) 4)))
  (setq FileName(substr putSDR33file (+(vl-string-position 92 putSDR33file 0 T)2) (-(strlen putSDR33file)(+(vl-string-position 92 putSDR33file 0 T)2)3)))
  (initget "Да Нет")(setq insert_points (getkword "\nВставляем точки? [Да/Нет] <Да>: "))
  (if (eq insert_points nil) (setq insert_points "Да"))
  (if (eq insert_points "Да")
    (if
      (eq(tblsearch "LAYER" FileName)nil)
      (command "_-LAYER" "с" FileName "")
      (command "_-LAYER" "_S" FileName "")
      )
    )
  (setq TXTfile (open putTXTfile "w"))
  (setq SDR33file (open putSDR33file"r"))
  (setq str (read-line SDR33file))
  (while (not(eq (substr str 1 4)"000"))
    (if (or(eq (substr str 1 4) "08TP") (eq (substr str 1 4) "08KI") (eq (substr str 1 4) "02TP"))
      (progn
	(write-line (strcat
		      (vl-string-trim" "(substr str 5 16))" "; номер
		      (vl-string-trim" "(substr str 21 16))" "; X
		      (vl-string-trim" "(substr str 37 16))" "; Y
		      (vl-string-trim" "(substr str 53 16))" "; H
		      (vl-string-trim" "(substr str 69 16)))  ; описание
	  TXTfile)
	(if (eq insert_points "Да")
	  (command "_AeccCreatePointManual"
		   (list
		     (atof(vl-string-trim" "(substr str 37 16)))
		     (atof(vl-string-trim" "(substr str 21 16)))
		     )
		   (vl-string-trim" "(substr str 5 16))
		   (atof(vl-string-trim" "(substr str 53 16)))
		   ""
		   )
	  )
	)
      )
    (setq str (read-line SDR33file))
    )
  (close SDR33file)
  (close TXTfile)
  (setvar "OSMODE" osn)
  (princ)
  )





(defun C:proba ( / patch n list_plansh planshet planshet_list Hmin Hmax Litera pl_namber insert_point)
  (setq patch (getfiled "Выбор файла планшетов" (getvar "DWGPREFIX") "txt;sdr" 0))
  (setq list_plansh (readfile patch))
  (setq n 1)
  (repeat (length list_plansh)
    (setq planshet (car list_plansh))
    (setq list_plansh (cdr list_plansh))
    (setq planshet_list (vl-string->list planshet))
    (setq planshet_list (reverse planshet_list))
    (while (not(eq (car planshet_list) (ascii " ")))
      (setq Hmin (append Hmin (list(car planshet_list))))
      (setq planshet_list (cdr planshet_list))
      )
    (setq Hmin (atof(vl-list->string(reverse Hmin))))
    (princ "\n")
    (princ Hmin)
    (setq planshet_list (cdr planshet_list))
    (while (not(eq (car planshet_list) (ascii " ")))
      (setq Hmax (append Hmax (list(car planshet_list))))
      (setq planshet_list (cdr planshet_list))
      )
    (setq Hmax (atof(vl-list->string(reverse Hmax))))
    (princ "\n")
    (princ Hmax)
    (setq planshet_list (cdr planshet_list))
    (setq Litera (vl-list->string(list(car planshet_list))))
    (princ "\n")
    (princ Litera)
    (setq planshet_list (cdr planshet_list))
    (setq planshet_list (cdr planshet_list))
    (princ "\n")
    (princ planshet_list)
    (while (not(eq (car planshet_list) (ascii "-")))
      (setq pl_namber (append pl_namber (list(car planshet_list))))
      (setq planshet_list (cdr planshet_list))
      )
    (setq pl_namber (atof(vl-list->string(reverse pl_namber))))
    (princ "\n")
    (princ pl_namber)
    (entmake (list '(0 . "LWPOLYLINE")
		   '(100 . "AcDbEntity")
		   '(6 . "Continuous")
		   '(100 . "AcDbPolyline")
		   '(90 . 4) '(70 . 1) '(43 . 0)
		   '(10 0 0 0)
		   '(10 480 0 0)
		   '(10 480 -480 0)
		   '(10 0 -480 0)))
    (setq insert_point(list(*(if(=(rem pl_namber 12)0)
			       11
			       (1-(rem pl_namber 12))
			       )
			     40)
			   (*(if(=(rem pl_namber 12)0)
			       (1-(fix(/ pl_namber 12)))
			       (fix(/ pl_namber 12)))
			     -40)0))
    (princ "\n")
    (princ insert_point)
    (cond
      ((= Litera "А")
       )
      ((= Litera "Б")
       (setq insert_point(mapcar '+ insert_point '(20 0 0)))
       )
      ((= Litera "В")
       (setq insert_point(mapcar '+ insert_point '(0 -20 0)))
       )
      ((= Litera "Г")
       (setq insert_point(mapcar '+ insert_point '(20 -20 0)))
       )
      )
    (entmake (list '(0 . "LWPOLYLINE")
		   '(100 . "AcDbEntity")
		   '(6 . "Continuous")
		   '(100 . "AcDbPolyline")
		   '(90 . 4) '(70 . 1) '(43 . 0)
		   (cons 10 insert_point)
		   (cons 10 (mapcar '+ insert_point '(20 0 0)))
		   (cons 10 (mapcar '+ insert_point '(20 -20 0)))
		   (cons 10 (mapcar '+ insert_point '(0 -20 0)))
		   ))
    (entmake (list '(0 . "TEXT")
		   (cons 10 insert_point)
		   (cons 40 5)
		   (cons 1 (rtos n 2 0)) (cons 7 (getvar "TEXTSTYLE"))
		   '(72 . 4)
		   (cons 11 (mapcar '+ insert_point '(10 -10 0)))))
    (entmake (list '(0 . "TEXT")
		   (cons 10 insert_point)
		   (cons 40 3)
		   (cons 1 (rtos Hmax 2 0)) (cons 7 (getvar "TEXTSTYLE"))
		   '(72 . 0)
		   (cons 11 (mapcar '+ insert_point '(1 -1 0)))
		   '(73 . 3)))
    (entmake (list '(0 . "TEXT")
		   (cons 10 (mapcar '+ insert_point '(1 -19 0)))
		   (cons 40 3)
		   (cons 1 (rtos Hmin 2 0)) (cons 7 (getvar "TEXTSTYLE"))
		   '(72 . 0)
		   '(73 . 0)))
    (entmake (list '(0 . "TEXT")
		   (cons 10 insert_point)
		   (cons 40 3)
		   (cons 1 (rtos (/(+ Hmax Hmin)2) 2 0)) (cons 7 (getvar "TEXTSTYLE"))
		   '(72 . 2)
		   (cons 11 (mapcar '+ insert_point '(19 -19 0)))
		   '(73 . 0)))
    (entmake (list '(0 . "TEXT")
		   (cons 10 insert_point)
		   (cons 40 3)
		   (cons 1 (rtos (/(- Hmax Hmin)(/(+ Hmax Hmin)2)) 2 3)) (cons 7 (getvar "TEXTSTYLE"))
		   '(72 . 2)
		   (cons 11 (mapcar '+ insert_point '(19 -1 0)))
		   '(73 . 3)))
    (setq n (1+ n))
    (setq Hmin nil)
    (setq Hmax nil)
    (setq Litera nil)
    (setq pl_namber nil)
    (setq insert_point nil)
    (setq planshet_list nil)
    (setq planshet nil)
    (princ "\n----------------------------------")
    )
  (princ)
  )


(defun C:proba (/ obj1 obj2 d line_odj line_vla_odj temp)
  (defun min_dist_point (p list_mas / point_list dist_list n)
    (repeat (/(length list_mas)3)
      (setq point_list (append point_list (list(list (car list_mas)(cadr list_mas)(caddr list_mas)))))
      (setq list_mas (cdddr list_mas))
      )
    (setq n 0)
    (repeat (length point_list)
      (setq dist_list (append dist_list (list (distance p (nth n point_list)))))
      (setq n (1+ n))
      )
    (nth (vl-position (apply 'min dist_list) dist_list) point_list)
    )
  (initget 1)(setq obj1(vlax-ename->vla-object (car (entsel))))
  (initget 1)(setq obj2(vlax-ename->vla-object (car (entsel))))
  (setq d 0)
  (while (< d (vlax-curve-getDistAtPoint obj1 (vlax-curve-getEndPoint obj1)))
    (entmake (list '(0 . "LINE") (cons 10 (vlax-curve-getPointAtDist obj1 d))
		   (cons 11 (polar (vlax-curve-getPointAtDist obj1 d)
				   (-(angle '(0 0 0) (vlax-curve-getfirstDeriv obj1 (vlax-curve-getParamAtPoint obj1 (vlax-curve-getPointAtDist obj1 d))))(/ pi 2))
				   2))))
    (setq line_odj (entlast))
    (setq line_vla_odj (vlax-ename->vla-object line_odj))
    (setq temp (vla-IntersectWith line_vla_odj obj2 acExtendBoth))
    (if (/= -1 (vlax-safearray-get-u-bound(vlax-variant-value temp)1))
      (progn
	(setq temp(vlax-safearray->list(vlax-variant-value temp)))
	(entmod(subst (cons 11 (min_dist_point (vlax-curve-getPointAtDist obj1 d) temp)) (assoc 11 (entget line_odj))(entget line_odj)))
	(entupd line_odj)
	)
      )
    (setq d (+ d 1))
    (if (< d (vlax-curve-getDistAtPoint obj1 (vlax-curve-getEndPoint obj1)))
      (progn
	(entmake (list '(0 . "LINE") (cons 10 (vlax-curve-getPointAtDist obj1 d))
		       (cons 11 (polar (vlax-curve-getPointAtDist obj1 d)
				       (-(angle '(0 0 0) (vlax-curve-getfirstDeriv obj1 (vlax-curve-getParamAtPoint obj1 (vlax-curve-getPointAtDist obj1 d))))(/ pi 2))
				       2))))
	(setq line_odj (entlast))
	(setq line_vla_odj (vlax-ename->vla-object line_odj))
	(setq temp (vla-IntersectWith line_vla_odj obj2 acExtendBoth))
	(if (/= -1 (vlax-safearray-get-u-bound(vlax-variant-value temp)1))
	  (progn
	    (setq temp(vlax-safearray->list(vlax-variant-value temp)))
	    (entmod(subst (cons 11 (polar(vlax-curve-getPointAtDist obj1 d)(angle(vlax-curve-getPointAtDist obj1 d)(min_dist_point (vlax-curve-getPointAtDist obj1 d) temp))2))
			    (assoc 11 (entget line_odj))(entget line_odj)))
	    
	    (entupd line_odj)
	    (setq d (+ d 1))
	    )
	  )
	)
      )
    )
  (princ)
  )

(defun C:proba ()
  (if (yes_no_dialog "Пробный вопрос?")
    (princ "Ты ответил ДА!")
    (princ "Ты ответил НЕТ!")
    )
  (princ)
  )



(defun C:ed_pl_z (/ pline t1 n vertix z); позволяет изменить отметку у выбранной вершины 3D полилини
  (setq pline (car (entsel "\nВыберите 3D полилинию: ")))
  (initget)(setq t1(getpoint "\nУкажи узел полилинии: "))
  (while
    (/= t1 nil)
    (setq spisokv (extract_coord_polyline pline))
    (setq n (vl-position t1 spisokv))
    (setq vertix (entnext pline))
    (repeat n
      (setq vertix (entnext vertix))
      )
    (initget)(setq z (getreal (strcat "\nВведи отметку <" (rtos (nth 2 (cdr(assoc 10 (entget vertix)))) 2 2) "> :")))
    (if (eq z nil) (setq z  (nth 2 (cdr(assoc 10 (entget vertix))))))
    (setq p (list (nth 0 (cdr (assoc 10 (entget vertix)))) (nth 1 (cdr(assoc 10 (entget vertix)))) z))
    (entmod (subst (cons 10 p)(assoc 10 (entget vertix)) (entget vertix)))
    (entupd pline)
    (initget)(setq t1(getpoint "\nУкажи узел полилинии: "))
    )
  (princ)
  )

(defun get_z (/ z z_summa list_z x z_srednee temp list_num_points num_point list_add_z nabor n)
  ; функция запроса отметок вычисления из них среднего и внесения поправки
    (begin_activex)
    (setq z 0.0)
    (if (eq popravka nil)
      (setq popravka 0.0))
    (while (not (eq z nil))
      (setq z_summa 0.0)
      (foreach x list_z (setq z_summa (+ z_summa x)))
      (if (/=(length list_z)0)
	(setq z_srednee (/ z_summa (length list_z)))
	(setq z_srednee 0.0)
	)
      (initget "ПОПравка ПИКеты ТЕКсты")
      (setq z (getreal (strcat"\nИзмерений: "(itoa (length list_z))
			      " Среднее: " (rtos z_srednee 2 2)
			      " Поправка: " (rtos popravka 2 2)
			      " Результат: "(rtos (+ z_srednee popravka) 2 2)
			      " Введи отметку или [ПОПравка/ПИКеты/ТЕКсты]<ПРОДОЛЖИТЬ>: ")))
      (cond
	((eq z "ПОПравка")
	 (initget)
	 (setq temp(getreal (strcat "\nВведи поправку к отметке<"(rtos popravka 2 3)">: ")))
	 (if
	   (/= temp nil)
	   (setq popravka temp)
	   )
	 )
	((eq z "ПИКеты")
	 (setq list_num_points (vlax-invoke C3D_active_document 'SelectPoints))
	 (foreach num_point list_num_points
	   (setq list_add_z(append list_add_z (list(vlax-get (vlax-invoke C3D_points 'find num_point) 'elevation))))
	   )
	 )
	((eq z "ТЕКсты")
	 (setq nabor (ssget '((0 . "TEXT"))))
	 (setq n 0)
	 (repeat (sslength nabor)
	   (setq list_add_z (append list_add_z (list(atof(clean_text_for_number(cdr(assoc 1(entget(ssname nabor n)))))))))
	   (setq n (1+ n))
	   )
	 )
	((eq 'REAL (type z))
	 (setq list_add_z (append (list z)))
	 )
	)
      (setq list_z(append list_z list_add_z))
      (setq list_add_z nil)
      )
    (+ z_srednee popravka)
    )

(defun load-all( / app_list app_counter)
  
  (setq app_list (list
		   "fil.lsp"
		   "frto.lsp"
		   "pltools.lsp"
		   "В работе.lsp"
		   "Вспомогалельные подпрограммы.LSP"
		   "Вставка объектов.LSP"
		   "Мусор.lsp"
		   "Планшеты.lsp"
		   "Подпись зданий.lsp"
		   "Профиль.lsp"
		   "Редактитование объектов.LSP"
		   "Рисование.LSP"
		   "Тексты.LSP"
		   "Трасса.LSP"
		   "Оформление.LSP"
		   )
    );_setq
  
  (foreach app_counter app_list
    (load app_counter)
    );_foreach
  );_defun

(defun C:lishnee (/ object ttt nabor block); переводит выделенные объекты в слой лишние пикеты
  (setq object (vibery_object))
  (while (not (eq object nil))
  (setq ttt (entget object))
  (setq ttt (subst (cons 8 "Лишние пикеты")(assoc 8 ttt) ttt))
  (entmod ttt)
  (entupd object)
  (setq object (vibery_object))
    )
  (princ)
  ); defun C:lishnee


(defun C:razmer (/ t1 t2 t3 t4 t5 spisok) ; рисует стрелочку с с подписью длинны
  (while (= spisok nil)
  (initget)(setq t1(getpoint "\nУкажи начачальную точку: ")); указываю точку
  (initget)(setq t2(getpoint t1 "\nУкажи конечную точку: ")); указываю вторую точку
  (setq t3 (polar t2 (angle t2 t1) (/ 2 (getvar "CANNOSCALEVALUE"))))
  (setq t5 (polar t1 (angle t1 t2) (/ 0.75 (getvar "CANNOSCALEVALUE"))))
  (setq spisok (list '(0 . "TEXT") (cons 10 t1) (cons 40 (/ 2 (m_kof))) (cons 1 (rtos (distance t1 t2) 2 2)) '(50 . 0) '(7 . "OTI")))
  (setq t4
	 (if
	   (= (angle t1 t2) (aglong (angle t1 t2)))
	   (polar
	     (polar t5 (angle t1 t2) (- (/ (distance t5 t3) 2) (/ (numx spisok) 2)))
		    (+ (angle t1 t2) (/ pi 2)) (/ 0.8 (getvar "CANNOSCALEVALUE"))
		    ); yes
	   (polar
	     (polar t5 (angle t1 t2) (+ (/ (distance t5 t3) 2) (/ (numx spisok) 2)))
		    (- (angle t1 t2) (/ pi 2)) (/ 0.8 (getvar "CANNOSCALEVALUE"))
		    ); no
	   ); if
	); t4
  (entmake (list '(0 . "TEXT") (cons 8 "Ситуация") (cons 10 t4) (cons 40 (/ 2 (m_kof))) (cons 1 (rtos (distance t1 t2) 2 2)) (cons 50 (aglong (angle t1 t2))) '(7 . "OTI")))
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") (cons 8 "Ситуация") '(100 . "AcDbPolyline")
		 '(90 . 3)
		 (cons 10 t2) '(40 . 0.0) (cons 41 (/ 0.8 (getvar "CANNOSCALEVALUE"))) '(42 . 0.0) 
		 (cons 10 t3) '(40 . 0.0) '(41 . 0.0) '(42 . 0.0)
		 (cons 10 t1) '(40 . 0.0) '(41 . 0.0) '(42 . 0.0)
		 )
	   )
    (setq spisok nil)
    )
  (princ)
  )

(defun C:proba (/ dcl_id dx dy)
  (setq dcl_id (load_dialog "Диалоговые окна.dcl"))
  (new_dialog "d_sektion" dcl_id)
  (setq dx (dimx_tile "sld"))
  (setq dy (dimy_tile "sld"))
  (start_image "sld")
  (slide_image 0 0 dx dy "sld_2")
  (end_image)
  (set_tile "text1" "Шир.внеш.(L1)")
  (set_tile "text2" "Шир.внут.(L2)")
  (set_tile "text3" "Толщ.ст.(L3)")
  (set_tile "text4" "До оси тр.(L4)")
  (set_tile "text5" "Меж.ос.тр.(L5)")
  (set_tile "text6" "L6")
  (set_tile "text7" "L7")
  (set_tile "text8" "L8")
  (set_tile "text9" "Диам.тр.(d1)")
  (set_tile "text10" "Диам.из.(d2)")
  (set_tile "text11" "Общ.выс.(h1)")
  (set_tile "text12" "h2")
  (set_tile "text13" "h3")
  (set_tile "text14" "h4")
  (set_tile "text15" "h5")
  (set_tile "text16" "h6")
  (set_tile "text17" "h7")
  (start_dialog)
  (unload_dialog dcl_id)
)

(defun C:draw_text_be_style_bsam ( / t1 a str); Пишет OTI
  (terpri)
  (format_check_text_style)
  (initget)(setq t1(getpoint "Укажи точку вставки текста:"))(terpri); указываю точку
  (initget)(setq a (getangle t1 "Введи угол поворота или укажи направление:"))(terpri); указываю угол
  (if (= a nil)
    (setq a (* pi 1.5))
    ); горизонтальное положение текста при пустом вводе
  (setq str "1")
  (while (not (= str nil))
    (initget)(setq str (getstring T "Введи текст: "))
    (if (= str "")
      (progn
      (setq str nil)
      );progn
      (progn
        (entmake (list '(0 . "TEXT") '(6 . "Continuous") (cons 10 t1) (cons 40 (/ (/ 2 (format_scale_kof)) 1.05994)) (cons 1 str) (cons 50 (+ a (/ pi 2))) '(7 . "OTI")))
        (setq t1 (polar t1 a (+ (/ (/ 2 (format_scale_kof)) 1.05994) (/ 1 (format_scale_kof)))))
	);progn
      );if
    );while
  (princ)
  )
(defun draw_text_be_style_drevn ( / t1 a str); Пишет OTI
  (terpri)
  (format_check_text_style)
  (initget)(setq t1(getpoint "Укажи точку вставки текста:"))(terpri); указываю точку
  (initget)(setq a (getangle t1 "Введи угол поворота или укажи направление:"))(terpri); указываю угол
  (if (= a nil)
    (setq a (* pi 1.5))
    ); горизонтальное положение текста при пустом вводе
  (setq str "1")
  (while (not (= str nil))
    (initget)(setq str (getstring T "Введи текст: "))
    (if (= str "")
      (progn
      (setq str nil)
      );progn
      (progn
        (entmake (list '(0 . "TEXT") '(6 . "Continuous") (cons 10 t1) (cons 40 (/ (/ 2 (format_scale_kof)) 1)) (cons 1 str) (cons 50 (+ a (/ pi 2))) '(7 . "OTI")))
        (setq t1 (polar t1 a (+ (/ (/ 2 (format_scale_kof)) 1) (/ 1 (format_scale_kof)))))
	);progn
      );if
    );while
  (princ)
  )
(defun C:draw_text_be_style_gost ( / t1 a str); Пишет "OTI"
  (terpri)
  (format_check_text_style)
  (initget)(setq t1(getpoint "\nУкажи точку вставки текста:")); указываю точку
  (initget)(setq a (getangle t1 "\nВведи угол поворота или укажи направление:")); указываю угол
  (if (= a nil)
    (setq a (* pi 1.5))
    ); горизонтальное положение текста при пустом вводе
  (setq str "1")
  (while (not (= str nil))
    (initget)(setq str (getstring T "Введи текст: "))
    (if (= str "")
      (progn
      (setq str nil)
      );progn
      (progn
        (entmake (list '(0 . "TEXT") '(6 . "Continuous") (cons 10 t1) (cons 40 (/ (/ 2 (format_scale_kof)) 1)) (cons 1 str) (cons 50 (+ a (/ pi 2))) '(7 . "OTI")))
        (setq t1 (polar t1 a (+ (/ (/ 2 (format_scale_kof)) 1) (/ 1 (format_scale_kof)))))
	);progn
      );if
    );while
  (princ)
  )
(defun draw_text_be_style_rub ( / t1 a str); Пишет OTI
  (terpri)
  (format_check_text_style)
  (initget)(setq t1(getpoint "Укажи точку вставки текста:"))(terpri); указываю точку
  (initget)(setq a (getangle t1 "Введи угол поворота или укажи направление:"))(terpri); указываю угол
  (if (= a nil)
    (setq a (* pi 1.5))
    ); горизонтальное положение текста при пустом вводе
  (setq str "1")
  (while (not (= str nil))
    (initget)(setq str (getstring T "Введи текст: "))
    (if (= str "")
      (progn
      (setq str nil)
      );progn
      (progn
        (entmake (list '(0 . "TEXT") '(6 . "Continuous") (cons 10 t1) (cons 40 (/ (/ 3 (format_scale_kof)) 1.07372)) (cons 1 str) (cons 50 (+ a (/ pi 2))) '(7 . "OTI")))
        (setq t1 (polar t1 a (+ (/ (/ 3 (format_scale_kof)) 1.07372) (/ 1 (format_scale_kof)))))
	);progn
      );if
    );while
  (princ)
  )

(defun C:proba ( / point elevation text)
  (load_global_variable)
  (setq point (car (entsel "\nВыбери пикет: ")))
  (while point
    (setq elevation (vlax-get-property  (vlax-ename->vla-object point) "ELEVATION"))
    (setq text (car (entsel "\nВыбери текст: ")))
    (command "_.UNDO" "_begin")
    (entmod (list (cons -1 text) (cons 1 (rtos elevation 2 3))))
    (entupd text)
    (command "_.UNDO" "_end")
    (setq point (car (entsel "\nВыбери пикет: ")))
    )
  (princ)
  )
(vlax-get (vlax-invoke C3D_points 'find num_point) 'elevation)

(setq  cogopto (vlax-ename->vla-object (CAR (ENTSEL))));;#<VLA-OBJECT IAeccPoint 129c6968>
(vlax-get-property  cogopto "NUMBER");;;-> 3
(vlax-get-property  cogopto "NORTHING");;;-> 36.2532
(vlax-get-property  cogopto "EASTING");;;-> 42.401
(vlax-get-property  cogopto "FullDescription");;;-> "F"
(vlax-get-property  cogopto "ELEVATION");;;-> 46.0
(vlax-PUT-property  cogopto "ELEVATION" 66.0);;;-> 46.0
(vlax-PUT-property  cogopto "LABELSTYLE" "COPY1");;;->nil
(vlax-PUT-property  cogopto "NORTHING" "60");;;->nil

  
(defun C:proba ( / nabor p1 p2 A LIST_A N P pp)
  (setq nabor (ssget '((0 . "LINE"))))
  (setq n 0)
  (repeat (sslength nabor)
    (setq p1 (cdr(assoc 10(entget(ssname nabor n)))))
    (setq p2 (cdr(assoc 11(entget(ssname nabor n)))))
    (setq list_a (append list_a (list(angle p1 p2))))
    (setq n (1+ n))
    )
  (setq a (/(apply '+ list_a)(length list_a)))
  (initget)(setq p(getpoint))
  (setq pp (polar p a 200))
  (entmake (list '(0 . "LINE") '(6 . "Continuous") (cons 10 p) (cons 11 pp)))
  (princ)
  )

(defun C:probaaa ( / patch list_in list_result)
  (setq patch "d:\\классификатор_classif.txt")
  (setq list_in (file_read_to_list patch))
  (setq list_result '())
  (repeat (/(length list_in)2)
    (setq list_result (append list_result (list (strcat (car list_in) "|" (cadr list_in)))))
    (setq list_in (cddr list_in))
    )
  (file_write_list_to_file "d:\\rezult1.txt" list_result)
  )

(defun C:ppppproba ( /
		    koren_lsp
		    koren_atc
		    patch_files_lsp
		    patch_files_atc
		    new_fun_name
		    old_fun_name
		    list_lsp
		    first_str
		    patch
		    patch_atc
		    patch_lsp
		    file_lsp
		    lsp_str
		    list_name
		    list_new_name
		    list_old_name
		    str_new
		    str_old
		    str_atc
		    )
  (setq koren_lsp "d:\\lsp_atc\\lsp")
  (setq koren_atc "d:\\lsp_atc\\atc")
  (setq patch_files_lsp(gt_find_files_be_extension koren_lsp "lsp"))
  (setq patch_files_atc(gt_find_files_be_extension koren_atc "atc"))
  (princ "\n Понеслась!")
  (mapcar '(lambda (patch)
	     ;;;  (setq patch (car patch_files_lsp))
	     ;;;  (terpri)
	     ;;;  (princ patch)
	     (setq new_fun_name (vl-filename-base patch))
	     (setq list_lsp (readfile patch))
	     (setq first_str (car list_lsp))
	     (setq old_fun_name (extract_fun_name_of_str first_str))
	     (if (/= new_fun_name old_fun_name)
	       (progn
		 (princ (strcat "\n"old_fun_name " => " new_fun_name))
		 (setq list_name(append list_old_name(list (strcat old_fun_name " => " new_fun_name))))
		 (setq list_old_name(append list_old_name(list old_fun_name)))
		 (setq list_new_name(append list_new_name(list new_fun_name)))
		 
		 (mapcar '(lambda (patch_atc)
			    (setq str_atc(car (readfile patch_atc)))
			    (while (vl-string-search old_fun_name str_atc)
			      (writefile patch_atc(list(vl-string-subst new_fun_name old_fun_name str_atc)))
			      (setq str_atc(car (readfile patch_atc)))
			      )
			    )
			 patch_files_atc
			 )
		 (mapcar '(lambda (patch_lsp)
			    (setq file_lsp (readfile patch_lsp))
			    (foreach lsp_str file_lsp (setq new_file_lsp (append new_file_lsp(list(vl-string-subst new_fun_name old_fun_name lsp_str)))))
			    (writefile patch_lsp new_file_lsp)
			    (setq new_file_lsp nil)
			    )
			 patch_files_lsp
			 )
		 )
	       )
	     )
	  patch_files_lsp)
  ;;;  (setq list_old_name (acad_strlsort list_old_name))
  ;;;  (foreach n list_old_name (princ (strcat "\n" n)))
  ;;;  (setq str_new "")(setq str_old "")
  ;;;  (foreach n list_new_name (setq str_new (strcat str_new " " n)))
  ;;;  (foreach y list_old_name (setq str_old (strcat str_old " " y)))
  ;;;  (princ "\n Новые!")
  ;;;  (foreach x list_old_name
  ;;;    (if (vl-string-search x str_new)
  ;;;      (princ (strcat "\n"x " == " (substr str_new (-(vl-string-search x str_new)10) 30)))))
  ;;;  (princ "\n старые!")
  ;;;  (foreach x list_old_name
  ;;;    (if (vl-string-search x str_old)
  ;;;      (princ (strcat "\n"x " == " (substr str_old (vl-string-search x str_old) 30)))))
  
  (princ)
  )





(defun extract_fun_name_of_str (str / 1spese 2spese)
  (setq 1spese (+ 2(vl-string-search" "str)))
  (setq 2spese (vl-string-search" " (substr str 1spese (strlen str))))
  (vl-string-trim "C:"(substr str 1spese 2spese))
  )

(defun C:proba ( / koren_txt koren_bmp patch_files_txt file all n button)
  (setq koren_txt "d:\\ToolPalette\\Palettes\\")
  (setq koren_bmp "d:\\ToolPalette\\Palettes\\Images\\")
  (setq patch_files_txt(file_name_list_to_patch (vl-directory-files koren_txt nil 1) koren_txt))
  (mapcar '(lambda (patch)
	     (setq file(open patch "r"))
	     (setq all_text (read-line file))
	     (close file)
	     (setq all all_text)
	     (while (>(strlen all) 100)
	       (setq n(+ 7(vl-string-search "</Tool>" all)))
	       (setq button(substr all 1 n))
	       (setq all(substr all (1+ n) (strlen all)))
	       (if (vl-string-search"<ItemName>"button)
		 (progn
		   (setq name_start(+ 11(vl-string-search"<ItemName>"button)))
		   (setq name_end(1+(vl-string-search"</ItemName>"button)))
		   (setq but_name(substr button name_start (- name_end name_start)))
		   (if (vl-string-search".\\Images\\"button)
		     (progn
		       (setq bmp_start(+ 10(vl-string-search".\\Images\\"button)))
		       (setq bmp_end(vl-string-search"/></Images>"button))
		       (setq bmp_name(substr button bmp_start (- bmp_end bmp_start)))
		       (setq all_text(vl-string-subst (strcat but_name ".bmp") bmp_name all_text))
		       (setq file (open patch "w"))
		       (write-line all_text file)
		       (close file)
		       (vl-file-rename (strcat koren_bmp bmp_name) (strcat koren_bmp but_name ".bmp"))
		       )
		     )
		   )
		 )
	       )
	     )
	  patch_files_txt)
  (princ)
  )

(defun C:proba ( / koren patch_files_bmp patch)
  (setq koren "c:\\Program Files\\geo_tools\\helps\\Images\\")
  (setq patch_files_bmp(file_name_list_to_patch (vl-directory-files koren nil 1) koren))
  (mapcar '(lambda (patch)
  (vl-file-rename patch (edit_translit_str patch))
	     )
	  patch_files_bmp)
  )
	  

(if (vl-string-search (strcat geo_tools_Path x)Support_Paths)
	       (progn
		 (setq n (vl-string-search (strcat ";" geo_tools_Path x)Support_Paths))
		 (setq Support_Paths (strcat (substr Support_Paths 1 n)(substr Support_Paths (+ (1+ n) (strlen ";" geo_tools_Path x)) (strlen Support_Paths))))
		 )
	       )

(defun C:proba ( / usl_z_point lwpolyline spisok point text_point text_str text_obj x line_obj )
  (load_global_variable)
  (setq lwpolyline (vlax-ename->vla-object(car (entsel "\nВыбери полилинию: "))))
  (initget)(setq usl_z_point(getpoint "\nУкажи точку на условном горизонте: "))
  (setq spisok(list_Coordinates_lwpolyline lwpolyline))
  (repeat(length spisok)
    (setq point (car spisok))
    (setq spisok (cdr spisok))
    (setq text_point (list (nth 0 point) (-(nth 1 usl_z_point)5)))
    (setq text_str (rtos(+(-(nth 1 point)(nth 1 usl_z_point))100)2 2))
    (setq text_obj (vla-AddText model_spece text_str (vlax-3D-point text_point) 2.5))
    (vla-put-Alignment text_obj 13)
    (vla-put-TextAlignmentPoint text_obj (vlax-3D-point text_point))
    (vla-put-Rotation text_obj (/ pi 2))
    (vla-put-Linetype text_obj "Continuous")
    (setq list_text_obj (append list_text_obj (list text_obj)))
    (setq line_obj(vla-AddLine model_spece (vlax-3D-point point)(vlax-3D-point (list (nth 0 point) (-(nth 1 usl_z_point)10)))))
    (vla-put-Linetype line_obj "Continuous")
    (setq list_line_obj (append list_line_obj (list line_obj)))
    )
  (setq reaktor-texts (vlr-object-reactor list_text_obj (list usl_z_point lwpolyline) (list '(:vlr-objectClosed . probka))))
  (setq reaktor-lwpolyline (vlr-object-reactor (list lwpolyline) (list usl_z_point reaktor-texts) (list '(:vlr-objectClosed . prob))))
  
  ;;;  (vlr-pers reaktor-lwpolyline)
  (princ)
  )

(defun prob (vlao reac args / data_list usl_z_point spisok point text_point text_str text_obj x line_obj)
  (setq data_list (vlr-data reac))
  (setq usl_z_point (car data_list))
  (setq reaktor-texts (cadr data_list))
  (setq spisok(list_Coordinates_lwpolyline vlao))
  ;;;  (setq list_text_obj (PL:GetD "list_text_obj"))
  ;;;  (setq list_line_obj (PL:GetD "list_line_obj"))
  (vlr-remove reaktor-texts)
  (if list_text_obj(foreach x list_text_obj (vlr-owner-remove reaktor-texts x)))
  (if list_text_obj(foreach x list_text_obj (vla-delete x)))
  (if list_line_obj(foreach x list_line_obj (if x(vla-delete x))))
  (setq list_text_obj nil)
  (setq list_line_obj nil)
  (repeat(length spisok)
    (setq point (car spisok))
    (setq spisok (cdr spisok))
    (setq text_point (list (nth 0 point) (-(nth 1 usl_z_point)5)))
    (setq text_str (rtos(+(-(nth 1 point)(nth 1 usl_z_point))100)2 2))
    (setq text_obj (vla-AddText model_spece text_str (vlax-3D-point text_point) 2.5))
    (vla-put-Alignment text_obj 13)
    (vla-put-TextAlignmentPoint text_obj (vlax-3D-point text_point))
    (vla-put-Rotation text_obj (/ pi 2))
    (vla-put-Linetype text_obj "Continuous")
    (setq list_text_obj (append list_text_obj (list text_obj)))
    (vlr-owner-add reaktor-texts text_obj)
    (setq line_obj(vla-AddLine model_spece (vlax-3D-point point)(vlax-3D-point (list (nth 0 point) (-(nth 1 usl_z_point)10)))))
    (vla-put-Linetype line_obj "Continuous")
    (setq list_line_obj (append list_line_obj (list line_obj)))
    )
  (vlr-add reaktor-texts)
  ;;;  (PL:SetD "list_line_obj" list_line_obj)
  ;;;  (PL:SetD "list_text_obj" list_text_obj)
  (princ)
  )

(defun probka (vlao reac args / data_list usl_z_point lwpolyline n f_point t_point)
  (setq data_list (vlr-data reac))
  (setq usl_z_point (car data_list))
  (setq lwpolyline (cadr data_list))
  (setq n(-(length list_text_obj)(length(member vlao list_text_obj))))
  (if n
    (progn
      (setq f_point(vlax-safearray->list(vlax-variant-value(vla-get-Coordinate lwpolyline n))))
      (setq t_point(list(nth 0 (vlax-safearray->list(vlax-variant-value(vla-get-TextAlignmentPoint vlao))))
			(+(-(atof(vla-get-TextString vlao))100)(nth 1 usl_z_point))))
      (vlr-remove reaktor-lwpolyline)
      (if (not(eq f_point t_point))
	(vla-put-Coordinate lwpolyline n (vlax-safearray-fill (vlax-make-safearray vlax-vbDouble '(0 . 1)) t_point)))
      (vlr-add reaktor-lwpolyline)
      )
    (alert "Фигня какая то...")
    )
  )

(defun list_Coordinates_lwpolyline (lwpolyline / listing list_coord)
  (setq listing(vlax-safearray->list(vlax-variant-value(vla-get-Coordinates lwpolyline))))
  (repeat (/(length listing)2)
    (setq list_coord (append list_coord (list(list (car listing)(cadr listing)))))
    (setq listing (cddr listing)))
  list_coord
  )



(defun C:proba ( / circle_obj text_point text_str text_obj reaktor-circle reaktor-text)
  (load_global_variable)
  (setq circle_obj (vlax-ename->vla-object(car (entsel "\nВыбери круг: "))))
  (setq text_point(vlax-safearray->list(vlax-variant-value(vla-get-Center circle_obj))))
  (setq text_str (rtos(vla-get-Radius circle_obj)2 3))
  (setq text_obj (vla-AddText model_spece text_str (vlax-3D-point text_point) 2.5))
  (vla-put-Alignment text_obj 4)
  (vla-put-TextAlignmentPoint text_obj (vlax-3D-point text_point))
  (vla-put-Rotation text_obj 0)
  (vla-put-Linetype text_obj "Continuous")
  (setq reaktor-circle (vlr-object-reactor (list circle_obj) nil (list '(:vlr-objectClosed . fun-reaktor-circle))))
  (setq reaktor-text (vlr-object-reactor (list text_obj) nil (list '(:vlr-objectClosed . fun-reaktor-text))))
  (vlr-data-set reaktor-circle (list text_obj reaktor-text))
  (vlr-data-set reaktor-text (list circle_obj reaktor-circle))
  (vlr-pers reaktor-circle)
  (vlr-pers reaktor-text)
  (setq flag_t nil)
  (setq flag_c nil)
  (princ)
  )

(defun fun-reaktor-circle (vlao reac args / flag_t)
  (princ "\n RUN fun-reaktor-circle")
  (if flag_c
    (progn
      (princ "\n No flag_c=")
      (princ flag_c)
      (setq flag_c nil)(terpri))
    (progn
      (princ "\n Yes flag_c=")
      (princ flag_c)
      (setq flag_t T)
      (vla-put-TextString (car(vlr-data reac))(rtos(vla-get-Radius vlao)2 3))
      (setq flag_t T)
      (vla-put-TextAlignmentPoint (car(vlr-data reac)) (vlax-variant-value(vla-get-Center vlao)))
      )
    )
  )

(defun fun-reaktor-text (vlao reac args / flag_c)
  (princ "\n RUN fun-reaktor-text")
  (if flag_t
    (progn
      (princ "\n No flag_t=")
      (princ flag_t)
      (setq flag_t nil)(terpri))
    (progn
      (princ "\n Yes flag_t=")
      (princ flag_t)
      (setq flag_c T)
      (vla-put-Radius(car(vlr-data reac))(atof(edit_text_clean_for_number(vla-get-TextString vlao))))
      (setq flag_c T)
      (vla-put-Center(car(vlr-data reac))(vla-get-TextAlignmentPoint vlao))
      )
    )
  )

(defun C:proba ( / circle-obj circle-vla-obj reaktor-circle reaktor-text text-vla-obj text-point text-str)
  (load_global_variable)
  (princ "\nВыбери окружность: ")
  (while (not circle-obj)
    (setq circle-obj (car (entsel)))
    (if (/= "CIRCLE" (cdr (assoc 0 (entget circle-obj))))(setq circle-obj nil)))
  (setq circle-vla-obj (vlax-ename->vla-object circle-obj))
  (setq text-point(vlax-safearray->list(vlax-variant-value(vla-get-Center circle-vla-obj))))
  (setq text-str (rtos(vla-get-Radius circle-vla-obj)2 3))
  (setq text-vla-obj (vla-AddText model_spece text-str (vlax-3D-point text-point) 2.5))
  (vla-put-Alignment text-vla-obj 4)
  (vla-put-TextAlignmentPoint text-vla-obj (vlax-3D-point text-point))
  (vla-put-Rotation text-vla-obj 0)
  (vla-put-Linetype text-vla-obj "Continuous")
  (setq reaktor-circle (vlr-object-reactor (list circle-vla-obj) (list text-vla-obj) (list '(:vlr-modified . catch-fun-reaktor-circle))))
  (setq reaktor-text (vlr-object-reactor (list text-vla-obj) (list circle-vla-obj) (list '(:vlr-modified . catch-fun-reaktor-text))))
  (vlr-pers reaktor-circle)
  (vlr-pers reaktor-text)
  )

(defun catch-fun-reaktor-circle (vlao reac args / )
  (vl-catch-all-apply 'fun-reaktor-circle (list vlao reac args))
  (princ)
  )

(defun catch-fun-reaktor-text (vlao reac args / )
  (vl-catch-all-apply 'fun-reaktor-text (list vlao reac args))
  (princ)
  )

(defun fun-reaktor-circle (vlao reac args / )
  (princ "\n RUN fun-reaktor-circle")
  (vla-put-TextString (car(vlr-data reac))(rtos(vla-get-Radius vlao)2 3))
  (vla-put-TextAlignmentPoint(car(vlr-data reac))(vlax-variant-value(vla-get-Center vlao)))
  )

(defun fun-reaktor-text (vlao reac args / str point)
  (princ "\n RUN fun-reaktor-text")
  (vla-put-Radius(car(vlr-data reac))(atof (edit_text_clean_for_number(vla-get-TextString vlao))))
  (vla-put-Center(car(vlr-data reac))(vla-get-TextAlignmentPoint vlao))
  )

(defun C:proba ( / circle-obj circle-vla-obj reaktor-circle reaktor-text text-vla-obj text-point text-str)
  (load_global_variable)
  (princ "\nВыбери окружность: ")
  (while (not circle-obj)
    (setq circle-obj (car (entsel)))
    (if (/= "CIRCLE" (cdr (assoc 0 (entget circle-obj))))(setq circle-obj nil)))
  (setq circle-vla-obj (vlax-ename->vla-object circle-obj))
  (setq text-point(vlax-safearray->list(vlax-variant-value(vla-get-Center circle-vla-obj))))
  (setq text-str (rtos(vla-get-Radius circle-vla-obj)2 3))
  (setq text-vla-obj (vla-AddText model_spece text-str (vlax-3D-point text-point) 2.5))
  (vla-put-Alignment text-vla-obj 4)
  (vla-put-TextAlignmentPoint text-vla-obj (vlax-3D-point text-point))
  (vla-put-Rotation text-vla-obj 0)
  (vla-put-Linetype text-vla-obj "Continuous")
  (setq reaktor-circle (vlr-object-reactor (list circle-vla-obj) nil (list '(:vlr-modified . fun-reaktor-circle))))
  (setq reaktor-text (vlr-object-reactor (list text-vla-obj) nil (list '(:vlr-modified . fun-reaktor-text))))
  (vlr-data-set reaktor-circle text-vla-obj)
  (vlr-data-set reaktor-text circle-vla-obj)
  (vlr-pers reaktor-circle)
  (vlr-pers reaktor-text)
  )

(defun catch-fun-reaktor-circle (vlao reac args / )
  (vl-catch-all-apply 'fun-reaktor-circle (list vlao reac args))
  (princ)
  )

(defun catch-fun-reaktor-text (vlao reac args / )
  (vl-catch-all-apply 'fun-reaktor-text (list vlao reac args))
  (princ)
  )

(defun fun-reaktor-circle (vlao reac args / )
  (princ "\n RUN fun-reaktor-circle")
  (setq reaktor-text (car(vlr-get-reactors-for-object(vlr-data reac))))
  (vla-put-TextString(car(vlr-owners reaktor-text))(rtos(vla-get-Radius vlao)2 3))
  (vla-put-TextAlignmentPoint(car(vlr-owners reaktor-text))(vlax-variant-value(vla-get-Center vlao)))
  )

(defun fun-reaktor-text (vlao reac args / str point)
  (princ "\n RUN fun-reaktor-text")
  (setq reaktor-circle (car(vlr-get-reactors-for-object(vlr-data reac))))
  (vla-put-Radius(car(vlr-owners reaktor-circle))(atof (edit_text_clean_for_number(vla-get-TextString vlao))))
  (vla-put-Center(car(vlr-owners reaktor-circle))(vla-get-TextAlignmentPoint vlao))
  )

(defun analisis-insert-item-in-list-by-number (new-item input-list number / n old-item result-list)
  (if (> number (length input-list)) (setq number (length input-list)))
  (if (< number 0) (setq number 0))
  (setq n 0)
  (repeat (1+(length input-list))
    (if (eq number n)
      (setq result-list (append result-list (list new-item)))
      (progn
	(setq old-item (car input-list))
	(setq input-list (cdr input-list))
	(setq result-list (append result-list (list old-item)))
	))
    (setq n (1+ n))
    )
  result-list
  )

(defun C:draw_3dpoly_be_2dpoly-os_and_3dpoly-otmetki ( / 2Dpline 3Dpline ttt spisokv2D spisokv3D n n3 sp_dist sp_d min_dist
	       nt1 nt2 nt3 t1 t2 t3 ttt alfa1 alfa2 beta1 beta2 tper list_points t3D t!
	       para n_par t!! flag spisokvALL)
(geo_tools-help "draw_3dpoly_be_2dpoly-os_and_3dpoly-otmetki")
  (initget)(setq 2Dpline (car (entsel "\nВыберите 2D полилинию оси трассы: ")))
  (cond
    ((eq 2Dpline nil)
     (alert "Ничего не выбрано!"))
    ((not(eq (cdr(assoc 0 (entget 2Dpline))) "LWPOLYLINE"))
     (alert "Выбрана не 2D полилиния!"))
    ((eq (cdr(assoc 0 (entget 2Dpline))) "LWPOLYLINE")
     (setq 2Dpline(entget 2Dpline))
     (initget)(setq 3Dpline (car (entsel "\nВыберите 3D полилинию с отметками: ")))
     (cond
       ((eq 3Dpline nil)
	(alert "Ничего не выбрано!"))
       ((not(eq (cdr(assoc 0 (entget 3Dpline))) "POLYLINE"))
	(alert "Выбрана не 3D полилиния!"))
       ((eq (cdr(assoc 0 (entget 3Dpline))) "POLYLINE")
	(initget "Да Нет")(setq Y_N (getkword "\nИнтерполировать точки без отметок? [Да/Нет] <Нет>: "))
	(if(eq Y_N nil)(setq Y_N "Нет"))
	(setq list_points '())
	
	
	;         ВЫЧИСЛЕНИЕ КООРДИНАТ ТОЧЕК СПРОЕЦИРОВАННЫХ НА ОСЬ ТРАССЫ
	
	
	(setq spisokv3D (extract_coord_polyline 3Dpline))
	(setq n3 (length spisokv3D))
	(repeat n3
	  (setq t3D (car spisokv3D))
	  (setq ttt (list (nth 0 t3D) (nth 1 t3D)))
	  (setq spisokv3D (cdr spisokv3D))
	  (setq spisokv2D (extract_coord_lwpolyline 2Dpline))
	  (setq n (length spisokv2D))
	  (setq sp_dist '())
	  (repeat n
	    (setq sp_dist
		   (append sp_dist(list(distance ttt (car spisokv2D))))
		  )
	    (setq spisokv2D (cdr spisokv2D))
	    )
	  (setq sp_d sp_dist)
	  (setq min_dist (car sp_d))
	  (setq sp_d(cdr sp_d))
	  (setq n (length sp_d))
	  (repeat n
	    (setq min_dist (min min_dist (car sp_d)))
	    (setq sp_d (cdr sp_d))
	    )
	  (setq nt2 (vl-position min_dist sp_dist))
	  (setq spisokv2D (extract_coord_lwpolyline 2Dpline))
	  (cond
	    ((= nt2 n) ; последняя точка
	     (setq nt1 (1- nt2))
	     (setq t1 (nth nt1 spisokv2D))
	     (setq t2 (nth nt2 spisokv2D))
	     (setq tper (polar t1 (angle t1 t2) (sqrt (- (expt (distance t1 ttt) 2) (expt (calculation_h_triang_be_2point t1 ttt t2) 2))) ))
	     )
	    ((= nt2 0) ; первая точка
	     (setq nt3 (1+ nt2))
	     (setq t2 (nth nt2 spisokv2D))
	     (setq t3 (nth nt3 spisokv2D))
	     (if(>(calculation_minimal_angle_be_3points ttt t2 t3)(/ pi 2))
	       (setq tper (polar t2 (angle t3 t2) (sqrt (- (expt (distance t2 ttt) 2) (expt (calculation_h_triang_be_2point t2 ttt t3) 2))) ))
	       (setq tper (polar t2 (angle t2 t3) (sqrt (- (expt (distance t2 ttt) 2) (expt (calculation_h_triang_be_2point t2 ttt t3) 2))) ))
	       )
	     )
	    (T
	     (setq nt1 (1- nt2))
	     (setq t1 (nth nt1 spisokv2D))
	     (setq t2 (nth nt2 spisokv2D))
	     (setq nt3 (1+ nt2))
	     (setq t3 (nth nt3 spisokv2D))
	     (setq alfa1 (calculation_minimal_angle_be_3points ttt t1 t2))
	     (setq beta1 (calculation_minimal_angle_be_3points ttt t2 t1))
	     (setq alfa2 (calculation_minimal_angle_be_3points ttt t2 t3))
	     (setq beta2 (calculation_minimal_angle_be_3points ttt t3 t2))
	     (cond
	       ((and (and(< alfa1 (/ pi 2)) (< beta1 (/ pi 2))) (not (and(< alfa2 (/ pi 2)) (< beta2 (/ pi 2))))) ; первый ttt t1 t2
		(setq tper (polar t1 (angle t1 t2) (sqrt (- (expt (distance t1 ttt) 2) (expt (calculation_h_triang_be_2point t1 ttt t2) 2))) ))
		)
	       ((and (not (and(< alfa1 (/ pi 2)) (< beta1 (/ pi 2)))) (and(< alfa2 (/ pi 2)) (< beta2 (/ pi 2)))) ; второй ttt t2 t3
		(setq tper (polar t2 (angle t2 t3) (sqrt (- (expt (distance t2 ttt) 2) (expt (calculation_h_triang_be_2point t2 ttt t3) 2))) ))
		)
	       ((and (< alfa1 (/ pi 2)) (< beta1 (/ pi 2)) (< alfa2 (/ pi 2)) (< beta2 (/ pi 2))) ; оба
		(cond
		  ((> (calculation_h_triang_be_2point t1 ttt t2) (h3p t2 ttt t3))
		   (setq tper (polar t2 (angle t2 t3) (sqrt (- (expt (distance t2 ttt) 2) (expt (calculation_h_triang_be_2point t2 ttt t3) 2))) ))
		   )
		  ((< (calculation_h_triang_be_2point t1 ttt t2) (h3p t2 ttt t3))
		   (setq tper (polar t1 (angle t1 t2) (sqrt (- (expt (distance t1 ttt) 2) (expt (calculation_h_triang_be_2point t1 ttt t2) 2))) ))
		   )
		  ((= (calculation_h_triang_be_2point t1 ttt t2) (h3p t2 ttt t3))
		   (setq tper t2))
		  )
		)
	       ((not (and (< alfa1 (/ pi 2)) (< beta1 (/ pi 2)) (< alfa2 (/ pi 2)) (< beta2 (/ pi 2)))) ; ни один
		(setq tper t2)
		
		)
	       )
	     )
	    )
	  (setq tper (list(list (nth 0 tper) (nth 1 tper) (nth 2 t3D))))
	  (setq list_points (append list_points tper))
	  )
	;               ФОРМИРОВАНИЕ ОБЩЕГО СПИСКА КООРДИНАТ ТОЧЕК ОСИ И ТОЧЕК С ОТМЕТКАМИ
	(terpri)
	(setq spisokvALL (extract_coord_lwpolyline 2Dpline))
	(setq n (length list_points))
	(repeat n
	  (setq flag nil)
	  (setq n_par (length spisokvALL))
	  (setq para 0)
	  (setq t!! (car list_points));трехмерная точка
	  (setq t!(list (nth 0 t!!) (nth 1 t!!) ));двухмерная точка
	  (setq list_points(cdr list_points))
	  (while (not flag) ; определение пары в которую надо добавить координатную точку
	    (cond
	      ((= para 0)
	       (setq p1 (nth 0 spisokvALL))
	       (setq p2 (nth 1 spisokvALL)))
	      ((= para n_par)
	       (setq p1 (nth (- para 2) spisokvALL))
	       (setq p2 (nth (1- para) spisokvALL)))
	      ((and (> para 0)(< para n_par))
	       (setq p1 (nth (1- para) spisokvALL))
	       (setq p2 (nth para spisokvALL)))
	      (T
	       (exit))
	      )
	    (if(or
		 (and (= para 0)(equal (angle t! p1) (angle p1 p2)(/ pi 648000))(> (distance p2 t!)(distance p2 p1)))
		 (and (= para n_par)(equal (angle t! p2) (angle p2 p1)(/ pi 648000))(> (distance p1 t!)(distance p1 p2)))
		 (and (and (> para 0)(< para n_par))(equal (angle t! p2) (angle p1 p2)(/ pi 648000))(< (distance p1 t!)(distance p1 p2)))
		 )
	      (setq flag T)
	      (setq flag nil)
	      )
	    
	    (if (eq flag nil)
	      (setq para (1+ para)))
	    (setq p1 nil)
	    (setq p2 nil)
	    )
	  (setq list_f_points nil)
	  (repeat para
	    (setq list_f_points (append list_f_points (list (car spisokvALL))))
	    (setq spisokvALL(cdr spisokvALL))
	    )
	  (setq spisokvALL (cons t!! spisokvALL))
	  (setq list_f_points (reverse list_f_points))
	  (repeat para
	    (setq spisokvALL(cons (car list_f_points) spisokvALL))
	    (setq list_f_points (cdr list_f_points))
	    )
	  )
	;         ЧИСТКА ТОЧЕК БЕЗ ОТМЕТКИ ИМЕЮЩИХ АНАЛОГ С ОТМЕТКОЙ
	(setq spisok_ver spisokvALL)
	(setq n (length spisokvALL))
	(repeat (1- n)
	  (setq p1 (car spisok_ver))
	  (setq spisok_ver (cdr spisok_ver))
	  (setq p2 (car spisok_ver))
	  (if (and (eq (nth 0 p1)(nth 0 p2))(eq (nth 1 p1)(nth 1 p2)))
	    (cond
	      ((eq (nth 2 p1) nil)
	       (setq spisokvALL (vl-remove p1 spisokvALL)))
	      ((eq (nth 2 p2) nil)
	       (setq spisokvALL (vl-remove p2 spisokvALL)))
	      )
	    )
	  )
	;                        ИНТЕРПОЛЯЦИЯ ТОЧЕК БЕЗ ОТМЕТОК
	(if
	  (eq Y_N "Да")
	  (progn
	    (setq n (length spisokvALL))
	    (setq n0 0)
	    (repeat n
	      (cond
		((and (= n0 0) (= (nth 2 (nth n0 spisokvALL)) nil)); первая
		 (setq flag2 nil)
		 (setq n00 (1+ n0))
		 (while (not flag2)
		   (if (= (nth 2 (nth n00 spisokvALL)) nil)
		     (setq n00 (1+ n00))
		     (progn
		       (setq spisokvALL
			      (subst (list (nth 0 (nth n0 spisokvALL)) (nth 1 (nth n0 spisokvALL)) (nth 2(nth n00 spisokvALL)))
				     (nth n0 spisokvALL)
				     spisokvALL)
			     )
		       (setq flag2 T)
		       )
		     ); if
		   )
		 )
		((and (= n0 (1- n)) (= (nth 2 (nth n0 spisokvALL)) nil)); последняя
		 (setq flag2 nil)
		 (setq n00 (1- n0))
		 (while (not flag2)
		   (if (= (nth 2 (nth n00 spisokvALL)) nil)
		     (setq n00 (1- n00))
		     (progn
		       (setq spisokvALL
			      (subst (list (nth 0 (nth n0 spisokvALL)) (nth 1 (nth n0 spisokvALL)) (nth 2(nth n00 spisokvALL)))
				     (nth n0 spisokvALL)
				     spisokvALL)
			     )
		       (setq flag2 T)
		       )
		     ); if
		   )
		 )
		((and (and (> n0 0) (< n0 (1- n)))(= (nth 2 (nth n0 spisokvALL)) nil)); серединые
		 (setq p1 (nth (1- n0) spisokvALL))
		 (setq H1 (nth 2 p1))
		 (setq s1 (distance (nth n0 spisokvALL) p1))
		 (setq n00 (1+ n0))
		 (setq s2 (distance (list(nth 0(nth n0 spisokvALL))(nth 1(nth n0 spisokvALL)))
				    (list(nth 0(nth n00 spisokvALL))(nth 1(nth n00 spisokvALL)))
				    ))
		 (while (= (nth 2 (nth n00 spisokvALL)) nil)
		   (setq s2 (+ s2 (distance (list(nth 0(nth n00 spisokvALL))(nth 1(nth n00 spisokvALL)))
					    (list(nth 0(nth (1+ n00) spisokvALL))(nth 1(nth (1+ n00) spisokvALL))))))
		   (setq n00 (1+ n00))
		   (if (= n00 n)
		     (setq n00 (1- n0)))
		   )
		 (setq p2 (nth n00 spisokvALL))
		 (setq H2 (nth 2 p2))
		 (setq H (+(*(/(- H2 H1)(+ s1 s2))s1)H1))
		 (setq spisokvALL
			(subst (list (nth 0 (nth n0 spisokvALL)) (nth 1 (nth n0 spisokvALL)) H)
			       (nth n0 spisokvALL)
			       spisokvALL)
		       )
		 )
		)
	      (setq n0 (1+ n0))
	      )
	    )
	  )
	(draw_pline_entmake spisokvALL nil T 0 0)
	(princ)
	)
       )
     )
    )
  )

(defun C:proba ( / axis-trace-obj elevation-3dpoly-obj general-list-points)
  (setq axis-trace-obj (vlax-ename->vla-object (car (entsel))))
  (setq elevation-3dpoly-obj (vlax-ename->vla-object (car (entsel))))
  (setq general-list-points (analisis-3d-and-2d-poly-by-general-3dpoly axis-trace-obj elevation-3dpoly-obj))
  (vla-add3dPoly model_spece
    (vlax-safearray-fill
      (vlax-make-safearray vlax-vbDouble (cons 0 (1-(*(length general-list-points)3))))
      (apply 'append (mapcar 'list (mapcar 'car general-list-points)
			     (mapcar 'cadr general-list-points)
			     (mapcar 'caddr general-list-points)))))
  (princ)
  )


(defun C:proba ( / text-obj reaktor-text-obj)
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  (setq text-obj (vlax-ename->vla-object (car (entsel "\nВыбери текст: "))))
  (vlr-object-reactor(list text-obj)nil(list '(:vlr-modified . fun-obj)))
  (vlr-editor-reactor nil '((:vlr-commandended . fun-comm)))
  (vlr-command-reactor nil(list '(:vlr-commandEnded . fun-comm)))
  )
(defun fun-obj (obj-run-reactor current-reactor arguments / )
  (princ "\n RUN fun-obj")
  (setq temp obj-run-reactor)
  )
(defun fun-comm (current-reactor arguments / )
  (princ "\n RUN fun-comm")
  (if temp
    (progn
      (princ "\n YES")
      (vla-put-TextString temp(edit_text_clean_for_number(vla-get-TextString temp)))
      (setq temp nil))(princ "\n NO")))


(defun C:inport_SDR33file_to_be_cogo_points ( / ss list_points_obj obj_vba points_list list_points_obj old_points_list new_points_list file_points_list osn putSDR33file SDR33file str n nn flag)
  (geo_tools-help "inport_SDR33file_to_be_cogo_points")
  ;;;  (setq mode (dialog-import-sdr-mode))
  ;;;  (if mode
  ;;;    (progn
  (load_global_variable)
  (setq osn (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setq putSDR33file (getfiled "Выбор файла SDR33" (getvar "DWGPREFIX") "txt;sdr" 0))
  (setq FileName(substr putSDR33file (+(vl-string-position 92 putSDR33file 0 T)2) (-(strlen putSDR33file)(+(vl-string-position 92 putSDR33file 0 T)2)3)))
  (setq SDR33file (open putSDR33file"r"))
  (setq str (read-line SDR33file))
  (while (not(or(eq str nil)(eq (substr str 1 4)"000")))
    (if (or(eq (substr str 1 4) "08TP") (eq (substr str 1 4) "08KI") (eq (substr str 1 4) "02TP"))
      (progn
	(setq file_points_list
	       (append file_points_list
		       (list (list
			       (vl-string-trim" "(substr str 5 16)); номер
			       (atof(vl-string-trim" "(substr str 21 16))); X
			       (atof(vl-string-trim" "(substr str 37 16))); Y
			       (atof(vl-string-trim" "(substr str 53 16))); H
			       ;;;			       (vl-string-trim" "(substr str 69 16)); описание
			       ))))))
    (setq str (read-line SDR33file))
    )
  (close SDR33file)
  (if file_points_list
    (progn
      (setq file_points_list (reverse file_points_list))
      (setq ss (ssget "X" '((0 . "AECC_COGO_POINT"))))
      (if ss
	(progn
	  (setq list_points_obj (convert_ss_to_list ss))
	  (mapcar '(lambda (x)
		     (setq obj_vba(vlax-ename->vla-object x))
		     (setq old_points_list
			    (append old_points_list
				    (list (list
					    (vlax-get-property  obj_vba "FullDescription")
					    (vlax-get-property  obj_vba "NORTHING")
					    (vlax-get-property  obj_vba "EASTING")
					    (vlax-get-property  obj_vba "ELEVATION")
					    ))))
		     )
		  list_points_obj)
	  (setq n 0)
	  (while (not(eq n (length file_points_list)))
	    (setq new_point (nth n file_points_list))
	    (setq nn 0)
	    (setq flag nil)
	    (while (not(eq nn (length old_points_list)))
	      (if (equal new_point (nth nn old_points_list))
		(progn
		  (setq nn (length old_points_list))
		  (setq flag T)
		  )
		(setq nn (1+ nn))
		)
	      )
	    (if (eq flag nil)
	      (setq new_points_list(append new_points_list(list new_point)))
	      )
	    (setq n (1+ n))
	    )
	  )
	(setq new_points_list file_points_list)
	)
      (if new_points_list
	(progn
	  (if
	    (eq(tblsearch "LAYER" FileName)nil)
	    (command "_-LAYER" "с" FileName "")
	    (command "_-LAYER" "_S" FileName "")
	    )
	  (mapcar '(lambda (x)
		     (vlax-put-property(vla-add C3D_points (vlax-3d-point (caddr x)(cadr x)(cadddr x)))'Description (car x))
		     )
		  new_points_list)
	  )
	(alert "Не импортировано ни одной новой точки!")
	)
      )
    (alert "В выбраном файле координат не обнаружено!")
    )
  (setvar "OSMODE" osn)
  ;;;  )
  ;;;    )
  ;;;  (princ "\n СТАРОЕ: ")(princ old_points_list)
  ;;;  (princ "\n ИЗ ФАЙЛА: ")(princ file_points_list)
  ;;;  (princ "\n НОВОЕ: ")(princ new_points_list)
  (princ)
  )

(defun C:proba ()
  (profile-draw-block-by-view-basement
    (nth (profile-editor-view-basement 0) (get-sorting-list-profile-dat))
    "Название профиля"
    (getpoint)
    )
  (princ)
  )

(defun C:proba (/ *error*)
  
  (defun *error* (msg)
    (princ "\n Кто то нажал ESC!")
    (alert "Кто то нажал ESC!")
    )
  
  (getpoint)
  
  (princ)
  )


(defun C:proba ()
  (setq x (vlax-ename->vla-object(car(entsel))))
  (if (> (vla-get-LeaderCount x) 0)
    (progn
  (setq point_list (vlax-safearray->list(vlax-variant-value(vla-GetLeaderLineVertices x 0))))
  (princ  (append point_leaders_list (list (list (car point_list)(cadr point_list)))))
  ))
  
  (princ)
  )

    (list
      "Название вида профиля"
      (list "1" "***Текущий***" "4" "20" "80" "Условный горизонт" "М горизонтальный" "М вертикальный" "%%uПродольный профиль")
      (list "№№ точек" "name-points"  "10" "-" "-" "-" "2.5" "1" "-")
      (list "Отметки" "elevations"  "15" "-" "2" "-" "2.5" "-" "-")
      (list "Глубина заложения" "deep"  "10" "-" "2" "-" "2.5" "-" "-")
      (list "Горизонтальные проложения" "distance" "10" "-" "2" "-" "2.5" "-" "-")
      (list "Длина/Уклон" "dist-downgrade" "15" "0" "2" "3" "2.5" "-" "-")
      (list "Развернутый план трассы" "map-trace" "40" "-" "2" "-" "2.5" "-" "1")
      (list "Углы поворота" "angle-trace"  "10" "-" "2" "-" "2.5" "-" "1")
      (list "ПИКЕТ" "pikets" "15" "-" "2" "-" "2.5" "-" "-")
      (list "Текстовая строка" "text-str" "10" "-" "-" "-" "-" "-" "-")
      )
  (vlax-ldata-get "geo_tools_dictionary" "profiles-list")
  (vlax-ldata-get "geo_tools_dictionary" "curent-profile")
  (vlax-ldata-put "geo_tools_dictionary" "curent-profile" horizon-elevation)

(defun dialog_export_table_to_sdr (table_name spisok / dcl_id data_dialog_export_table_to_sdr
				   flag1 flag2 flag3 flag4 flag5
				   symbol1 symbol2 select1 select2 select3)
  (setq data_dialog_export_table_to_sdr (PL:GetD "data_dialog_export_table_to_sdr"))
  (if (eq data_dialog_export_table_to_sdr nil)
    (setq data_dialog_export_table_to_sdr (list 1 1 1 1 1 "_" "_" 0 1 2)))
  (setq flag1 (nth 0 data_dialog_export_table_to_sdr))
  (setq flag2 (nth 1 data_dialog_export_table_to_sdr))
  (setq flag3 (nth 2 data_dialog_export_table_to_sdr))
  (setq flag4 (nth 3 data_dialog_export_table_to_sdr))
  (setq flag5 (nth 4 data_dialog_export_table_to_sdr))
  (setq symbol1 (nth 5 data_dialog_export_table_to_sdr))
  (setq symbol2 (nth 6 data_dialog_export_table_to_sdr))
  (setq select1 (nth 7 data_dialog_export_table_to_sdr))
  (setq select2 (nth 8 data_dialog_export_table_to_sdr))
  (setq select3 (nth 9 data_dialog_export_table_to_sdr))
  (setq dcl_id (load_dialog "Диалоговые окна.dcl"))
  (new_dialog "d_export_table_to_sdr" dcl_id)
  (set_tile "flag1" (itoa flag1))
  (set_tile "flag2" (itoa flag2))
  (set_tile "flag3" (itoa flag3))
  (set_tile "flag4" (itoa flag4))
  (set_tile "flag5" (itoa flag5))
  (set_tile "symbol1" symbol1)
  (set_tile "symbol2" symbol2)
  (set_tile "select1" (itoa select1))
  (set_tile "select2" (itoa select2))
  (set_tile "select3" (itoa select3))
  (set_tile_text_str_for_dialog_export_table_to_sdr
    (list flag1 flag2 flag3 flag4 flag5 symbol1 symbol2 select1 select2 select3)
    table_name spisok)
  (action_tile "flag1" "(if (= flag1 0)(setq flag1 1)(setq flag1 0))(set_tile_text_str_for_dialog_export_table_to_sdr
									(list flag1 flag2 flag3 flag4 flag5 symbol1 symbol2 select1 select2 select3)
									table_name spisok)")
  (action_tile "flag2" "(if (= flag2 0)(setq flag2 1)(setq flag2 0))(set_tile_text_str_for_dialog_export_table_to_sdr
									(list flag1 flag2 flag3 flag4 flag5 symbol1 symbol2 select1 select2 select3)
									table_name spisok)")
  (action_tile "flag3" "(if (= flag3 0)(setq flag3 1)(setq flag3 0))(set_tile_text_str_for_dialog_export_table_to_sdr
									(list flag1 flag2 flag3 flag4 flag5 symbol1 symbol2 select1 select2 select3)
									table_name spisok)")
  (action_tile "flag4" "(if (= flag4 0)(setq flag4 1)(setq flag4 0))(set_tile_text_str_for_dialog_export_table_to_sdr
									(list flag1 flag2 flag3 flag4 flag5 symbol1 symbol2 select1 select2 select3)
									table_name spisok)")
  (action_tile "flag5" "(if (= flag5 0)(setq flag5 1)(setq flag5 0))(set_tile_text_str_for_dialog_export_table_to_sdr
									(list flag1 flag2 flag3 flag4 flag5 symbol1 symbol2 select1 select2 select3)
									table_name spisok)")
  (action_tile "symbol1" "(setq symbol1(get_tile\"symbol1\"))(set_tile_text_str_for_dialog_export_table_to_sdr
									(list flag1 flag2 flag3 flag4 flag5 symbol1 symbol2 select1 select2 select3)
									table_name spisok)")
  (action_tile "symbol2" "(setq symbol2(get_tile\"symbol2\"))(set_tile_text_str_for_dialog_export_table_to_sdr
									(list flag1 flag2 flag3 flag4 flag5 symbol1 symbol2 select1 select2 select3)
									table_name spisok)")
  (action_tile "select1" "(setq select1 (atoi $value))(set_tile_text_str_for_dialog_export_table_to_sdr
									(list flag1 flag2 flag3 flag4 flag5 symbol1 symbol2 select1 select2 select3)
									table_name spisok)")
  (action_tile "select2" "(setq select2 (atoi $value))(set_tile_text_str_for_dialog_export_table_to_sdr
									(list flag1 flag2 flag3 flag4 flag5 symbol1 symbol2 select1 select2 select3)
									table_name spisok)")
  (action_tile "select3" "(setq select3 (atoi $value))(set_tile_text_str_for_dialog_export_table_to_sdr
									(list flag1 flag2 flag3 flag4 flag5 symbol1 symbol2 select1 select2 select3)
									table_name spisok)")
  (action_tile "save" "(if (= flag1 flag2 flag3 flag4 flag5 0)(alert\"Внимание!\nНедостаточно данных!\nВсе выключено!\")(done_dialog 2))")
  (if (eq (start_dialog) 2)
    (progn
      (setq data_dialog_export_table_to_sdr
	     (list flag1 flag2 flag3 flag4 flag5 symbol1 symbol2 select1 select2 select3))
      (PL:SetD "data_dialog_export_table_to_sdr" data_dialog_export_table_to_sdr)
      data_dialog_export_table_to_sdr
      )
    )
  )


(defun edit_list_to_strcat (com_list list_shel table_name /)
  (strcat
    "08TP"
    (edit_str_adding_probel
      (edit_str_trim_for_nunber_symbols
	(edit_translit_str
	  (strcat
	    (if(eq(nth 0 com_list)1)
	      (cond
		((eq(nth 7 com_list)0)table_name)
		((eq(nth 7 com_list)1)(nth 0 list_shel))
		((eq(nth 7 com_list)2)(nth 1 list_shel)))
	      "")
	    (if(eq(nth 1 com_list)1)
	      (nth 5 com_list)
	      "")
	    (if(eq(nth 2 com_list)1)
	      (cond
		((eq(nth 8 com_list)0)table_name)
		((eq(nth 8 com_list)1)(nth 0 list_shel))
		((eq(nth 8 com_list)2)(nth 1 list_shel)))
	      "")
	    (if(eq(nth 3 com_list)1)
	      (nth 6 com_list)
	      "")
	    (if(eq(nth 4 com_list)1)
	      (cond
		((eq(nth 9 com_list)0)table_name)
		((eq(nth 9 com_list)1)(nth 0 list_shel))
		((eq(nth 9 com_list)2)(nth 1 list_shel)))
	      "")
	    )
	  )
	15
	nil)
      16 T)
    (edit_str_adding_probel (nth 2 list_shel) 16 nil)
    (edit_str_adding_probel (nth 3 list_shel) 16 nil)
    (edit_str_adding_probel "0.000" 16 nil)
    (edit_str_adding_probel " " 16 nil)
    )
  )


(defun C:export_table_to_SDR33file ( / n table_obj test_str spisok putTXTfile putSDR33file x com_list)
(geo_tools-help "export_table_to_SDR33file")
  (initget 1)(setq table_obj (car (entsel "\nВыбери существующую таблицу с координатами: ")))
  (setq table_obj (vlax-ename->vla-object table_obj))
  (setq n 2)
  (while (not(eq n (vla-get-rows table_obj)))
    (setq test_str (vla-GetText table_obj n 0 ))
    (if (not (eq test_str ""))
      (if (wcmatch (substr test_str 1 1) "#")
	(progn
	  (setq spisok
		 (append spisok
			 (list (list
				 (vla-GetText table_obj n 0 )
				 (vla-GetText table_obj n 1 )
				 (vla-GetText table_obj n 2 )
				 (vla-GetText table_obj n 3 )
				 ))))
	  )
	)
      )
    (setq n (1+ n))
    )
  (setq perffix(vla-GetText table_obj 0 0))
  (setq com_list(dialog_export_table_to_sdr perffix spisok))
  (if com_list
    (progn
      (setq putTXTfile(strcat(getvar "DWGPREFIX")(substr (getvar "DWGNAME") 1 (-(strlen (getvar "DWGNAME"))4)) "_" perffix))
      (setq putSDR33file (getfiled "Имя сохраняемого файла..." putTXTfile "sdr" 1))
      (setq SDR33file (open putSDR33file "w"))
      (mapcar '(lambda (x)(write-line(edit_list_to_strcat com_list x perffix)SDR33file))
	      spisok)
      (close SDR33file)
      )
    )
  (princ)
  )


  (vlax-ldata-get "geo_tools_dictionary" "curent-profile")
  (vlax-ldata-put "geo_tools_dictionary" "curent-profile" horizon-elevation)

(defun C:proba (/ *error*)
  
  (defun *error* (msg)
    (princ "\n Кто то нажал ESC!")
    (alert "Кто то нажал ESC!")
    )
  
  (getpoint)
  
  (princ)
  )

(command "_insert" name-block (command))
(vlax-ldata-get(vlax-ename->vla-object(car(entsel)))"parameters-list")
(vlax-ldata-get (vlax-ename->vla-object (car(entsel))) "profile-name")
(vlax-ldata-get (vlax-ename->vla-object (car(entsel))) "data-list-dialog")
(vlax-ldata-get (vlax-ename->vla-object (car(entsel))) "obj-list")
(assoc "horizontal-lin-list"(vlax-ldata-get (vlax-ename->vla-object (car(entsel))) "obj-list"))

(defun C:proba (/ obj)
  (setq obj(cdr(assoc "block"(vlax-ldata-get (cdr(vlax-ldata-get "geo_tools_dictionary" "curent-profile"))"obj-list"))))
  (vla-put-Linetype
    (vla-AddLine obj
      (vlax-3D-point (list 0 0))
      (vlax-3D-point (mapcar '+ (list 0 0) (list  10 10)))
      )"Continuous")
  (vla-Update(cdr(vlax-ldata-get "geo_tools_dictionary" "curent-profile")))
  )


(defun C:proba ()
  (load_global_variable)
  (apply 'max
	 (mapcar 'caddr
		 (vl-remove-if
		   (function (lambda (x) (if (eq (caddr x)0)T nil)))
		   (analisis-3d-and-2d-poly-by-general-3dpoly
		     (vlax-ename->vla-object (car(entsel)))
		     (vlax-ename->vla-object (car(entsel))))
		   )
		 )
	 )
  (foreach x (analisis-intersect-obj (vlax-ename->vla-object (car(entsel)))(vlax-ename->vla-object (car(entsel))))
    (draw-circle-vla model_spece x 1)
    )
  )


(defun C:proba ( / ANG BLOCK LENG LINE P1 P2)
  (if(setq line(car(entsel)))
    (progn
      (setq line (vlax-ename->vla-object line))
      (setq leng (vla-get-Length line))
      (setq p1 (vlax-safearray->list(vlax-variant-value(vla-get-StartPoint line))))
      (setq p2 (vlax-safearray->list(vlax-variant-value(vla-get-EndPoint line))))
      (setq ang (analysis_angle_be_long_distance (angle p1 p2)))
      (draw_text_vla model_spece (rtos leng 2 2) (getpoint) (/ 2.0 (getvar "CANNOSCALEVALUE")) nil ang T T T)
      )
    )
  )


(defun C:proba ( / mleader_obj old-obj)
  (load_global_variable)
  (if(setq old-obj(car(entsel)))
    (progn
      (setq old-obj (vlax-ename->vla-object old-obj))
      (setq mleader_obj (vla-addmleader model_spece (vla-GetLeaderLineVertices old-obj 0)0))
      (vla-put-StyleName mleader_obj (vla-get-StyleName old-obj))
      (vla-put-textstring mleader_obj (vla-get-TextString old-obj))
      (vla-put-TextHeight mleader_obj (vla-get-TextHeight old-obj))
      (vla-put-TextStyleName mleader_obj (vla-get-TextStyleName old-obj))
      (vla-put-Layer mleader_obj (vla-get-Layer old-obj))
      (vla-put-Color mleader_obj (vla-get-Color old-obj))
      (vla-delete old-obj)
      )
    )
  )


(defun C:proba ()
  (load_global_variable)
   (draw-lwpolyline-vla-by-list-property
			     (list(cons "spece" model_spece)
				  (cons "Coordinates" 
					(convert-safearray-to-list-points
					  (vlax-variant-value
					    (vla-get-Coordinates
					      (vlax-ename->vla-object
						(car(entsel))))) 3)
					)
				  (cons "Linetype" "Continuous")
				  ))
  )

(defun C:proba (/ obj obj-v list-entget result-list)
  (princ(entget(setq obj(car(entsel)))))
  (setq obj-v(entnext obj))
  (while (not(eq obj-v nil))
    (setq list-entget(entget obj-v))
    (terpri)
    (princ list-entget)
    (setq obj-v(entnext obj-v))
    (if (= (cdr (assoc 0 list-entget)) "VERTEX")
      (setq result-Face-list(append result-Face-list (list (list(cdr (assoc 100 list-entget))
							     (cdr (assoc 10 list-entget))
								(cdr (assoc 71 list-entget))
								(cdr (assoc 72 list-entget))
								(cdr (assoc 73 list-entget))
								(cdr (assoc 74 list-entget))))))
      )
    )
  (princ result-Face-list)
  (princ)
  )
  (princ result-list)
    (if (= (cdr (assoc 0 list-entget)) "POLYLINE")
      (progn
	(setq obj-v(entnext obj-v))
	(while (not(eq obj-v nil))
	  (setq list-entget(entget obj-v))
	  (if (= (cdr (assoc 0 list-entget)) "VERTEX")
	    (setq result-list(append result-list (list (cdr (assoc 10 list-entget)))))
	    )
	  (setq obj-v(entnext obj-v))
	  )
	)
      )
    (if (not(eq obj-v nil))
    (setq obj-v(entnext obj-v))
      )
    (if (= (cdr (assoc 0 list-entget)) "AcDbFaceRecord")
      (setq result-Face-list(append result-Face-list (list (list(cdr (assoc 71 list-entget))
								(cdr (assoc 72 list-entget))
								(cdr (assoc 73 list-entget))
								(cdr (assoc 74 list-entget))))))
      )
    (if (= (cdr (assoc 0 list-entget)) "AcDbFaceRecord")
      (setq result-point-list(append result-point-list (list (cdr (assoc 10 list-entget)))))
      )

(defun C:proba (/
		curent-pline face-list flag-all flag-pl line-list list-entget list-pline number obj obj-v vertex-list
		nabor SUM
		)
  (load_global_variable)
  (vla-StartUndoMark active_document)
  (setq nabor(ssget "_X" '((0 . "POLYLINE"))))
  (setq nabor(converting_ss_to_list nabor))
  (terpri)
  (setq sum (length nabor))
  (princ (strcat"\r  1/"(itoa(length nabor))))
  (repeat sum
    (setq obj (car nabor))
    (setq nabor(cdr nabor))
    (princ (strcat"\r  "(itoa(- sum (length nabor)))"/"(itoa sum)))
    (if (= (cdr (nth 10 (entget obj))) "AcDbPolyFaceMesh")
      (progn
	(setq obj-v(entnext obj))
	(setq list-entget(entget obj-v))
	(while (not(= (cdr (assoc 0 list-entget)) "SEQEND"))
	  (if (= (cdr (nth 10 list-entget)) "AcDbVertex")
	    (setq vertex-list(append vertex-list (list (cdr (assoc 10 list-entget)))))
	    (setq face-list(append face-list (list (list (cdr (assoc 71 list-entget))
							 (cdr (assoc 72 list-entget))
							 (cdr (assoc 73 list-entget))
							 (cdr (assoc 74 list-entget))))))
	    )
	  (setq obj-v(entnext obj-v))
	  (setq list-entget(entget obj-v))
	  )
	(foreach x face-list
	  (setq line-list(append line-list (list(list (nth(1-(abs(nth 0 x)))vertex-list)(nth(1-(abs(nth 3 x)))vertex-list))
						(list (nth(1-(abs(nth 1 x)))vertex-list)(nth(1-(abs(nth 2 x)))vertex-list))
						)))
	  )
	(setq vertex-list nil
	      face-list nil)
	(gc)
	(while (not flag-all)
	  (setq number 0)
	  (setq flag-pl nil)
	  (setq curent-pline (car line-list))
	  (setq line-list(cdr line-list))
	  (if (eq line-list nil)
	    (setq flag-pl T)
	    )
	  (while (not flag-pl)
	    (cond
	      ((and (= (nth 0 (nth 0 curent-pline)) (nth 0 (nth 0 (nth number line-list))))
		    (= (nth 1 (nth 0 curent-pline)) (nth 1 (nth 0 (nth number line-list)))))
	       (setq curent-pline(cons (nth 1 (nth number line-list)) curent-pline))
	       (setq line-list(vl-remove(nth number line-list)line-list))
	       (setq number 0)
	       )
	      ((and (= (nth 0 (nth 0 curent-pline)) (nth 0 (nth 1 (nth number line-list))))
		    (= (nth 1 (nth 0 curent-pline)) (nth 1 (nth 1 (nth number line-list)))))
	       (setq curent-pline(cons (nth 0 (nth number line-list)) curent-pline))
	       (setq line-list(vl-remove(nth number line-list)line-list))
	       (setq number 0)
	       )
	      ((and (= (nth 0 (nth (1-(length curent-pline)) curent-pline)) (nth 0 (nth 0 (nth number line-list))))
		    (= (nth 1 (nth (1-(length curent-pline)) curent-pline)) (nth 1 (nth 0 (nth number line-list)))))
	       (setq curent-pline(append curent-pline(list(nth 1 (nth number line-list)))))
	       (setq line-list(vl-remove(nth number line-list)line-list))
	       (setq number 0)
	       )
	      ((and (= (nth 0 (nth (1-(length curent-pline)) curent-pline)) (nth 0 (nth 1 (nth number line-list))))
		    (= (nth 1 (nth (1-(length curent-pline)) curent-pline)) (nth 1 (nth 1 (nth number line-list)))))
	       (setq curent-pline(append curent-pline(list(nth 0 (nth number line-list)))))
	       (setq line-list(vl-remove(nth number line-list)line-list))
	       (setq number 0)
	       )
	      (T
	       (setq number (1+ number))
	       )
	      )
	    (if(or(eq line-list nil)
		  (> number(1-(length line-list)))
		  )
	      (setq flag-pl T)
	      )
	    (bump!)
	    )
	  (setq list-pline(append list-pline(list curent-pline)))
	  (if (eq line-list nil)
	    (setq flag-all T)
	    )
	  )
	(foreach x list-pline
	  (draw-lwpolyline-vla-by-list-property
	    (list(cons "spece" model_spece)
		 (cons "Coordinates" x)
		 (cons "Layer" (cdr (assoc 8 (entget obj))))
		 (cons "Color" (cdr (assoc 62 (entget obj))))
		 (cons "Linetype" "Continuous")
		 ))
	  )
	(entdel obj)
	)
      )
    (setq list-pline nil
	  obj nil
	  obj-v nil
	  curent-pline nil
	  flag-all nil
	  flag-pl nil)
    )
  (vla-EndUndoMark active_document)
  (princ)
  )

(defun converting_ss_to_list ( nabor / n _list) ; конвертирует набор в список
     (setq n 0)
     (if nabor
       (progn
	 (repeat (sslength nabor)
	   (setq _list (append _list (list(ssname nabor n))))
	   (setq n (1+ n)))
	 )
       )
  _list
  )

(setq bcnt 0)
(defun bump! (/)
  (princ
    (nth bcnt '("\r-" "\r\\" "\r|" "\r/"))
    )
  (setq bcnt (rem (1+ bcnt) 4))
  )

(alert "Проблема!!! полилиния меньше 3х точек!")
  (entdel obj)
  (setq ssnab(ssadd))
  (if (> (sslength ssnab) 1)
  (if (and (getvar "PEDITACCEPT") (= (getvar "PEDITACCEPT") 1))
    (vl-cmdf "_pedit" "_Multiple" ssnab "" "_Join" 0 "")
    (vl-cmdf "_pedit" "_Multiple" ssnab "" "_Y" "_Join" 0 ""))
  )
    (entmake (list '(0 . "LINE") '(8 . "Ситуация") '(6 . "Continuous")
		   (cons 10 (nth(1-(abs(nth 0 x)))vertex-list))
		   (cons 11 (nth(1-(abs(nth 3 x)))vertex-list))))
    (setq ssnab(ssadd (entlast)ssnab))
    (entmake (list '(0 . "LINE") '(8 . "Ситуация") '(6 . "Continuous")
		   (cons 10 (nth(1-(abs(nth 1 x)))vertex-list))
		   (cons 11 (nth(1-(abs(nth 2 x)))vertex-list))))
    (setq ssnab(ssadd (entlast)ssnab))
    (setq ssnab(ssadd(vlax-vla-object->ename
		       (draw-line-vla-by-list-property
			 (list
			   (cons "spece" model_spece)
			   (cons "StartPoint" (nth(1-(abs(nth 0 x)))vertex-list))
			   (cons "EndPoint" (nth(1-(abs(nth 3 x)))vertex-list))
			   (cons "Linetype" "Continuous")
			   )))ssnab))
    (setq ssnab(ssadd(vlax-vla-object->ename
		       (draw-line-vla-by-list-property
			 (list
			   (cons "spece" model_spece)
			   (cons "StartPoint" (nth(1-(abs(nth 1 x)))vertex-list))
			   (cons "EndPoint" (nth(1-(abs(nth 2 x)))vertex-list))
			   (cons "Linetype" "Continuous")
			   )))ssnab))






(setq ssnab(ssadd (vlax-vla-object->ename (draw-line-vla-by-list-property(list
				     (cons "spece" model_spece)
				     (cons "StartPoint" (nth(1-(abs(nth 0 x)))vertex-list))
				     (cons "EndPoint" (nth(1-(abs(nth 3 x)))vertex-list))
				     (cons "Linetype" "Continuous")
				     )))ssnab))

    (draw-line-vla-by-list-property(list
				     (cons "spece" model_spece)
				     (cons "StartPoint" (nth(1-(abs(nth 0 x)))vertex-list))
				     (cons "EndPoint" (nth(1-(abs(nth 3 x)))vertex-list))
				     (cons "Linetype" "Continuous")
				     ))
    (draw-line-vla-by-list-property(list
				     (cons "spece" model_spece)
				     (cons "StartPoint" (nth(1-(abs(nth 1 x)))vertex-list))
				     (cons "EndPoint" (nth(1-(abs(nth 2 x)))vertex-list))
				     (cons "Linetype" "Continuous")
				     ))
    (setq result--list(append result--list (list (vl-prin1-to-string list-entget))))
  (file_write_list_to_file "d:\\proba.txt" result--list)
(file_write_list_to_file "d:\\proba.txt" )
    (if (= (cdr (assoc 0 list-entget)) "POLYLINE")
      (progn
	(setq obj-v(entnext obj-v))
	(while (not(eq obj-v nil))
	  (setq list-entget(entget obj-v))
	  (if (= (cdr (assoc 0 list-entget)) "VERTEX")
	    (setq result-list(append result-list (list (cdr (assoc 10 list-entget)))))
	    
	    )
	  (setq obj-v(entnext obj-v))
	  )
	)
      )
    (if (not(eq obj-v nil))
    (setq obj-v(entnext obj-v))
      )



(defun C:proba (/
		CURENT-PLINE FACE-LIST FLAG-ALL FLAG-PL LINE-LIST LIST-ENTGET LIST-PLINE NUMBER OBJ OBJ-V VERTEX-LIST
		)
  (load_global_variable)
  (vla-StartUndoMark active_document)
  (setq obj(car(entsel)))
  (terpri)
  (setq obj-v(entnext obj))
  (setq list-entget(entget obj-v))
  (while (not(= (cdr (assoc 0 list-entget)) "SEQEND"))
    (if (= (cdr (nth 10 list-entget)) "AcDbVertex")
      (setq vertex-list(append vertex-list (list (cdr (assoc 10 list-entget)))))
      (setq face-list(append face-list (list (list (cdr (assoc 71 list-entget))
						   (cdr (assoc 72 list-entget))
						   (cdr (assoc 73 list-entget))
						   (cdr (assoc 74 list-entget))))))
      )
    (setq obj-v(entnext obj-v))
    (setq list-entget(entget obj-v))
    )
  (foreach x face-list
    (setq line-list(append line-list (list(list (nth(1-(abs(nth 0 x)))vertex-list)(nth(1-(abs(nth 3 x)))vertex-list))
					  (list (nth(1-(abs(nth 1 x)))vertex-list)(nth(1-(abs(nth 2 x)))vertex-list))
					  )))
    )
  (princ line-list)
  (while (not flag-all)
    (setq number 0)
    (setq flag-pl nil)
    (setq curent-pline (car line-list))
    (setq line-list(cdr line-list))
    (if (eq line-list nil)
	(setq flag-pl T)
	)
    (while (not flag-pl)
      (cond
	((and (= (nth 0 (nth 0 curent-pline)) (nth 0 (nth 0 (nth number line-list))))
	      (= (nth 1 (nth 0 curent-pline)) (nth 1 (nth 0 (nth number line-list)))))
	 (setq curent-pline(cons (nth 1 (nth number line-list)) curent-pline))
	 (setq line-list(edit-list-del-item-by-number number line-list))
	 (setq number 0)
	 )
	((equal (nth 0 curent-pline) (nth 1 (nth number line-list))0.001)
	 (setq curent-pline(cons (nth 0 (nth number line-list)) curent-pline))
	 (setq line-list(edit-list-del-item-by-number number line-list))
	 (setq number 0)
	 )
	((equal (nth (1-(length curent-pline)) curent-pline) (nth 0 (nth number line-list))0.001)
	 (setq curent-pline(append curent-pline(list(nth 1 (nth number line-list)))))
	 (setq line-list(edit-list-del-item-by-number number line-list))
	 (setq number 0)
	 )
	((equal (nth (1-(length curent-pline)) curent-pline) (nth 1 (nth number line-list))0.001)
	 (setq curent-pline(append curent-pline(list(nth 0 (nth number line-list)))))
	 (setq line-list(edit-list-del-item-by-number number line-list))
	 (setq number 0)
	 )
	)
      (if(or(eq line-list nil)(= number(1-(length line-list)))(> number(1-(length line-list))))
	 (setq flag-pl T)
	 (setq number (1+ number))
	 )
      (bump)
      )
    (setq list-pline(append list-pline(list curent-pline)))
    (if (eq line-list nil)
      (setq flag-all T)
      )
    )
  (foreach x list-pline
  (draw_pline_entmake x nil nil 0 0)
    )
  (vla-EndUndoMark active_document)
  (princ)
  )


(defun C:proba1 ( / LIST-ENTGET LIST-OBJ NABOR POINT n x)
  (terpri)
  (setq nabor(ssget "_X"'((0 . "TEXT"))))
  (setq n 0)
  (setq sum (sslength nabor))
  (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
  (while (not(eq(sslength nabor)0))
    (setq list-entget (entget (ssname nabor 0)))
    (setq point (cdr (assoc 10 list-entget)))
    (entmod (subst (cons 10 (list (nth 0 point)(nth 1 point)0))(assoc 10 list-entget) list-entget))
    (entupd (ssname nabor 0))
    (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
    (setq n (1+ n))
    (setq nabor(ssdel (ssname nabor 0)nabor))
    )
  (princ)
  )
(defun C:proba2 ( / LIST-ENTGET LIST-OBJ NABOR POINT n x)
  (terpri)
  (setq nabor(ssget "_X"'((0 . "LWPOLYLINE"))))
  (setq n 0)
  (setq sum (sslength nabor))
  (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
  (while (not(eq(sslength nabor)0))
    (setq list-entget (entget (ssname nabor 0)))
    (entmod (subst (cons 38 0.0)(assoc 38 list-entget) list-entget))
    (entupd (ssname nabor 0))
    (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
    (setq n (1+ n))
    (setq nabor(ssdel (ssname nabor 0)nabor))
    )
  (princ)
  )

(defun C:proba3 ( / N NABOR OBJ SUM x)
  (load_global_variable)
  (vla-StartUndoMark active_document)
  (foreach x (list "LWPOLYLINE" "TEXT")
    (terpri)
    (setq nabor(ssget "_X"(list (cons 0 x))))
    (setq n 0)
    (setq sum (sslength nabor))
    (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
    (while (not(eq(sslength nabor)0))
      (setq obj (vlax-ename->vla-object (ssname nabor 0)))
      (vla-move obj (vlax-3D-point (list 0.0 0.0))(vlax-3D-point (list -0.165 0.056)))
      (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
      (setq n (1+ n))
      (setq nabor(ssdel (ssname nabor 0)nabor))
      )
    )
  (vla-EndUndoMark active_document)
  (princ)
  )
(defun C:move_all ( / N NABOR OBJ SUM x)
  (load_global_variable)
  (vla-StartUndoMark active_document)
    (terpri)
    (setq nabor(ssget "_X"))
    (setq n 0)
    (setq sum (sslength nabor)) 
    (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
    (while (not(eq(sslength nabor)0))
      (setq obj (vlax-ename->vla-object (ssname nabor 0)))
      
      (vla-move obj (vlax-3D-point (list -140439.071 67281.503))(vlax-3D-point (list 7639.898 9640.317)))
      (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
      (setq n (1+ n))
      (setq nabor(ssdel (ssname nabor 0)nabor))
      )
  (vla-EndUndoMark active_document)
  (princ)
  )

(defun C:proba( / NAME X Y)
  (load_global_variable)
  (setq name (cdr(assoc 1(entget(car(entsel "\nВыбери текст Наименованием: "))))))
  (setq x (atof(cdr(assoc 1(entget(car(entsel "\nВыбери текст с X: ")))))))
  (setq y (atof(cdr(assoc 1(entget(car(entsel "\nВыбери текст с Y: ")))))))
  (draw_mleader_vla model_spece name 2.5 (list y x) (mapcar '+ (list y x) '(10 10)))
  )

(defun add-list-patch (new_directoryes dir / list_directory); возвращает список с путем добавленым к директориям
  (repeat (length new_directoryes)
    (setq list_directory(append list_directory(list(cons (car new_directoryes)(strcat dir "\\"(car new_directoryes))))))
    (setq new_directoryes (cdr new_directoryes))
    )
  list_directory
  )
(defun C:set-colr-by-layer ( / N NABOR OBJ SUM)
  (load_global_variable)
  (vla-StartUndoMark active_document)
  (terpri)
  (setq nabor(ssget "_X"))
  (setq n 0)
  (setq sum (sslength nabor))
  (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
  (while (not(eq(sslength nabor)0))
    (setq obj(vlax-ename->vla-object (ssname nabor 0)))
    (if (vlax-property-available-p obj "Color" T)
      (vla-put-color obj 256)
      )
    (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
    (setq n (1+ n))
    (setq nabor(ssdel (ssname nabor 0)nabor))
    )
  (vla-EndUndoMark active_document)
  (princ)
  )
(defun C:proba-SOLID ( / nabor ttt n) ;заменяет объект SOLID на за замкнутую полилинию
  (begin_activex)
  (setq nabor(ssget "_X"'((0 . "SOLID"))))
  (setq n 0)
  (repeat (sslength nabor)
    (setq ttt (entget(ssname nabor n)))
    (if (cdr (assoc 62 ttt))
      (entmake (list '(0 . "LWPOLYLINE")
		     '(100 . "AcDbEntity")
		     '(6 . "Continuous")
		     '(100 . "AcDbPolyline")
		     '(90 . 4) '(70 . 1)
		     (cons 8 (cdr (assoc 8 ttt)))
		     (cons 62 (cdr (assoc 62 ttt)))
		     (cons 10 (cdr (assoc 11 ttt))) (cons 10 (cdr (assoc 10 ttt)))
		     (cons 10 (cdr (assoc 12 ttt))) (cons 10 (cdr (assoc 13 ttt)))))
      (entmake (list '(0 . "LWPOLYLINE")
		     '(100 . "AcDbEntity")
		     '(6 . "Continuous")
		     '(100 . "AcDbPolyline")
		     '(90 . 4) '(70 . 1)
		     (cons 8 (cdr (assoc 8 ttt)))
		     (cons 10 (cdr (assoc 11 ttt))) (cons 10 (cdr (assoc 10 ttt)))
		     (cons 10 (cdr (assoc 12 ttt))) (cons 10 (cdr (assoc 13 ttt)))))
      )
    (entdel (ssname nabor n))
    (setq n (1+ n))
    )
  )


(defun C:move_all ( / N NABOR OBJ SUM x)
  (load_global_variable)
  (vla-StartUndoMark active_document)
    (terpri)
    (setq nabor(ssget "_X"))
    (setq n 0)
    (setq sum (sslength nabor)) 
    (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
    (while (not(eq(sslength nabor)0))
      (setq obj (vlax-ename->vla-object (ssname nabor 0)))
      
      (vla-move obj (vlax-3D-point (list -163.596 33.708))(vlax-3D-point (list 7639.898 9640.317)))
      (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
      (setq n (1+ n))
      (setq nabor(ssdel (ssname nabor 0)nabor))
      )
  (vla-EndUndoMark active_document)
  (princ)
  )


(defun C:set-layer ( / nabor ttt n) ;заменяет объект SOLID на за замкнутую полилинию
  (load_global_variable)
  (vla-StartUndoMark active_document)
  (terpri)
  (setq nabor(ssget "_X"'((8 . "0"))))
  (setq n 0)
  (setq sum (sslength nabor))
  (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
  (while (not(eq(sslength nabor)0))
    (setq obj (vlax-ename->vla-object (ssname nabor 0)))
    (if (vlax-property-available-p obj "layer" T)
      (vla-put-layer obj "temp")
      )
    (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
    (setq n (1+ n))
    (setq nabor(ssdel (ssname nabor 0)nabor))
    )
  (vla-EndUndoMark active_document)
  (princ)
  )
(defun C:set-text-style ( / N NABOR OBJ SUM)
  (load_global_variable)
  (vla-StartUndoMark active_document)
  (terpri)
  (setq nabor(ssget "_X"'((0 . "TEXT"))))
  (setq n 0)
  (setq sum (sslength nabor))
  (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
  (while (not(eq(sslength nabor)0))
    (setq obj (vlax-ename->vla-object (ssname nabor 0)))
    (if (vlax-property-available-p obj "StyleName" T)
      (vla-put-StyleName obj "STANDARD")
      )
    (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
    (setq n (1+ n))
    (setq nabor(ssdel (ssname nabor 0)nabor))
    )
  (vla-EndUndoMark active_document)
  (princ)
  )

(defun C:sort-layer ( / N NABOR OBJ SUM)
  (load_global_variable)
  (vla-StartUndoMark active_document)
  (terpri)
  (setq nabor(ssget "_X"'((0 . "TEXT")(8 . "_тексты"))))
  (setq n 0)
  (setq sum (sslength nabor))
  (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
  (while (not(eq(sslength nabor)0))
    (setq obj (vlax-ename->vla-object (ssname nabor 0)))
    (if (<(vla-get-Height obj)7.0)
      (vla-put-layer obj "_пояснительные подписи")
      )
    (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
    (setq n (1+ n))
    (setq nabor(ssdel (ssname nabor 0)nabor))
    )
  (vla-EndUndoMark active_document)
  (princ)
  )

(and(wcmatch (vla-get-TextString obj) "#*")(<(vla-get-Height obj)6.0))

(defun C:del-smal-text ( / N NABOR OBJ SUM)
  (load_global_variable)
  (vla-StartUndoMark active_document)
  (terpri)
  (setq nabor(ssget "_X"'((0 . "TEXT")(8 . "железная дорога"))))
  (setq n 0)
  (setq sum (sslength nabor))
  (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
  (while (not(eq(sslength nabor)0))
    (setq obj (vlax-ename->vla-object (ssname nabor 0)))
    (if (<(vla-get-Height obj)7.0)
      (vla-erase obj)
      )
    (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
    (setq n (1+ n))
    (setq nabor(ssdel (ssname nabor 0)nabor))
    )
  (vla-EndUndoMark active_document)
  (princ)
  )


(defun C:proba (/
		CURENT-PLINE FACE-LIST FLAG-ALL FLAG-PL LINE-LIST LIST-ENTGET LIST-PLINE NUMBER OBJ OBJ-V VERTEX-LIST
		)
  (load_global_variable)
  (vla-StartUndoMark active_document)
  (setq obj(car(entsel)))
  (terpri)
  (setq obj-v(entnext obj))
  (setq list-entget(entget obj-v))
  (while (not(= (cdr (assoc 0 list-entget)) "SEQEND"))
    (if (= (cdr (nth 10 list-entget)) "AcDbVertex")
      (setq vertex-list(append vertex-list (list (cdr (assoc 10 list-entget)))))
      (setq face-list(append face-list (list (list (cdr (assoc 71 list-entget))
						   (cdr (assoc 72 list-entget))
						   (cdr (assoc 73 list-entget))
						   (cdr (assoc 74 list-entget))))))
      )
    (setq obj-v(entnext obj-v))
    (setq list-entget(entget obj-v))
    )
  (foreach x face-list
    (setq line-list(append line-list (list(list (nth(1-(abs(nth 0 x)))vertex-list)(nth(1-(abs(nth 3 x)))vertex-list))
					  (list (nth(1-(abs(nth 1 x)))vertex-list)(nth(1-(abs(nth 2 x)))vertex-list))
					  )))
    )
  (princ line-list)
  (while (not flag-all)
    (setq number 0)
    (setq flag-pl nil)
    (setq curent-pline (car line-list))
    (setq line-list(cdr line-list))
    (if (eq line-list nil)
	(setq flag-pl T)
	)
    (while (not flag-pl)
      (cond
	((and (= (nth 0 (nth 0 curent-pline)) (nth 0 (nth 0 (nth number line-list))))
	      (= (nth 1 (nth 0 curent-pline)) (nth 1 (nth 0 (nth number line-list)))))
	 (setq curent-pline(cons (nth 1 (nth number line-list)) curent-pline))
	 (setq line-list(edit-list-del-item-by-number number line-list))
	 (setq number 0)
	 )
	((equal (nth 0 curent-pline) (nth 1 (nth number line-list))0.001)
	 (setq curent-pline(cons (nth 0 (nth number line-list)) curent-pline))
	 (setq line-list(edit-list-del-item-by-number number line-list))
	 (setq number 0)
	 )
	((equal (nth (1-(length curent-pline)) curent-pline) (nth 0 (nth number line-list))0.001)
	 (setq curent-pline(append curent-pline(list(nth 1 (nth number line-list)))))
	 (setq line-list(edit-list-del-item-by-number number line-list))
	 (setq number 0)
	 )
	((equal (nth (1-(length curent-pline)) curent-pline) (nth 1 (nth number line-list))0.001)
	 (setq curent-pline(append curent-pline(list(nth 0 (nth number line-list)))))
	 (setq line-list(edit-list-del-item-by-number number line-list))
	 (setq number 0)
	 )
	)
      (if(or(eq line-list nil)(= number(1-(length line-list)))(> number(1-(length line-list))))
	 (setq flag-pl T)
	 (setq number (1+ number))
	 )
      (bump)
      )
    (setq list-pline(append list-pline(list curent-pline)))
    (if (eq line-list nil)
      (setq flag-all T)
      )
    )
  (foreach x list-pline
  (draw_pline_entmake x nil nil 0 0)
    )
  (vla-EndUndoMark active_document)
  (princ)
  )

(defun C:proba ( / LIST-ENTGET LIST-OBJ NABOR POINT n x)
  (terpri)
  (setq nabor(ssget "_X"'((0 . "AECC_COGO_POINT"))))
  (setq n 0)
  (setq sum (sslength nabor))
  (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
  (while (not(eq(sslength nabor)0))
    (setq vla-object (vlax-ename->vla-object (ssname nabor 0)))
    (if (vlax-property-available-p vla-object "RawDescription")
      (if (wcmatch(vlax-get-property  vla-object "RawDescription")"*(####г.)*")
	(vla-put-layer vla-object "Пункты новые")
	)
      )
    (entupd (ssname nabor 0))
    (princ (strcat"\r  "(itoa n)"/"(itoa sum)))
    (setq n (1+ n))
    (setq nabor(ssdel (ssname nabor 0)nabor))
    )
  (princ)
  )
(defun profile-draw-eledation-by-list-limits (2Dpline list_granic t1! t1_ins_z t2_ins_z ht mg mv usl_z styl /
					      os_linii_podpisey spisokv p1 p2 g1 g2 n Dgranicy t1 htz z granica obj-list text-obj)
  (setq os_linii_podpisey (+(/(- t2_ins_z t1_ins_z )2.0) t1_ins_z ))
  (setq spisokv (extract_coord_lwpolyline (entget 2Dpline)))
  (setq p1 (car spisokv))
  (setq spisokv (cdr spisokv))
  (setq granica (car list_granic))
  (setq g1 (car granica))
  (setq g2 (cadr granica))
  (if (not(and(>(nth 0 p1)g1)(<(nth 0 p1)g2))); обработка первой точки/границы в пофиле
    (progn
      (setq z (+(*(- (nth 1 p1) (nth 1 t1!))(/ (* mv (/ 1000.0 mg)) 1000.0))usl_z))
      (setq text-obj(draw_text_vla model_spece (rtos z 2 2) (list(nth 0 p1)(+ t1_ins_z (/(- t2_ins_z t1_ins_z )2))) ht 13 (/ pi 2) nil T T))
      (vla-put-StyleName text-obj styl)
      (setq obj-list(append obj-list (list text-obj)))
      (setq obj-list(append obj-list(list(draw-line-vla model_spece (list(nth 0 p1)t1_ins_z) (list(nth 0 p1)t2_ins_z)))))
      )
    (progn
      (setq n (analisis_kol-vo_elevation_in_limit g1 g2 2Dpline))
      (setq Dgranicy (* n(/ (* 2 (/ 4.0 3.0)) (/ 1000.0 mg))))
      (setq t1 (mapcar '+ (list g1 os_linii_podpisey) (list(/(/ (* 2 (/ 4.0 3.0)) (/ 1000.0 mg))2) 0)))
      (repeat (1- n)
	(setq z (+(*(- (nth 1 p1) (nth 1 t1!))(/ (* mv (/ 1000.0 mg)) 1000.0))usl_z))
	(setq text-obj(draw_text_vla model_spece (rtos z 2 2) t1 (/ 2 (/ 1000.0 mg)) 4 (/ pi 2) nil T T))
	(vla-put-StyleName text-obj styl)
	(setq obj-list(append obj-list (list text-obj)))
	(setq text-obj
	       (vla-addLightWeightPolyline model_spece
		 (convert-list-points-to-lwpolyline-safearray
		   (list(list (nth 0 p1) t2_ins_z )
			(list (nth 0 p1) (- t2_ins_z (/ 1.0 (/ 1000.0 mg))))
			(list (nth 0 t1) (- t2_ins_z (/ 2.4 (/ 1000.0 mg))))
			(list (nth 0 t1) (- t2_ins_z (/ 3.4 (/ 1000.0 mg))))
			))))
	(vla-put-Linetype text-obj "Continuous")
	(setq obj-list(append obj-list (list text-obj)))
	(setq text-obj
	       (vla-addLightWeightPolyline model_spece
		 (convert-list-points-to-lwpolyline-safearray
		   (list(list (nth 0 t1) (+ t1_ins_z (/ 3.4 (/ 1000.0 mg))))
			(list (nth 0 t1) (+ t1_ins_z (/ 2.4 (/ 1000.0 mg))))
			(list (nth 0 p1) (+ t1_ins_z (/ 1.0 (/ 1000.0 mg))))
			(list (nth 0 p1) t1_ins_z )
			))))
	(vla-put-Linetype text-obj "Continuous")
	(setq obj-list(append obj-list (list text-obj)))

	(setq t1 (mapcar '+ t1 (list(/ (* 2 (/ 4.0 3.0)) (/ 1000.0 mg)) 0)))
	(setq p1 (car spisokv))
	(setq spisokv (cdr spisokv))
	)
      (setq z (+(*(- (nth 1 p1) (nth 1 t1!))(/ (* mv (/ 1000.0 mg)) 1000.0))usl_z))
      (setq text-obj(draw_text_vla model_spece (rtos z 2 2) t1 (/ 2 (/ 1000.0 mg)) 4 (/ pi 2) nil T T))
      (vla-put-StyleName text-obj styl)
      (setq obj-list(append obj-list (list text-obj)))
      (setq text-obj
	     (vla-addLightWeightPolyline model_spece
	       (convert-list-points-to-lwpolyline-safearray
		 (list(list (nth 0 p1) t2_ins_z )
		      (list (nth 0 p1) (- t2_ins_z (/ 1.0 (/ 1000.0 mg))))
		      (list (nth 0 t1) (- t2_ins_z (/ 2.4 (/ 1000.0 mg))))
		      (list (nth 0 t1) (- t2_ins_z (/ 3.4 (/ 1000.0 mg))))
		      ))))
      (vla-put-Linetype text-obj "Continuous")
      (setq obj-list(append obj-list (list text-obj)))
      (setq text-obj
	     (vla-addLightWeightPolyline model_spece
	       (convert-list-points-to-lwpolyline-safearray
		 (list(list (nth 0 t1) (+ t1_ins_z (/ 3.4 (/ 1000.0 mg))))
		      (list (nth 0 t1) (+ t1_ins_z (/ 2.4 (/ 1000.0 mg))))
		      (list (nth 0 p1) (+ t1_ins_z (/ 1.0 (/ 1000.0 mg))))
		      (list (nth 0 p1) t1_ins_z )
		      ))))
      (vla-put-Linetype text-obj "Continuous")
      (setq obj-list(append obj-list (list text-obj)))
      (setq p1 (list g2 os_linii_podpisey))
      (setq list_granic (cdr list_granic))
      )
    )
  (while (/=(length spisokv)0); обработка остальных точек
    (setq p2 (car spisokv))
    (setq spisokv (cdr spisokv))
    (setq granica (car list_granic))
    (setq g1 (car granica))
    (setq g2 (cadr granica))
    (if
      (not(and(>(nth 0 p2)g1)(<(nth 0 p2)g2)))
      (progn
	(cond
	  ((> (*(-(nth 0 p2)(nth 0 p1))0.6) ht)
	   (setq htz ht))
	  ((< (*(-(nth 0 p2)(nth 0 p1))0.6) (/ 1.6 (/ 1000.0 mg)))
	   (setq htz (/ 1.6 (/ 1000.0 mg))))
	  (T
	   (setq htz (*(-(nth 0 p2)(nth 0 p1))0.6)))
	  )
	(setq z (+(*(- (nth 1 p2) (nth 1 t1!))(/ (* mv (/ 1000.0 mg)) 1000.0))usl_z))
	
	(setq text-obj(draw_text_vla model_spece (rtos z 2 2) (list(nth 0 p2)os_linii_podpisey) htz 13 (/ pi 2) nil T T))
	(vla-put-StyleName text-obj styl)
	(setq obj-list(append obj-list (list text-obj)))
	(setq obj-list(append obj-list(list(draw-line-vla model_spece (list(nth 0 p2)t1_ins_z) (list(nth 0 p2)t2_ins_z)))))
	(setq p1 p2)
	
	)
      (progn
	(setq n (analisis_kol-vo_elevation_in_limit g1 g2 2Dpline))
	(setq Dgranicy (* n(/ (* 2 (/ 4.0 3.0)) (/ 1000.0 mg))))
	(setq t1 (mapcar '+ (list g1 os_linii_podpisey) (list(/(/ (* 2 (/ 4.0 3.0)) (/ 1000.0 mg))2) 0)))
	(repeat (1- n)
	  (setq z (+(*(- (nth 1 p2) (nth 1 t1!))(/ (* mv (/ 1000.0 mg)) 1000.0))usl_z))
	  (setq text-obj(draw_text_vla model_spece (rtos z 2 2) t1 (/ 2 (/ 1000.0 mg)) 4 (/ pi 2) nil T T))
	  (vla-put-StyleName text-obj styl)
	  (setq obj-list(append obj-list (list text-obj)))
	  (setq text-obj
		 (vla-addLightWeightPolyline model_spece
		   (convert-list-points-to-lwpolyline-safearray
		     (list(list (nth 0 p2) t2_ins_z )
			  (list (nth 0 p2) (- t2_ins_z (/ 1.0 (/ 1000.0 mg))))
			  (list (nth 0 t1) (- t2_ins_z (/ 2.4 (/ 1000.0 mg))))
			  (list (nth 0 t1) (- t2_ins_z (/ 3.4 (/ 1000.0 mg))))
			  ))))
	  (vla-put-Linetype text-obj "Continuous")
	  (setq obj-list(append obj-list (list text-obj)))
	  (setq text-obj
		 (vla-addLightWeightPolyline model_spece
		   (convert-list-points-to-lwpolyline-safearray
		     (list(list (nth 0 t1) (+ t1_ins_z (/ 3.4 (/ 1000.0 mg))))
			  (list (nth 0 t1) (+ t1_ins_z (/ 2.4 (/ 1000.0 mg))))
			  (list (nth 0 p2) (+ t1_ins_z (/ 1.0 (/ 1000.0 mg))))
			  (list (nth 0 p2) t1_ins_z )
			  ))))
	  (vla-put-Linetype text-obj "Continuous")
	  (setq obj-list(append obj-list (list text-obj)))
	  (setq p2 (car spisokv))
	  (setq spisokv (cdr spisokv))
	  (setq t1 (mapcar '+ t1 (list(/ (* 2 (/ 4.0 3.0)) (/ 1000.0 mg)) 0)))
	  )
	(setq z (+(*(- (nth 1 p2) (nth 1 t1!))(/ (* mv (/ 1000.0 mg)) 1000.0))usl_z))
	(setq text-obj(draw_text_vla model_spece (rtos z 2 2) t1 (/ 2 (/ 1000.0 mg)) 4 (/ pi 2) nil T T))
	(vla-put-StyleName text-obj styl)
	(setq obj-list(append obj-list (list text-obj)))
	(setq text-obj
	       (vla-addLightWeightPolyline model_spece
		 (convert-list-points-to-lwpolyline-safearray
		   (list(list (nth 0 p2) t2_ins_z )
			(list (nth 0 p2) (- t2_ins_z (/ 1.0 (/ 1000.0 mg))))
			(list (nth 0 t1) (- t2_ins_z (/ 2.4 (/ 1000.0 mg))))
			(list (nth 0 t1) (- t2_ins_z (/ 3.4 (/ 1000.0 mg))))
			))))
	(vla-put-Linetype text-obj "Continuous")
	(setq obj-list(append obj-list (list text-obj)))
	(setq text-obj
	       (vla-addLightWeightPolyline model_spece
		 (convert-list-points-to-lwpolyline-safearray
		   (list(list (nth 0 t1) (+ t1_ins_z (/ 3.4 (/ 1000.0 mg))))
			(list (nth 0 t1) (+ t1_ins_z (/ 2.4 (/ 1000.0 mg))))
			(list (nth 0 p2) (+ t1_ins_z (/ 1.0 (/ 1000.0 mg))))
			(list (nth 0 p2) t1_ins_z )
			))))
	(vla-put-Linetype text-obj "Continuous")
	(setq obj-list(append obj-list (list text-obj)))
	(setq p1 (list g2 os_linii_podpisey))
	(setq list_granic (cdr list_granic))
	)
      )
    )
  obj-list
  )

(vla-put-ActiveLinetype active_document (vla-item (vla-get-linetypes active_document) (cdr(assoc 6 (entget obj)))))

(defun C:proba ( / INPUT-LIST N OBJ RESULT VLA-OBJ)
  (load_global_variable)
  (setq obj(car(entsel "\nВыберите поверхность: ")))
  (if obj
    (progn
      (setq vla-obj (vlax-ename->vla-object obj))
      (setq input-list(vlax-get vla-obj 'Points))
      (setq n 0)
      (repeat (/(length input-list)3)
	(setq result (append result (list(list (nth n input-list)(nth (1+ n) input-list)(nth (+ n 2) input-list)))))
	(setq n (+ n 3))
	)
      (foreach x result
	(command "_AeccAddSurfaceSpotElevLabel" obj x (command))
	(vla-put-layer(vlax-ename->vla-object (entlast))(vla-get-Name(vla-get-ActiveLayer active_document)))
	)
      )
    )
  (princ)
  )

(defun C:proba ()
  (princ(analisis-surfese-and-2d-poly (car(entsel))(car(entsel))))
  (princ)
  )

(vl-registry-read "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders" "AppData")
(vl-registry-read "HKEY_CURRENT_USER\\Volatile Environment" "AppData")


(defun load_global_variable (/); начало работы с расширением lisp'а
  (vl-load-com)
  (setq acad_application (vlax-get-acad-object))
  (if
    (vl-catch-all-error-p
      (setq C3D_Application (vl-catch-all-apply
			      'vla-getInterfaceObject
			      (list
				acad_application
				(cond
				  ((eq (getvar "ACADVER") "17.1s (LMS Tech)")"AeccXUiLand.AeccApplication.5.0"); Acad 2008
				  ((eq (getvar "ACADVER") "17.2s (LMS Tech)")"AeccXUiLand.AeccApplication.6.0"); Acad 2009
				  ((eq (getvar "ACADVER") "18.0s (LMS Tech)")"AeccXUiLand.AeccApplication.7.0"); Acad 2010
				  ))))
      )
    (progn
      (princ (vl-catch-all-error-message C3D_Application))
      (setq C3D_Application nil)
      (princ "\nAutocad")
      )
    (progn
      (setq C3D_active_document (vla-get-ActiveDocument C3D_Application))
      (setq C3D_points (vlax-get C3D_active_document 'points))
      (princ "\nCivil")
      )
    )
  (setq C3D_Application (vla-getInterfaceObject acad_application
			  (cond
			    ((eq (getvar "ACADVER") "17.1s (LMS Tech)")"AeccXUiLand.AeccApplication.5.0"); Acad 2008
			    ((eq (getvar "ACADVER") "17.2s (LMS Tech)")"AeccXUiLand.AeccApplication.6.0"); Acad 2009
			    ((eq (getvar "ACADVER") "18.0s (LMS Tech)")"AeccXUiLand.AeccApplication.7.0"); Acad 2010
			    )
			  )
	)
  (setq active_document (vla-get-ActiveDocument acad_application))
  (setq layouts_document (vla-get-Layouts active_document))
  (setq model_spece (vla-get-ModelSpace active_document))
  (setq paper_spece (vla-get-PaperSpace active_document))
  (setq blocks_spece (vla-get-Blocks active_document))
  (princ)
  )

(defun C:draw_graf_be_tabl ( / columns n nn rows spisok_n spisok_nn table_obj list-point)
  ; построить график по табличным данным
  (load_global_variable)
  (setq table_obj (car (entsel "\nВыбери таблицу: ")))
  (cond
    ((eq table_obj nil)
     (alert "Ничего не выбрано!"))
    ((not(eq (cdr(assoc 0 (entget table_obj))) "ACAD_TABLE"))
     (alert "Выбран не допустимый объект!"))
    ((eq (cdr(assoc 0 (entget table_obj))) "ACAD_TABLE")
     (setq table_obj (vlax-ename->vla-object table_obj))
     (setq spisok_n (list(vla-GetText table_obj 0 0 )))
     (setq rows(vla-get-rows table_obj))
     (setq columns(vla-get-columns table_obj))
     (setq n 1)
     (repeat (1- rows)
       (setq nn 0)
       (repeat columns
	 (setq spisok_nn(append spisok_nn(list (vla-GetText table_obj n nn ))))
	 (setq nn (1+ nn))
	 )
       (setq spisok_n(append spisok_n(list spisok_nn)))
       (setq spisok_nn nil)
       (setq n (1+ n))
       )
     )
    )
  (setq n 2)
  (repeat (-(length spisok_n)2)
    (setq list-point (append list-point (list (nth n spisok_n))))
    (setq n (1+ n))
    )
  (draw-3DPoly-vla-by-list-property
    (list (cons "spece" model_spece)
	  (cons "Coordinates" list-point)
	  )
    )
  (princ)
  )


(vla-addArc model_spece (vlax-3D-point (getpoint)) 20 (/ pi 2) pi)
;;;vla-GetXData
;;;Как выяснилось, ни как.
;;;Все свойства BackgroundFill к мультитексту присобачиваются
;;;через расширенные данные, с именем приложения ACAD, и
;;;в разделе ACAD_MTEXT_BBRT.
;;;Вот только проблема в том, что к расширенным данным
;;;с этим именем, метод GetXData, а тем более SetXData,
;;;доступа не имеют, он для них как бы не существует.
;;;Может кто ещё что нарыл?
;;;
;;;Данная тема была начата в разделе "Программирование-Другое"
;;;http://www.caduser.ru/forum/index.php...&TID=34127
;;;Кое что всё же удалось выяснить, но возникли вопросы,
;;;может кто то знает как решить следующие проблемы:
;;;Начиная с AutoCAD 2004 информация о свойстве объекта
;;;MText "Background" хранится в самом объекте под кодами:
;;;-90- значение -1- фон указанного цвета;
;;;значение -3- фон цвета фона рисунка;
;;;-63- цвет фона Мультитекста;
;;;-45- значение offset factor.

(setq en (entlast))
(setq ed (entget en))
(setq ed (subst (cons 90 1) (assoc 90 ed) ed ))
(setq ed (subst (cons 45 2.3) (assoc 45 ed) ed ))
(setq ed (subst (cons 63 170) (assoc 63 ed) ed ))
(entmod ed)

(defun c:t2m (/        sset     count    num      en       el
              mcontent bbox     point1   point2   point3   point4
              mwidth   mheight  mstyle   njust    mrotate  nmtext
              ss
             )
  (vl-load-com)
  (setvar "cmdecho" 0)
  (if (setq sset (ssget "_:L" '((0 . "TEXT"))))
  (progn  
  (setq count 0
        ss    (ssadd)
  ) ;_ end of setq
  (while (ssname sset COUNT)
    (setq EN (ssname sset COUNT))
    (setq EL (entget EN))
    (if (= (cdr (assoc 0 EL)) "TEXT")
      (progn
        (setq mcontent (cons '1 (strcase (cdr (assoc 1 el)))))
        (setq EL (subst mcontent(assoc 1 EL) EL))
        (setq bbox (acet-geom-textbox EL 0.1))
        (setq point1 (car bbox))
        (setq point2 (cadr bbox))
        (setq point3 (cadr (cdr bbox)))
        (setq point4 (cadr (cdr (cdr bbox))))
        (setq mwidth (cons '41 (distance point1 point2)))
        (setq mheight (cons '40 (cdr (assoc 40 el))))
        (setq mstyle (cons '7 (cdr (assoc 7 el))))
        (setq nspace (cons '410 (cdr (assoc 410 EL))))
        (setq minsert (cons '10 (cdr (assoc 10 EL))))
        (cond
          ((and (= (cdr (assoc 72 el)) 0) (= (cdr (assoc 73 el)) 3))
           (setq NJUST (cons '71 1))
          )                                       ;JY
          ((and (= (cdr (assoc 72 el)) 1) (= (cdr (assoc 73 el)) 3))
           (setq NJUST (cons '71 2))
          )                                       ;JU
          ((and (= (cdr (assoc 72 el)) 2) (= (cdr (assoc 73 el)) 3))
           (setq NJUST (cons '71 3))
          )                                       ;JI
          ((and (= (cdr (assoc 72 el)) 0) (= (cdr (assoc 73 el)) 2))
           (setq NJUST (cons '71 7))
          )                                       ;JN
          ((and (= (cdr (assoc 72 el)) 1) (= (cdr (assoc 73 el)) 2))
           (setq NJUST (cons '71 7))
          )                                       ;JN
          ((and (= (cdr (assoc 72 el)) 2) (= (cdr (assoc 73 el)) 2))
           (setq NJUST (cons '71 6))
          )                                       ;JK
          ((and (= (cdr (assoc 72 el)) 0) (= (cdr (assoc 73 el)) 0))
           (setq NJUST (cons '71 7))
          )                                       ;JN
          ((and (= (cdr (assoc 72 el)) 4) (= (cdr (assoc 73 el)) 0))
           (setq NJUST (cons '71 7))
          )                                       ;JN
          ((and (= (cdr (assoc 72 el)) 0) (= (cdr (assoc 73 el)) 1))
           (setq NJUST (cons '71 7))
          )                                       ;JN
          ((and (= (cdr (assoc 72 el)) 1) (= (cdr (assoc 73 el)) 0))
           (setq NJUST (cons '71 7))
          )                                       ;JN
          ((and (= (cdr (assoc 72 el)) 1) (= (cdr (assoc 73 el)) 1))
           (setq NJUST (cons '71 8))
          )                                       ;JM
          ((and (= (cdr (assoc 72 el)) 2) (= (cdr (assoc 73 el)) 1))
           (setq NJUST (cons '71 9))
          )                                       ;J,
          ((and (= (cdr (assoc 72 el)) 2) (= (cdr (assoc 73 el)) 0))
           (setq NJUST (cons '71 7))
          )                                       ;JN
        ) ;_ end of cond
        (setq mrotate (cons '50 (cdr (assoc 50 el))))
        (setq nmtext (list '(0 . "MTEXT")   '(100 . "AcDbEntity")
                           '(67 . 0)        nspace
                           '(8 . "TEXT")    '(100 . "AcDbMText")
                           minsert          njust
                           mheight          mwidth
                           mstyle           mcontent
                           mrotate
                          ) ;_ end of list
        ) ;_ end of setq
        (vla-put-backgroundfill
          (vlax-ename->vla-object (entmakex nmtext))
          :vlax-true
        ) ;_ end of vla-put-BackgroundFill
        (ssadd (entlast) ss)
        (entdel en)
        (setq count (+ count 1))
      ) ;_ end of progn
      (setq count (+ count 1))
    ) ;_ end of if
  ) ;_ end of while
  (if (> (sslength ss) 0)
    (command "_draworder" ss "" "_F")
  ) ;_ end of if
  )
  (princ "\nNo objects selected.")
 )
  (setvar "cmdecho" 1)
  (princ)
) ;_ end of defun

(vl-registry-read "HKEY_LOCAL_MACHINE\\SOFTWARE\\Autodesk\\AutoCAD\\R17.1\\ACAD-6000:419" "ProductName")
(vl-registry-descendents "HKEY_LOCAL_MACHINE\\SOFTWARE\\Autodesk\\AutoCAD\\R17.1\\ACAD-6000:419")

(defun textlist-to-list (input-list / RESULT-LIST STRING-ITEM TEMP)
  (if input-list
    (progn
      (if (=(setq string-item (car input-list))"<list>")
	(while (not(= string-item"</list>"))
	  (if (= string-item "<list>")
	    (setq result-list (append result-list (textlist-to-list (cdr input-list))))
	    (setq result-list (append result-list (list string-item)))
	    )
	  (setq input-list(cdr input-list))
	  (setq string-item (car input-list))
	  )
	(while (not(= string-item"<list>"))
	  (if (= string-item "<list>")
	    (setq result-list (append result-list (textlist-to-list (cdr input-list))))
	    (setq result-list (append result-list (list string-item)))
	    )
	  (setq input-list(cdr input-list))
	  (setq string-item (car input-list))
	  )
	)
      result-list
      )
    )
  )
(defun list-to-list (input-list / RESULT-LIST STRING-ITEM TEMP)
  (if input-list
    (progn
      (setq string-item (car input-list))
      (while (not(= string-item"</list>"))
	(if (= string-item "<list>")
	  (progn
	    (setq temp(list-to-list (cdr input-list)))
	    (setq result-list(append result-list (list (car temp))))
	    (setq input-list(cdr temp))
	    )
	  (setq result-list(append result-list (list string-item)))
	  )
	(setq input-list(cdr input-list))
	(setq string-item (car input-list))
	)
      (list result-list (if (=(car input-list)"</list>")(cdr input-list)input-list))
      )
    )
  )

(defun list-to-item (input-list / )
  (setq string-item (car input-list))
  (while (not(= string-item"</item>"))
    (setq result-list(append result-list (list string-item)))
    (setq input-list(cdr input-list))
    (setq string-item (car input-list))
    )
  (list result-list (if (=(car input-list)"</list>")(cdr input-list)input-list))
  )

(defun textlist-to-list (input-list / )
  (if input-list
    (cons
      (if (not(or (= (car input-list)"<list>")(= (car input-list)"</list>")(= (car input-list)"<item>")(= (car input-list)"</item>")))
	(car input-list)
	(cond
	  ((=(car input-list)"<list>")
	   (list(textlist-to-list (cdr input-list)))
	   )
	  ((=(car input-list)"</list>")
	   (textlist-to-list (cdr input-list)))
	  ((=(car input-list)"<item>")
	   (textlist-to-list (cdr input-list)))
	  ((=(car input-list)"</item>")
	   (textlist-to-list (cdr input-list)))
	  )
	)
      (textlist-to-list (cdr input-list))
      )
    )
  )

(defun textlist-to-list (input-list / )
  (if input-list
    (append
      (cond
	((=(car input-list)"<list>")
	 (textlist-to-list (cdr input-list))
	 )
	((=(car input-list)"</list>")
	 (textlist-to-list (cdr input-list)))
	((=(car input-list)"<item>")
	 (textlist-to-list (cdr input-list)))
	((=(car input-list)"</item>")
	 (textlist-to-list (cdr input-list)))
	(T
	 (list(car input-list)))
	)
      (textlist-to-list (cdr input-list))
      )
    )
  )
(defun get-item (input-list /)
  (setq n 0)
  (while flag
    (setq str (nth n input-list))
    
    )
  )
;;;==============================================================
;;;acaddoc.lsp
(vl-load-com)
(setq *geo_tools_root_dir* (strcat (vl-registry-read "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion" "ProgramFilesDir") "\\geo_tools"))
(setq *geo_tools_user_dat_dir* (strcat (vl-registry-read "HKEY_CURRENT_USER\\Volatile Environment" "AppData") "\\geo_tools"))
(load (strcat *geo_tools_root_dir*"\\lisp\\lib\\file_find_be_extension.lsp"))
(load (strcat *geo_tools_root_dir*"\\lisp\\lib\\file_name_list_to_patch.lsp"))
(load (strcat *geo_tools_root_dir*"\\lisp\\geo-tools-load-all.lsp"))
(load (strcat *geo_tools_root_dir* "\\lisp\\all.fas"))
(geo-tools-load-all)
(load_global_variable)
(geo-tools-update)

(defun C:proba ()
  (princ(profile-control-mode))
  (if (yes_no_dialog "Пробный вопрос?")
    (princ "Ты ответил ДА!")
    (princ "Ты ответил НЕТ!")
    )
  (princ)
  )


(defun C:proba ( / object coor-trace)
  (setq object (car (entsel "\nВыбери текст: ")))
  (princ(vlax-ldata-list (vlax-ename->vla-object object)))
  (princ(analysis-number-item-in-list(assoc "ruler-list"(vlax-ldata-get (vlax-ename->vla-object object)"obj-list"))(vlax-ldata-get (vlax-ename->vla-object object)"obj-list")))
  (vla-Copy(vlax-ename->vla-object object))
  (setq coor-trace(vlax-ldata-get(vlax-ename->vla-object object)"coor-trace"))
  (princ(rtos(trace-get-length coor-trace)2 19))
  (princ(vlax-ldata-list "geo_tools_dictionary"))
  (vlax-ldata-delete "geo_tools_dictionary" "old-profile-item-list")(vlax-ldata-delete "geo_tools_dictionary" "old-item-list")
  (princ)
  )

(defun C:proba ( / d list-str num num_row point_list t0 t1 table_obj vinoska vla_vinoska)
  ;строит таблицу по мультивыноскам
  (initget 1)(setq t0 (getpoint "\nУкажи точку вставки таблицы координат: "))
  (setq table_obj (vla-AddTable model_spece (vlax-3D-point t0) 2 3 12 20))
  (vla-put-StyleName table_obj "Координаты")
  (vla-SetText table_obj 1 0 "Диаметр")
  (vla-SetCellAlignment table_obj 1 0 acMiddleCenter)
  (vla-SetCellTextHeight table_obj 1 0 2.8)
  (vla-SetText table_obj 1 1 "Длинна")
  (vla-SetCellAlignment table_obj 1 1 acMiddleCenter)
  (vla-SetCellTextHeight table_obj 1 1 2.8)
  (vla-SetText table_obj 1 2 "Тип прокладки")
  (vla-SetCellAlignment table_obj 1 2 acMiddleCenter)
  (vla-SetCellTextHeight table_obj 1 2 2.8)
  (vla-SetColumnWidth table_obj 2 25)
  (setq num_row 2)
  (setq num 1)
  (initget)(setq vinoska(car (entsel "\nВыбери выноску: ")))
  (if vinoska
    (progn
      (setq vla_vinoska(vlax-ename->vla-object vinoska))
      (setq point_list (vlax-safearray->list(vlax-variant-value(vla-GetLeaderLineVertices vla_vinoska 0))))
      (setq t1 (list (car point_list) (cadr point_list)))
      )(exit))
  
  (while (not (eq t1 nil))
    (if (eq (vla-get-ContentBlockName vla_vinoska) "")
	(setq d (vla-get-TextString vla_vinoska))
	(setq d (strcat"т."(cadr(extract_302 (entget vinoska))))))
    (setq list-str (convert-string-to-list-be-delimiter d "\\P"))
    (vla-InsertRows table_obj (1+ num_row) 8 1)
    (vla-SetText table_obj num_row 0 (nth 0 list-str))
    (vla-SetCellAlignment table_obj 0 0 acMiddleCenter)
    (vla-SetText table_obj num_row 1 (vl-string-left-trim"L="(nth 1 list-str)))
    (vla-SetCellAlignment table_obj num_row 1 acMiddleLeft)
    (vla-SetText table_obj num_row 2 (nth 2 list-str))
    (vla-SetCellAlignment table_obj num_row 2 acMiddleCenter)
    (vla-SetCellTextHeight table_obj num_row 2 2.8)
    (setq num_row (1+ num_row))
    (setq num (1+ num))
    (initget)(setq vinoska(car (entsel "\nВыбери выноску: ")))
    (if vinoska
      (progn
	(setq vla_vinoska(vlax-ename->vla-object vinoska))
	(setq point_list (vlax-safearray->list(vlax-variant-value(vla-GetLeaderLineVertices vla_vinoska 0))))
	(setq t1 (list (car point_list) (cadr point_list)))
	)(exit))
    )
  )

(defun C:proba ( / list-str list-text-obj nabor p1 p2)
  ; объединяет тексты в мультивыноску
  (setq nabor(ssget '((0 . "TEXT"))))
  (if nabor
    (progn
      (setq list-text-obj (convert_ss_to_list nabor))
      (foreach x list-text-obj
	(if list-str
	  (setq list-str (strcat list-str "\\P" (revers-elevation-in-string(cdr (assoc 1 (entget x))))))
	  (setq list-str (revers-elevation-in-string(cdr (assoc 1 (entget x)))))
	  )
	)
      (if(setq p1 (getpoint "\nУкажи точку мультивыноски: "))
	(if(setq p2 (getpoint p1 "\nУкажи вторую точку мультивыноски: "))
	  (progn
	    (vla-StartUndoMark active_document)
	    (draw-mleader-vla-by-list-property
	      (list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
		   (cons "LeaderLineCoor" (list p1 p2))
		   (cons "StyleName" "Подписи")
		   (cons "TextHeight" (/ 2.0 (getvar "CANNOSCALEVALUE")))
		   (cons "TextString" list-str)
		   ))
	    (vla-EndUndoMark active_document)
	    )
	  )
	)
      )
    )
  (princ)
  )

(defun revers-elevation-in-string (string / pos)
  (if
    (setq pos(vl-string-position (ascii " ") string 0 T))
    (strcat (substr string (+ pos 2))" "(substr string 1 pos))
    string)
  )
