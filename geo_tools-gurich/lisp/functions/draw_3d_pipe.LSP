(defun C:draw_3d_pipe ( / line-list list-duble ssnabor radius list-arc diametr solid Layer Color)
  ; строит твердотельную трубу на основе набора линейных объектов (линия, полилиния, 3Dполилиния) и значения диаметра
  (initget 6)
  (setq diametr (getreal "\nВведи диаметр<10.0>: "))
  (if (not diametr)
    (setq diametr 10.0))
  (setq radius (*(/ diametr 2.0)1.5))
  (setq ssnabor (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE"))))
  (vla-StartUndoMark active_document)
  (setq line-list (convert-nab-to-list-line-obj ssnabor))
  (foreach x line-list
    (setq list-duble nil)
    (foreach y line-list
      (if (not(eq x y))
	(if(check-line-by-common-points x y T)
	  (setq list-duble (append list-duble (list y)))
	  )
	)
      )
    (if (and list-duble (=(length list-duble)1))
      (setq list-arc (append list-arc (list (draw-otvod-and-edit-lines x (car list-duble) radius))))
      )
    (setq list-duble nil)
    (foreach y line-list
      (if (not(eq x y))
	(if(check-line-by-common-points x y nil)
	  (setq list-duble (append list-duble (list y)))
	  )
	)
      )
    (if (and list-duble (=(length list-duble)1))
      (setq list-arc (append list-arc (list (draw-otvod-and-edit-lines x (car list-duble) radius))))
      )
    )
  (setvar "CMDECHO" 0)
  (setvar "NOMUTT" 1)
  (foreach x (append list-arc line-list)
    (setq Layer (vla-get-Layer(vlax-ename->vla-object x)))
    (setq Color (vla-get-Color(vlax-ename->vla-object x)))
    (entmake (list '(0 . "CIRCLE") '(10 0.0 0.0 0.0) (cons 40 (/ diametr 2))))
    (command "_sweep" (entlast) "" x)
    (setq solid (vlax-ename->vla-object(entlast)))
    (vla-put-Layer solid Layer)
    (vla-put-Color solid Color)
    )
  (setvar "NOMUTT" 0)
  (setvar "CMDECHO" 1)
  (vla-EndUndoMark active_document)
  (princ)
  )

(defun check-line-by-common-points (obj1 obj2 flag-start / )
  ; проверка двух линий на общность одной точки
  ; obj1 obj2 - объекты линий типа ename
  ; flag-start - T или nil проверяется первая точка или вторая
  (setq flag-start
	 (if flag-start
	   10
	   11))
  (if (and obj1 obj2)
    (if(or (equal (cdr(assoc flag-start (entget obj1)))(cdr(assoc 10 (entget obj2)))0.001)
	   (equal (cdr(assoc flag-start (entget obj1)))(cdr(assoc 11 (entget obj2)))0.001)
	   )
      T
      nil
      )
    (progn
      (princ "\nОшибка! Объект не задан!")
      nil
      )
    )
  )

(defun get-other-point-line (point-line / )
  (if (= point-line 11)
    10 11)
  )

(defun draw-otvod-and-edit-lines (obj1 obj2 radius /
				  ab alfa betta common-point m-p-centr m-p1 m-p2 m-p3 m-p4 m-p5 normal p-centr p1 p2 p3 p4 p5)
  (cond
    ((equal (cdr(assoc 10 (entget obj1)))(cdr(assoc 10 (entget obj2)))0.001)
     (setq common-point (list 10 10))
     )
    ((equal (cdr(assoc 11 (entget obj1)))(cdr(assoc 11 (entget obj2)))0.001)
     (setq common-point (list 11 11))
     )
    ((equal (cdr(assoc 10 (entget obj1)))(cdr(assoc 11 (entget obj2)))0.001)
     (setq common-point (list 10 11))
     )
    ((equal (cdr(assoc 11 (entget obj1)))(cdr(assoc 10 (entget obj2)))0.001)
     (setq common-point (list 11 10))
     )
    (t
     (setq common-point nil))
    )
  (if common-point
    (progn
      (setq p1 (cdr(assoc (get-other-point-line (nth 0 common-point))(entget obj1)))
	    p2 (cdr(assoc (nth 0 common-point) (entget obj1)))
	    p3 (cdr(assoc (get-other-point-line (nth 1 common-point))(entget obj2))))
      (setq normal (calculation-normal-by-3points p1 p2 p3))
      (setq m-p1 (trans p1 0 normal)
	    m-p2 (trans p2 0 normal)
	    m-p3 (trans p3 0 normal))
      (setq alfa (/(calculation_angle_be_3points m-p1 m-p2 m-p3)2))
      (setq betta (- pi alfa (/ pi 2)))
      (setq ab (/ (* radius (sin betta))(sin alfa)))
      (setq m-p4 (polar m-p2 (angle m-p2 m-p1) ab))
      (setq m-p5 (polar m-p2 (angle m-p2 m-p3) ab))
      (setq m-p-centr (polar m-p4 (+(angle m-p4 m-p2)(/ pi 2)) radius))
      (setq p4 (trans m-p4 normal 0))
      (setq p5 (trans m-p5 normal 0))
      (setq p-centr (trans m-p-centr normal 0))
      (entmod (list (cons -1 obj1) (cons (nth 0 common-point) p4)))
      (entmod (list (cons -1 obj2) (cons (nth 1 common-point) p5)))
      (vlax-vla-object->ename
	(draw-arc-vla-by-list-property
	  (list
	    (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	    (cons "Center" p-centr)
	    (cons "StartAngle" (angle m-p-centr m-p4))
	    (cons "EndAngle" (angle m-p-centr m-p5))
	    (cons "Radius" radius)
	    (cons "Lineweight"(vla-get-Lineweight (vlax-ename->vla-object obj1)))
	    (cons "Layer"(vla-get-Layer (vlax-ename->vla-object obj1)))
	    (cons "Color"(vla-get-Color (vlax-ename->vla-object obj1)))
	    (cons "Linetype" (vla-get-Linetype (vlax-ename->vla-object obj1)))
	    (cons "Normal" normal)
	    ))
	)
      )
    (princ "\nОшибка! Нет общей точки у линий!")
    )
  )

(defun calculation-normal-by-3points (p1 p2 p3 / )
  ;Функция вычисления вектора нормали по трем точкам
  (list
    (+ (* (nth 1 p1) (- (nth 2 p2)(nth 2 p3)))
       (* (nth 1 p2) (- (nth 2 p3)(nth 2 p1)))
       (* (nth 1 p3) (- (nth 2 p1)(nth 2 p2))))
    (+ (* (nth 2 p1) (- (nth 0 p2)(nth 0 p3)))
       (* (nth 2 p2) (- (nth 0 p3)(nth 0 p1)))
       (* (nth 2 p3) (- (nth 0 p1)(nth 0 p2))))
    (+ (* (nth 0 p1) (- (nth 1 p2)(nth 1 p3)))
       (* (nth 0 p2) (- (nth 1 p3)(nth 1 p1)))
       (* (nth 0 p3) (- (nth 1 p1)(nth 1 p2))))
    )
  )

(defun convert-nab-to-list-line-obj (nabor / item line-list n nabor p1 p2 points-list vla-obj-item)
  ; конвертирует набор линий, полилиний, 3Dполилиний в список линий
  (setq n 0)
  (if nabor
    (progn
      (repeat (sslength nabor)
	(setq item (ssname nabor n))
	(cond
	  ((= (cdr(assoc 0 (entget item))) "LWPOLYLINE")
	   (setq vla-obj-item (vlax-ename->vla-object item))
	   (setq points-list (convert-safearray-to-list-points (vlax-variant-value(vla-get-Coordinates vla-obj-item)) 2))
	   (repeat (1-(length points-list))
	     (setq p1 (append (car points-list) (list (vla-get-Elevation vla-obj-item))))
	     (setq points-list (cdr points-list))
	     (setq p2 (append (car points-list) (list (vla-get-Elevation vla-obj-item))))
	     (setq line-list
		    (append line-list
			    (list
			      (vlax-vla-object->ename
				(draw-line-vla-by-list-property
				  (list
				    (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
				    (cons "StartPoint" p1)
				    (cons "EndPoint" p2)
				    (cons "Lineweight"(vla-get-Lineweight vla-obj-item))
				    (cons "Layer"(vla-get-Layer vla-obj-item))
				    (cons "Color"(vla-get-Color vla-obj-item))
				    (cons "Linetype" (vla-get-Linetype vla-obj-item))
				    ))
				)
			      )))
	     )
	   (entdel item)
	   )
	  ((= (cdr(assoc 0 (entget item))) "POLYLINE")
	   (setq vla-obj-item (vlax-ename->vla-object item))
	   (setq points-list (convert-safearray-to-list-points (vlax-variant-value(vla-get-Coordinates vla-obj-item)) 3))
	   (repeat (1-(length points-list))
	     (setq p1 (car points-list))
	     (setq points-list (cdr points-list))
	     (setq p2 (car points-list))
	     (setq line-list
		    (append line-list
			    (list
			      (vlax-vla-object->ename
				(draw-line-vla-by-list-property
				  (list
				    (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
				    (cons "StartPoint" p1)
				    (cons "EndPoint" p2)
				    (cons "Lineweight"(vla-get-Lineweight vla-obj-item))
				    (cons "Layer"(vla-get-Layer vla-obj-item))
				    (cons "Color"(vla-get-Color vla-obj-item))
				    (cons "Linetype" (vla-get-Linetype vla-obj-item))
				    ))
				)
			      )))
	     )
	   (entdel item)
	   )
	  ((= (cdr(assoc 0 (entget item))) "LINE")
	   (setq line-list (append line-list (list(ssname nabor n))))
	   )
	  (T
	   (princ "\nОшибка не предвиденный тип объекта!"))
	  )
	(setq n (1+ n)))
      )
    )
  line-list
  )