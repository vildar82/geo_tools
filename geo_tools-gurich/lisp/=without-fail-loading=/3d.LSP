(defun C:r_ml-hz ( / ANG OBJ P1 P2 P3 point)
  (setq p1 '(-5 5 0)
	p2 '(5 5 0)
	p3 '(5 -5 0))
  (setq obj (vlax-ename->vla-object (car (entsel "\nВыбери текст: "))))
  (princ(calculation_radiany_to_gms(setq ang (angle (trans '(0 0 0) 0 2)(trans '(10 0 0) 0 2)))))
  (setq point (vla-get-InsertionPoint obj))
  (vla-put-Normal obj (vlax-3D-point(calculation-normal-by-3points (trans p2 2 0)(trans p1 2 0)(trans p3 2 0))))
  (vla-put-Rotation obj 0)
  (vla-put-InsertionPoint obj point)
  (princ)
  )

(defun C:r_ml ( / OBJ P1 P2 P3 P10 P20 P30 P40 P50 POINTS)
  ; поворот мультивыноски в пространстве "лицом" к пользователю
  (setq obj (vlax-ename->vla-object (car (entsel "\nВыбери объект: "))))
  (setq points (vlax-safearray->list(vlax-variant-value(vla-GetLeaderLineVertices obj 0)))) ; координаты точек мультивыноски в мировой СК
  (setq p1 (list (nth 0 points)(nth 1 points)(nth 2 points))
	p2 (list (nth 3 points)(nth 4 points)(nth 5 points))
	p3 (list (+(nth 0 p2)8.0)(nth 1 p2)(nth 2 p2))); предпологается что мультивыноска паралельна горизонтальной оси координат(не правильно)
  (setq p10 p1
	p20 (mapcar '- p1 (mapcar '- (trans p1 2 0)(trans p2 2 0)))
	p30 (mapcar '- p1 (mapcar '- (trans p1 2 0)(trans p3 2 0))); пересчитываем координаты точки из мировой системы в систему экрана
	p40 (mapcar '- p20 (mapcar '/ (mapcar '- p20 p2) (list 2.0 2.0 2.0)))
	p50 (mapcar '- p30 (mapcar '/ (mapcar '- p30 p3) (list 2.0 2.0 2.0)));направление на ось поворота
	)
  
  (draw_pline_entmake (list p1 p20 p30) nil T nil nil)
  (draw_pline_entmake (list p1 p50) nil T nil nil)
  (draw_pline_entmake (list p1 p2 p3) nil T nil nil)
  (vla-rotate3d obj (vlax-3d-point p1)(vlax-3d-point p50) pi)
  (setq points (vlax-safearray->list(vlax-variant-value(vla-GetLeaderLineVertices obj 0))))
  (setq p200 (list (nth 3 points)(nth 4 points)(nth 5 points)))
  (draw_pline_entmake (list p20 p200) nil T nil nil)
  (princ(angle-for-2planes (list p1 p20 p30)(list p1 p200 p30)))
  (draw_pline_entmake (list p1 p30) nil T nil nil)
  (vla-rotate3d obj (vlax-3d-point p1)(vlax-3d-point p30) (* -1.0(angle-for-2planes (list p1 p200 p30)(list p1 p20 p30))))
  
  (princ)
  )

(defun angle-for-2planes (plane1 plane2 / a1 a2 b1 b2 c1 c2 temp)
  (setq temp(coefficient-plane plane1))
  (setq a1 (nth 0 temp)	b1 (nth 1 temp)	c1 (nth 2 temp))
  (setq temp(coefficient-plane plane2))
  (setq a2 (nth 0 temp)	b2 (nth 1 temp)	c2 (nth 2 temp))
  (arccos(/(+(* a1 a2)(* b1 b2)(* c1 c2))(*(sqrt(+(* a1 a1)(* b1 b1)(* c1 c1)))(sqrt(+(* a2 a2)(* b2 b2)(* c2 c2))))))
  )

(defun coefficient-plane (plane  / a b c )
  (setq p1 (nth 0 plane) p2 (nth 1 plane) p3 (nth 2 plane))
  (setq a (-(*(-(nth 1 p2)(nth 1 p1))(-(nth 2 p3)(nth 2 p1)))(*(-(nth 1 p3)(nth 1 p1))(-(nth 2 p2)(nth 2 p1)))))
  (setq b (-(*(-(nth 0 p2)(nth 0 p1))(-(nth 2 p3)(nth 2 p1)))(*(-(nth 2 p2)(nth 2 p1))(-(nth 0 p3)(nth 0 p1)))))
  (setq c (-(*(-(nth 0 p2)(nth 0 p1))(-(nth 1 p3)(nth 1 p1)))(*(-(nth 1 p2)(nth 1 p1))(-(nth 0 p3)(nth 0 p1)))))
  (list a b c)
  )

(defun arccos (x /)
  ; функция Arccos угла, заданного в радианах
  (cond
    ((= x 0.0) (/ PI 2.0))
    ((= x 1.0) 0.0)
    ((= x -1.0) PI)
    (t (- (/ PI 2.0) (atan (/ x (sqrt (abs (- 1 (* x x))))))))
    )
  )

(defun arcsin (x /)
  ; функция Arcsin угла, заданного в радианах
  (cond
    ((= x 0.0) 0.0)
    ((= x 1.0) (/ pi 2))
    ((= x -1.0) (/ (- 0 pi) 2))
    (t (atan (/ x (sqrt (abs (- 1 (* x x)))))))
    )
  )