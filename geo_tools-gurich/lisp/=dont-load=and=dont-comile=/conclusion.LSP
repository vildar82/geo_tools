(defun conclusion ( / n ent PN P P0 a)
  ;|Функция запрашивает контур (2D-полининию из отрезков) и точку P.
Если контур замкнут и точка не находится на его границе,
  а внутри контура, фунция возвращает T. Иначе - nil.
  Функция также печатает в текстовое окно соответствующее сообщение.
  Может некорректно работать, если полилиния содержит дуги
  http://forum.dwg.ru/showthread.php?t=60792&page=2|;
  
  (setq n 0) ;Начальное значение числа пересечений контуром оси PX.
  (princ "\nУкажите контур (2D-полилинию).")
  (and
    (setq ent (ssget "_:S" '((0 . "LWPOLYLINE"))))
    (setq PN (vl-remove-if '(lambda (x) (/= (car x) 10))
	       (setq ent (entget (ssname ent 0)))
	       )         )
    (or (= (cdr (assoc 70 ent)) 1) (prompt "\nКонтур не замкнут."))
    (setq P (getpoint "\nУкажите точку: "))
    (or
      (not (setq P0 (ssget P)))
      (not (ssmemb (cdar ent) P0))
      (prompt "\nТочка на границе контура.")
      )
    (setq PN (mapcar '(lambda (x) (angle P (cdr x))) PN)
	  P0 (last PN))
    (foreach P PN
      (setq a (- P P0) P0 P)
      (or (<= (abs a) Pi) (setq n (1+ n)))
      )
    (or (/= (rem n 2) 0) (prompt "\nТочка снаружи контура."))
    (princ "\nТочка внутри контура.")
    t
    )
  )