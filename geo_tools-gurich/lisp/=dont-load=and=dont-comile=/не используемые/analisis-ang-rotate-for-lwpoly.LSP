(defun analisis-ang-rotate-for-lwpoly (lwpolyline-obj / spisokv n p1 p2 p3 a b g angles)
  ; возвращает значение измеренных углов полилинии в списке
  (setq spisokv (list-coordinates-lwpolyline lwpolyline-obj))
  (setq n (- (length spisokv) 2))
  (repeat n
    (setq p1 (car spisokv))
    (setq spisokv (cdr spisokv))
    (setq p2 (car spisokv))
    (setq p3 (cadr spisokv))
    (setq a (angle p2 p1))
    (setq b (angle p2 p3))
    (cond
      ((> a b)
       (setq g (- a b)))
      ((< a b)
       (setq g (-(* pi 2)(- b a))))
      )
    (setq angles (append angles (list g)))
    )
  angles
  )