(defun v_norm_3p (p1 p2 p3 / )
  ;; Функция вычисления вектора нормали по трем точкам
  (v_norm_2v (mapcar (function -) p1 p2) (mapcar (function -) p1 p3))
  )

(defun v_norm_2v (v1 v2)
  ;; ElpanovEvgeniy 03.12.2005
  ;; Функция вычисления вектора нормали по двум векторам
  ;; (v_norm_2v v1 v2)
  ;; (v_norm_2v '(5 10 0) '(1 2 3))
  ((lambda (a b)
     (mapcar (function (lambda (a1 b1 a2 b2) (- (* a1 b1) (* a2 b2))))
	     a
	     (cdr b)
	     b
	     (cdr a)
	     ) ;_  mapcar
     ) ;_  lambda
    (list (cadr v1) (caddr v1) (car v1) (cadr v1))
    (list (cadr v2) (caddr v2) (car v2) (cadr v2))
    ) ;_  lambda
  )