(defun convert_coords_list_from_safearray_mleader (list_points / n pt pt_array); создает безапасный массив для создания мультивыноски
  (load_global_variable)
  (setq
    pt_array(vlax-make-safearray vlax-vbdouble (cons 1 (* 3 (length list_points))))
    n 0
    )
  (while (< n (length list_points))
    (setq pt (nth n list_points))
    (vlax-safearray-put-element pt_array (+ (* n 3) 1) (car pt))
    (vlax-safearray-put-element pt_array (+ (* n 3) 2) (cadr pt))
    (vlax-safearray-put-element pt_array (+ (* n 3) 3) (caddr pt))
    (setq n (1+ n))
    )
  pt_array
  )
