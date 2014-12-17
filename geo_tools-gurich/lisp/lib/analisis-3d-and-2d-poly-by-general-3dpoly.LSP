(defun analisis-3d-and-2d-poly-by-general-3dpoly (axis-2dtrace-obj elevation-3dpoly-obj /
	3d-point list-3d-points 2d-point list-2d-points new-point general-list-points p1 p2 temp)
  ; анализирует два VLA объекта первый двумерна€ полилини€ второй трехмерна€ полилини€
  ; возвращает список снесенных точек трехмерной полилинии на двухмерную, дубликаты
  ; имеющие отметку 0 удал€ютс€
  (setq list-2d-points (list-coordinates-lwpolyline axis-2dtrace-obj))
  (foreach 2d-point list-2d-points
    (setq general-list-points (append general-list-points (list (list(vlax-curve-getParamAtPoint axis-2dtrace-obj 2d-point)
								  (list (car 2d-point) (cadr 2d-point) 0.0)))))
    )
  (setq list-3d-points (list-coordinates-polyline elevation-3dpoly-obj))
  (foreach 3d-point list-3d-points
    (setq new-point (vlax-curve-getClosestPointTo axis-2dtrace-obj (list (car 3d-point) (cadr 3d-point) (vla-get-Elevation axis-2dtrace-obj))))
    (setq general-list-points (append general-list-points (list (list(vlax-curve-getParamAtPoint axis-2dtrace-obj new-point)
								     (list (car new-point) (cadr new-point) (caddr 3d-point)))))))
  (setq general-list-points (vl-sort general-list-points (function (lambda (p1 p2) (< (car p1)(car p2))))))
  (setq general-list-points (apply 'append (mapcar 'list (mapcar 'cadr general-list-points))))
  (setq temp general-list-points)
  (repeat (1- (length general-list-points))
    (setq p1 (car temp))
    (setq temp (cdr temp))
    (setq p2 (car temp))
    (if (and (eq (nth 0 p1)(nth 0 p2))(eq (nth 1 p1)(nth 1 p2)))
      (cond
	((eq (nth 2 p1) 0.0)
	 (setq general-list-points (vl-remove p1 general-list-points)))
	((eq (nth 2 p2) 0.0)
	 (setq general-list-points (vl-remove p2 general-list-points)))
	)))
;;;  (princ "\n")
;;;  (princ general-list-points)
  general-list-points
  )