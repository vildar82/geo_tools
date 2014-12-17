(defun draw-reactive-profile-lwpolyline-obj ( list-3d-points horizon-elevation start-profile-point horizon-scale vertical-scale /
					     list-2d-points p1 p2 z)
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  (repeat (length list-3d-points)
    (setq p1 (car list-3d-points))
    (setq list-3d-points (cdr list-3d-points))
    (setq z (nth 2 p1))
    (if (not (eq z 0.0))
      (setq list-2d-points
	     (append list-2d-points
		     (list
		       (list
			 (nth 0 start-profile-point)
			 (+(nth 1 start-profile-point)
			   (/(- z horizon-elevation)
			     (/ (* vertical-scale (/ 1000 horizon-scale)) 1000)))
			 )))))
    (if (> (length list-3d-points) 0)
      (progn
	(setq p2 (car list-3d-points))
	(setq start-profile-point (list
				    (+ (nth 0 start-profile-point)
				       (distance (list (nth 0 p1) (nth 1 p1)) (list (nth 0 p2) (nth 1 p2))))
				    (nth 1 start-profile-point))))))
;;;  (princ "\n")
;;;  (princ list-2d-points)
    (vlax-make-variant(vlax-safearray-fill
			(vlax-make-safearray vlax-vbDouble (cons 0 (1-(*(length list-2d-points)2))))
			(apply 'append (mapcar 'list (mapcar 'car list-2d-points)(mapcar 'cadr list-2d-points)))))
  )