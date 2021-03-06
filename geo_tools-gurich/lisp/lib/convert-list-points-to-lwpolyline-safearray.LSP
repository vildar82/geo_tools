(defun convert-list-points-to-lwpolyline-safearray (input-list / temp)
  (setq temp (apply 'append (mapcar 'list (mapcar 'car input-list)(mapcar 'cadr input-list))))
  (vlax-make-variant(vlax-safearray-fill
		      (vlax-make-safearray vlax-vbDouble (cons 0  (1-(length temp))))
			temp))
  )