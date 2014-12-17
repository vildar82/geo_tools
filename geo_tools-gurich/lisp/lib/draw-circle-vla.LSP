(defun draw-circle-vla (spece insert-point Radius / circle_obj)
  (setq circle_obj (vla-AddCircle spece (vlax-3D-point insert-point) Radius))
  (vla-put-Linetype circle_obj "Continuous")
  circle_obj
  )