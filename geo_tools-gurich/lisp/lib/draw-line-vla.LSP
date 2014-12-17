(defun draw-line-vla (spece insert-point-1 insert-point-2 / obj-line)
  (setq obj-line(vla-AddLine spece(vlax-3D-point insert-point-1)(vlax-3D-point insert-point-2)))
  (vla-put-Linetype obj-line "Continuous")
  obj-line
  )