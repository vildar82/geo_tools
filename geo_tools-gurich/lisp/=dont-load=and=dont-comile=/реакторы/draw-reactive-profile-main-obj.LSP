(defun draw-reactive-profile-main-obj (horizon-elevation start-profile-point / )
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  (setq profile-main-obj (vla-AddText model_spece (rtos horizon-elevation 2 3) (vlax-3D-point start-profile-point) 2.5))
  (vla-put-Alignment profile-main-obj 14)
  (vla-put-TextAlignmentPoint profile-main-obj (vlax-3D-point start-profile-point))
  (vla-put-Rotation profile-main-obj 0)
  (vla-put-Linetype profile-main-obj "Continuous")
  profile-main-obj
  )