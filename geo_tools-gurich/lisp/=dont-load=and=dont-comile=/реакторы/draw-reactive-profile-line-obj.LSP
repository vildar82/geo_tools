(defun draw-reactive-profile-line-obj (start-profile-point axis-trace-obj / profile-line-obj)
  (if model_spece(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  (setq profile-line-obj(vla-AddLine model_spece (vlax-3D-point start-profile-point)(vlax-3D-point (list (+(nth 0 start-profile-point)(vla-get-Length axis-trace-obj)) (nth 1 start-profile-point)))))
  (vla-put-Linetype profile-line-obj "Continuous")
  profile-line-obj
  )