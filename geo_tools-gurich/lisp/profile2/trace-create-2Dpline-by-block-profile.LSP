(defun trace-create-2Dpline-by-block-profile (block-profile-obj / coor-trace)
  (setq coor-trace(vlax-ldata-get block-profile-obj"coor-trace"))
  (if coor-trace
    (progn
      (if (vlax-ldata-get block-profile-obj "trace-2Dpline")(vlax-ldata-delete block-profile-obj "trace-2Dpline"))
      (draw-lwpolyline-vla-by-list-property
	(list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	     (cons "Coordinates" coor-trace)
	     ))
      )
    (progn
      
      (vlax-ldata-put block-profile-obj "coor-trace" (edit-list-del-duble-points(list-coordinates-lwpolyline (vlax-ldata-get block-profile-obj"trace-2Dpline"))))
      (vlax-ldata-delete block-profile-obj "trace-2Dpline")
      (setq coor-trace(vlax-ldata-get block-profile-obj"coor-trace"))
      (draw-lwpolyline-vla-by-list-property
	(list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	     (cons "Coordinates" coor-trace)
	     ))
      )
    )
  )