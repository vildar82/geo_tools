(defun profile-draw-gorizontal-lines (block-profile-obj / 2Dpline block-obj buffer-zone
				      length-pline n start-point line-obj-list
				      horizontal-scale profile-string-list scale-coefficient)
  ; чертит горизонтальные линии в профиле
  (setq block-obj (cdr(assoc "block"(vlax-ldata-get block-profile-obj"obj-list"))))
  (setq profile-string-list(vlax-ldata-get block-profile-obj"profile-string-list"))
  (setq horizontal-scale(vlax-ldata-get block-profile-obj"horizontal-scale"))
  (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
  (setq buffer-zone(*(atof(nth 3 (vlax-ldata-get block-profile-obj"profile-parameters")))scale-coefficient))
  (setq start-point(vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj))))
  (setq length-pline (trace-get-length (vlax-ldata-get block-profile-obj"coor-trace")))
  (setq line-obj-list (append line-obj-list(list (draw-line-vla model_spece
						   start-point
						   (mapcar '+ start-point (list(+ length-pline buffer-zone)0))))))
  (setq n 0)
  (foreach x profile-string-list
    (setq n (- n(*(atof(nth 2 x))(/ 1.0 (/ 1000.0 horizontal-scale)))))
    (setq line-obj-list (append line-obj-list(list (draw-line-vla model_spece
						     (mapcar'+ start-point(list  0 n))
						     (mapcar'+ start-point(list (+ length-pline buffer-zone) n))))))
    )
  line-obj-list
  )