(defun profile-draw-vertical-lines (block-profile-obj 2Dpline /
				    n p spisokv start-point
				    )
  (if(=(vlax-ldata-get "geo_tools_dictionary" "profile-mode")1)
    (setq start-point(vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj))))
    (setq start-point(getpoint"\nУкажи точку на условном горизонте: ")))
  (if start-point
    (progn
      (setq spisokv (extract_coord_lwpolyline (entget 2Dpline)))
      (setq n (length spisokv))
      (repeat n
	(setq p (car spisokv))
	(setq spisokv (cdr spisokv))
	(entmake (list '(0 . "LINE") '(6 . "Continuous") (cons 10 p) (cons 11 (list (nth 0 p)(nth 1 start-point)))))
	)
      )
    )
  )