(defun profile-draw-intersection-lines (block-profile-obj /
					block-obj buffer-zone curent-text-style h-text horizontal-scale
					insert-point intersection-line-obj intersection-lines-type-list lines-type-list
					list-all-lines-type list-points n nabor result-list
					scale-coefficient start-point str-all-lines-type vertical-scale temp x 2Dpline
					profile-parameters profile-string-list)
  (setq 2Dpline (trace-create-2Dpline-by-block-profile block-profile-obj))
  (setq block-obj (cdr(assoc "block"(vlax-ldata-get block-profile-obj"obj-list"))))
  (setq vertical-scale (vlax-ldata-get block-profile-obj"vertical-scale"))
  (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
  (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
  
  (setq profile-string-list(vlax-ldata-get block-profile-obj"profile-string-list"))
  (setq profile-parameters(vlax-ldata-get block-profile-obj"profile-parameters"))
  (setq buffer-zone(*(atof(nth 3 profile-parameters))scale-coefficient))
  (setq start-point(mapcar'+ (vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj)))(list buffer-zone 0)))
  (setq curent-text-style (nth 1 profile-parameters))
  (if(or(eq curent-text-style "***Текущий***")(not(member curent-text-style(get-text-style-list))))
    (setq curent-text-style(getvar "TEXTSTYLE")))
  (setq lines-type-list (profile-get-intersection-lines-type-list))
  (setq h-text (*(atof(nth 9 profile-parameters))scale-coefficient))
  (setq intersection-lines-type-list(profile-get-intersection-lines-type-list))
  (setq list-all-lines-type(mapcar 'car intersection-lines-type-list))
  (setq str-all-lines-type (car list-all-lines-type))
  (setq list-all-lines-type (cdr list-all-lines-type))
  (mapcar '(lambda (x)
	     (setq str-all-lines-type(strcat str-all-lines-type"," x))
	     ) ;_ end of lambda
	  list-all-lines-type)
  (setq nabor (ssget "_X" (list '(0 . "LINE,LWPOLYLINE")(cons 6 str-all-lines-type))))
  (if nabor
    (progn
      (setq n 0)
      (repeat (sslength nabor)
	(setq intersection-line-obj(ssname nabor n))
	(if (setq list-points(analisis-intersect-obj 2Dpline intersection-line-obj))
	  (foreach x list-points
	    (setq insert-point (list(+(nth 0 start-point)(vlax-curve-getDistAtPoint 2Dpline x))(nth 1 start-point)))
	    (setq result-list(append result-list(list(draw-line-vla model_spece insert-point (mapcar '+ insert-point (list 0 100))))))
	    (setq temp(draw_text_vla model_spece (cadr(assoc (cdr(assoc 6 (entget intersection-line-obj))) intersection-lines-type-list))
						       (mapcar '+ insert-point (list 0 (/ h-text 2))) h-text 12 nil nil T T))
	    (vla-put-StyleName temp curent-text-style)
	    (setq result-list(append result-list(list temp)))
	    )
	  )
	(setq n (1+ n))
	)
      )
    )
  (vla-erase 2Dpline)
  result-list
  )