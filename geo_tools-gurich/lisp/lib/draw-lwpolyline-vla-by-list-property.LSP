(defun draw-lwpolyline-vla-by-list-property(input-list / lwpolyline)
;;;  создание примитива LightWeightPolyline средствами ActiveX
;|
  (draw-lwpolyline-vla-by-list-property
    (list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	 (cons "Coordinates" result-list2)
	 (cons "Lineweight"(vla-get-Lineweight vla-line))
	 (cons "Layer"(vla-get-Layer vla-line))
	 (cons "Elevation"(vla-get-Elevation vla-line))
	 (cons "Color"(vla-get-Color vla-line))
	 (cons "Linetype" (vla-get-Linetype vla-line))
	 ))
|;
  (setq lwpolyline(vla-AddLightWeightPolyline(cdr(assoc "spece" input-list))
		    (convert-list-points-to-lwpolyline-safearray(cdr(assoc"Coordinates"input-list)))))
  (foreach x input-list
    (if (vlax-property-available-p lwpolyline (car x) T)
      (if (not(eq (car x)"Coordinates"))
	(eval(list(read(strcat "vla-put-" (car x)))lwpolyline(cdr x)))
	)
      )
    )
  (vla-update lwpolyline)
  lwpolyline
  )