(defun draw-3DPoly-vla-by-list-property(input-list / polyline)
  ;;;  создание примитива Polyline средствами ActiveX
  (setq polyline(vla-Add3DPoly(cdr(assoc "spece" input-list))
		  (convert-list-points-to-polyline-safearray(cdr(assoc"Coordinates"input-list)))))
  (foreach x input-list
    (if x
      (if (vlax-property-available-p polyline (car x) T)
	(if (not(eq (car x)"Coordinates"))
	  (eval(list(read(strcat "vla-put-" (car x)))polyline(cdr x)))
	  )
	)
      )
    )
  polyline
  )
