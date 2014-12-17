(defun draw-arc-vla-by-list-property(input-list / arc)
  ;;;  создание примитива arc средствами ActiveX
  (setq arc (vla-AddArc
	      (cdr(assoc "spece" input-list))
	      (vlax-3D-point(cdr(assoc "Center" input-list)))
	      (cdr(assoc "Radius" input-list))
	      (cdr(assoc "StartAngle" input-list))
	      (cdr(assoc "EndAngle" input-list))))
  (foreach x input-list
    (if (vlax-property-available-p arc (car x) T)
      (if (or (eq (car x)"Normal")(eq (car x)"Center"))
	(eval(list(read(strcat "vla-put-" (car x)))arc(vlax-3D-point(cdr x))))
	(eval(list(read(strcat "vla-put-" (car x)))arc(cdr x)))
	)
      )
    )
  arc
  )