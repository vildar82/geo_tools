(defun draw-circle-vla-by-list-property(input-list / circle_obj)
  ;;;  создание примитива circle средствами ActiveX
;| пример
  (draw-circle-vla-by-list-property
    (list
      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
      (cons "InsertionPoint" (list 10 10 0))
      (cons "Radius" 20.0)
      (cons "Color" 1)
      ))
|;
  (setq circle_obj (vla-AddCircle
		     (cdr(assoc "spece" input-list))
		     (vlax-3D-point(cdr(assoc "InsertionPoint" input-list)))
		     (cdr(assoc "Radius" input-list))))
  (foreach x input-list
    (if (vlax-property-available-p circle_obj (car x) T)
      (if (not(or(eq (car x)"InsertionPoint")(eq (car x)"Radius")))
	(eval(list(read(strcat "vla-put-" (car x)))circle_obj(cdr x)))
	)
      )
    )
  circle_obj
  )