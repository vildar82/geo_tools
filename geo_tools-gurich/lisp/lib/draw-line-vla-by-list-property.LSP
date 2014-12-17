(defun draw-line-vla-by-list-property(input-list / text)
;;;  создание примитива Line средствами ActiveX
;| пример
  (draw-line-vla-by-list-property
    (list
      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
      (cons "StartPoint" (list 10 10 0))
      (cons "EndPoint" (list 0 0 0))
      (cons "Color" 1)
      ))
|;
  (setq text (vla-AddLine (cdr(assoc "spece" input-list)) (vlax-3D-point(cdr(assoc "StartPoint" input-list))) (vlax-3D-point(cdr(assoc "EndPoint" input-list)))))
  (foreach x input-list
    (if (vlax-property-available-p text (car x) T)
      (if (wcmatch (car x)"*Point")
	(eval(list(read(strcat "vla-put-" (car x)))text(vlax-3D-point(cdr x))))
	(eval(list(read(strcat "vla-put-" (car x)))text(cdr x)))
	)
      )
    )
  text
  )