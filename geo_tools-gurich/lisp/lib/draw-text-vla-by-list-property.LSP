(defun draw-text-vla-by-list-property(input-list / text)
;;;  создание примитива Text средствами ActiveX
;;;  пример
;|
  (draw-text-vla-by-list-property
    (list
      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
      (cons "TextString" "zzz")
      (cons "InsertionPoint" (list 10 10 0))
      (cons "Height" 10)
      (cons "Rotation" (/ pi 2))
      (cons "Color" 1)
      (cons "Alignment" 4)
      ))
|;
  (setq text (vla-AddText (cdr(assoc "spece" input-list)) (cdr(assoc "TextString" input-list)) (vlax-3D-point(cdr(assoc "InsertionPoint" input-list))) (cdr(assoc "Height" input-list))))
  (foreach x input-list
    (if (vlax-property-available-p text (car x) T)
      (if (or (wcmatch (car x)"*Point") (wcmatch (car x)"Normal"))
	(eval(list(read(strcat "vla-put-" (car x)))text(vlax-3D-point(cdr x))))
	(eval(list(read(strcat "vla-put-" (car x)))text(cdr x)))
	)
      )
    )
  (if (not(eq (cdr(assoc "Alignment" input-list)) 0))
  (vla-put-TextAlignmentPoint text (vlax-3D-point(cdr(assoc "InsertionPoint" input-list))))
    )
  text
  )