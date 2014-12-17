(defun draw-mtext-vla-by-list-property(input-list / mtext)
;;;  создание примитива mtext средствами ActiveX
;;;  пример
;|
  (draw-mtext-vla-by-list-property
    (list
      (cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
      (cons "TextString" "zzz")
      (cons "InsertionPoint" (list 10 10 0))
      (cons "Width" 20)
      (cons "Height" 10)
      (cons "Rotation" (/ pi 2))
      (cons "Color" 1)
      (cons "AttachmentPoint" 4)
      ))
|;
  (setq mtext (vla-AddMText (cdr(assoc "spece" input-list))  (vlax-3D-point(cdr(assoc "InsertionPoint" input-list))) (cdr(assoc "Width" input-list))(cdr(assoc "TextString" input-list))))
  (foreach x input-list
    (if (vlax-property-available-p mtext (car x) T)
      (if (and (wcmatch (car x)"*Point")(not(eq (car x)"AttachmentPoint")))
	(eval(list(read(strcat "vla-put-" (car x)))mtext(vlax-3D-point(cdr x))))
	(eval(list(read(strcat "vla-put-" (car x)))mtext(cdr x)))
	)
      )
    )
  (if (not(eq (cdr(assoc "AttachmentPoint" input-list)) 0))
    (vla-put-InsertionPoint mtext (vlax-3D-point(cdr(assoc "InsertionPoint" input-list))))
    )
  mtext
  )