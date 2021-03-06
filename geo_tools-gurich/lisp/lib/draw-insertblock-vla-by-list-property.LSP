(defun draw-insertblock-vla-by-list-property (input-list / insertblock)
  ;;;  �������� ��������� insertblock ���������� ActiveX
  ;;;  (draw-insertblock-vla-by-list-property
  ;;;    (list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
  ;;;	 (cons "InsertionPoint" '(0 0 0))
  ;;;	 (cons "EffectiveName" "��� �����")
  ;;;	 (cons "XScaleFactor" 1.0)
  ;;;    (cons "YScaleFactor" 1.0)
  ;;;    (cons "ZScaleFactor" 1.0)
  ;;;	 (cons "Rotation" 0.0)
  ;;;	 ))
  (setq block(vla-insertblock
	       (cdr(assoc "spece" input-list))
	       (vlax-3D-point(cdr(assoc"InsertionPoint" input-list)))
	       (cdr(assoc"EffectiveName" input-list))
	       (cdr(assoc"XScaleFactor" input-list))
	       (cdr(assoc"YScaleFactor" input-list))
	       (cdr(assoc"ZScaleFactor" input-list))
	       (cdr(assoc"Rotation" input-list))
	       ))
  (foreach x input-list
    (if (vlax-property-available-p block (car x) T)
      (if (not(or(eq (car x)"InsertionPoint")
		 (eq (car x)"EffectiveName")
		 (eq (car x)"InsertionPoint")
		 (eq (car x)"XScaleFactor")
		 (eq (car x)"YScaleFactor")
		 (eq (car x)"ZScaleFactor")
		 (eq (car x)"Rotation")))
	(eval(list(read(strcat "vla-put-" (car x)))block(cdr x)))
	)
      )
    )
  (vla-update block)
  block
  )