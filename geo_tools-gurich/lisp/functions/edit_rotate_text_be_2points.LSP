(defun C:edit_rotate_text_be_2points (/ t1 a object ttt flag vla-object) ; ������������ ��������� ����� �� ���� ������ (������ �������� � �������)
  (geo_tools-help "edit_rotate_text_be_2points")
  
  (if
    (setq object (car (entsel "\n������ ����� ��� �����: ")))
    (if
      (not (or (eq (cdr (assoc 0 (entget object))) "TEXT")(eq (cdr (assoc 0 (entget object))) "MTEXT")(eq (cdr (assoc 0 (entget object))) "AECC_COGO_POINT")))
      (progn
	(setq object nil)
	(princ "\n������! ������ ������ ����������������� ����!")
	); progn
      (progn
	(initget)(setq t1(getpoint "\n����� ��������� ����� ����� ���������: ")); �������� �����
	(if (eq t1 nil)
	  (setq a nil)
	  (progn
	    (initget)(setq a (getangle t1 "\n����� ���� �������� ��� ����� ������ ����� ���������: ")); �������� ����
	    ))
	(if (= a nil)
	  (setq a (* pi 1.5))
	  ); �������������� ��������� ������� ��� ������ �����
	(vla-StartUndoMark active_document)
	(while (not flag)
	  (setq ttt (entget object))
	  (cond
	    ((eq (cdr(assoc 0 ttt)) "AECC_COGO_POINT")
	     (setq vla-object (vlax-ename->vla-object object))
	     (vlax-put-property vla-object 'LabelRotation (+ a (/ pi 2)))
	     )
	    ((or (eq (cdr(assoc 0 ttt)) "TEXT") (eq (cdr(assoc 0 ttt)) "MTEXT"))
	     (setq ttt (subst (cons 50 (+ a (/ pi 2)))(assoc 50 ttt) ttt))
	     (entmod ttt)
	     (entupd object)
	     )
	    ); cond
	  (if (setq object (car (entsel "\n������ ����� ��� �����: ")))
	    (if (not (or (eq (cdr (assoc 0 (entget object))) "TEXT")(eq (cdr (assoc 0 (entget object))) "MTEXT")(eq (cdr (assoc 0 (entget object))) "AECC_COGO_POINT")))
	      (progn
		(princ "\n������! ������ ������ ����������������� ����!")
		(setq flag T)
		)
	      )
	    (progn
	      (princ "\n������ �� �������!")
	      (setq flag T)
	      )
	    )
	  )
	(vla-EndUndoMark active_document)
	
	
	)
      ); if
    (princ "\n������ �� �������!")
    ); if
  
  (princ)
  )
