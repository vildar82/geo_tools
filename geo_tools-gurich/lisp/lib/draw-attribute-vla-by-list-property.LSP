(defun draw-attribute-vla-by-list-property(input-list / attribute)
  ;;;  �������� �������� ���������� ActiveX
  (setq attribute
	 (vla-AddAttribute; �������� �������
	   (cdr(assoc "spece" input-list))
	   (cdr(assoc "Height" input-list))
	   (cdr(assoc "Mode" input-list))
	   (cdr(assoc "PromptString" input-list));���������
	   (vlax-3D-point(cdr(assoc "InsertionPoint" input-list)))
	   (cdr(assoc "TagString" input-list));���
	   (cdr(assoc "TextString" input-list));�������� �� ���������
	   ))
  (foreach x input-list ; ���������� ������� ����������
    (if (vlax-property-available-p attribute (car x) T)
      (if (not(or(eq(car x)"Height")
		 (eq(car x)"Mode")
		 (eq(car x)"PromptString")
		 (eq(car x)"InsertionPoint")
		 (eq(car x)"TagString")
		 (eq(car x)"TextString")))
	(eval(list(read(strcat "vla-put-" (car x)))attribute(cdr x)))
	)
      )
    )
  (if (assoc "Alignment" input-list); ��� ��������� �������� "Alignment" ��������������� "InsertionPoint" ������� ������ ����
    (if (not(eq (cdr(assoc "Alignment" input-list)) 0))
      (vla-put-TextAlignmentPoint attribute (vlax-3D-point(cdr(assoc "InsertionPoint" input-list))))
      )
    )
  attribute
  )