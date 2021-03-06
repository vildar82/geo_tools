(defun create-table-style (/ item)
  ; ������� ����� ������ �������� ���������
  (if(vl-catch-all-error-p
       (vl-catch-all-apply 'vla-item
	 (list (vla-item (vla-get-dictionaries
			   (vla-get-activedocument (vlax-get-acad-object))
			   )"acad_tablestyle") "����������"))); ��������� ������� ���������� ����� ������
    (progn
;;;      (create-text-style "OTI" "ESKDU.shx")
      (setq
	item (vla-addobject
	       (vla-item (vla-get-dictionaries
			   (vla-get-activedocument (vlax-get-acad-object))
			   );_ end of vla-get-dictionaries
			 "acad_tablestyle"
			 );_ end of vla-item
	       "����������"
	       "AcDbTableStyle"
	       );_ end of vla-AddObject
	);_ end of setq
      (vla-put-name item "����������"); ��� �����
      (vla-put-description item "������� �������� ���������"); �������� �����
      (vla-put-flowdirection item 0); ����������� (?)
      (vla-put-horzcellmargin item 0.5); �����.������ ������ �� ������� ������
      (vla-put-vertcellmargin item 0.5); ����.������ ������ �� ������� ������
      (vla-settextheight
	item
	acdatarow
	2.0
	); ������ ������ �� ��������� ��� datacells
      (vla-settextheight
	item
	acheaderrow
	2.0
	); ������ ������ ��� ����� �������
      (vla-settextheight
	item
	actitlerow
	2.0
	); ������ ������ ��� ��������� �������
      (vla-settextstyle
	item
	(+ acdatarow acheaderrow actitlerow)
	(getvar "TEXTSTYLE")
	); ����� ������ �� ��������� ��� ���� �����
      (vla-setgridvisibility
	item
	(+ achorztop acvertleft acvertright)
	actitlerow
	:vlax-false
	);_ end of vla-SetGridVisibility ��������� ����� ������ ��������
      (vla-setgridvisibility
	item
	(+ achorzinside acvertinside achorztop achorzbottom acvertleft acvertright)
	(+ acdatarow acheaderrow)
	:vlax-true
	);_ end of vla-SetGridVisibility �������� ����� ������ ����� ���������� (�� ���� �����:))
      T
      )
    nil
    )
  );_ end of defun