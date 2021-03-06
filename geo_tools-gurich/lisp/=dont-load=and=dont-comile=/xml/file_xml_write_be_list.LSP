(defun file_xml_write_be_list ( patch output-list / )
  ; ������ ������ � ���� ������� ����� xml
  (file_write_list_to_file patch
    (list(strcat "<?xml version=\"1.0\" encoding=\"windows-1251\"?>"
		 "<list>"
		 (apply 'strcat(list-to-textlist-xml output-list))
		 "</list>")))
  )

(defun list-to-textlist-xml (input-list / item)
  ; ��������� (���� xml) ������������� ������
  (if input-list
    (append
      (if (eq (type (setq item (car input-list))) 'LIST)
	(append
	  (list"<list>")
	  (list-to-textlist-xml item)
	  (list"</list>")
	  )
	(list
	  "<item>"
	  (strcat "type="(vl-prin1-to-string (type item)))
	  (strcat " value="(convert-item-to-text item))
	  "</item>")
	)
      (list-to-textlist-xml (cdr input-list))
      )
    )
  )

(defun convert-item-to-text (item /)
  ; �������������� ������ � ��������� ����� � ���� ������, �������� ����� ��������������� � 16 ������� ����� �������, ������ ������������ ��� ���������
  (cond
    ((= (type item) 'INT)
     (itoa item)
     )
    ((= (type item) 'REAL)
     (rtos item 2 16)
     )
    ((= (type item) 'STR)
;;;     (strcat "\""item"\"")
     item
     )
    )
  )