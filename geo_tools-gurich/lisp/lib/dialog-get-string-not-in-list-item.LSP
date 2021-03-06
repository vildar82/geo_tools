(defun dialog-get-string-not-in-list-item (input-list top str / dcl_id text-str)
  ; ���������� ���� ������� ��������� ������ � ��������� �������� ������ ������, �
  ; ����� ��������� ��������� �������� � ������
  (setq dcl_id (geo-tools-load-dialog "d_get_string"))
  (setq text-str str)
  (new_dialog "d_get_string" dcl_id)
  (set_tile "text-str" str)
  (set_tile "top" top)
  (action_tile "text-str"
    (vl-prin1-to-string
      '(progn
	(setq text-str $value)
	)
      )
    )
  (action_tile "ok"
    (vl-prin1-to-string
      '(progn
	(if (not(member text-str input-list))
	 (if (eq text-str "")
	  (alert "������ �� �������!")
	  (done_dialog 2)
	  )
	 (alert (strcat top " ������ ���� ����������!")))
	)
      )
    )
  (if (>(start_dialog)1)text-str nil)
  )