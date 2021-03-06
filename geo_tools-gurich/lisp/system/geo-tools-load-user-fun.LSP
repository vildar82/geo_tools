(defun geo-tools-load-user-fun (/ lsp_list lsp_item num-ignor num-error)
  ; ��������� ��� ���������������� ����� *.lsp � �������� \\lisp 
	(setq lsp_list (file_find_be_extension (strcat *geo_tools_user_dat_dir* "\\lisp") "lsp"))
	(setq num-ignor 0)
	(setq num-error 0)
	(foreach lsp_item lsp_list
	  (if (wcmatch lsp_item "*=dont-load=*")
	    (setq num-ignor (1+ num-ignor))
	    (if
	      (vl-catch-all-error-p
		(vl-catch-all-apply
		  'load
		  (list
		    lsp_item
		    )
		  ))
	      (progn
		(princ (strcat "\n��������! Geo Tools �� ������� ��������� ����:\n" lsp_item))
		(setq num-error (1+ num-error))
		)
	      )
	    )
	  )
	(if (> (length lsp_list) 0)
	  (princ(strcat "\n���������� " (itoa(length lsp_list)) " ���������������� lsp ������")))
	(if (> num-ignor 0)
	  (princ(strcat "\n��������������� " (itoa num-ignor) " ���������������� lsp ������")))
	(if (> num-error 0)
	  (princ(strcat "\n�� ��������� �� �� ������ " (itoa num-error) " ���������������� lsp ������")))
  (princ)
  )