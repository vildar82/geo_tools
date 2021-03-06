(defun geo-tools-load-support (patch / list-extension list-files num-error num-ignor num-loads)
  ; ��������� ��� ����������� (*.lsp *.fas *.vlx) �����  � �������� ��������
  (setq list-extension (list "lsp" "fas" "vlx"))
  (setq num-loads 0)
  (setq num-ignor 0)
  (setq num-error 0)
  (foreach x list-extension
    (setq list-files (append list-files(file_find_be_extension patch x))))
  (foreach y list-files
    (if (wcmatch y "*=dont-load=*")
      (setq num-ignor (1+ num-ignor))
      (if
	(vl-catch-all-error-p(vl-catch-all-apply'load (list y)))
	(progn
	  (princ (strcat "\n��������! Geo Tools �� ������� ��������� ����:\n" y))
	  (setq num-error (1+ num-error))
	  )
	(setq num-loads (1+ num-loads))
	)
      )
    )
  (if(> num-loads 0)(princ(strcat "\n��������� �������������� ������ "(itoa num-loads))))
  (if(> num-ignor 0)(princ(strcat "\n��������������� �������������� ������ "(itoa num-ignor))))
  (if(> num-error 0)(princ(strcat "\n������ �������� �������������� ������ "(itoa num-error))))
  (princ)
  )