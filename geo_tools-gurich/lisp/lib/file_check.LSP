(defun file_check (patch / file) ; �������� ��� �����
  (setq file (open patch "r"))
  (if file(progn(close file)T)nil))
