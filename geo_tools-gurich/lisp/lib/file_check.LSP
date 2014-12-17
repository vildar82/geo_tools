(defun file_check (patch / file) ; проверка сущ файла
  (setq file (open patch "r"))
  (if file(progn(close file)T)nil))
