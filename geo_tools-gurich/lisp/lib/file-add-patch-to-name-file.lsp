(defun file-add-patch-to-name-file (new_directoryes dir / list_directory); ���������� ������ � ����� ���������� � �����������
  (repeat (length new_directoryes)
    (setq list_directory(append list_directory(list(strcat dir "\\"(car new_directoryes)))))
    (setq new_directoryes (cdr new_directoryes))
    )
  list_directory
  )