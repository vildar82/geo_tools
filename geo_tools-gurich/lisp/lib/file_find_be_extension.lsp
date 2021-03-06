(defun file_find_be_extension (patch extension / list_directory new_directoryes new_files list_files result_list)
  ; ������ ����� � ��������� �������� (������� ��������) � ��������� ����������� (���� nil �� ������� ��� ��� �������)
  (setq list_directory (list patch))
  (while (not(eq list_directory nil))
    (setq new_directoryes (cddr(vl-directory-files (car list_directory) nil -1)))
    (setq new_files (vl-directory-files (car list_directory) nil 1))
    (if new_files (setq list_files(append list_files (file_name_list_to_patch new_files(car list_directory)))))
    (if new_directoryes (setq list_directory(append list_directory (file_name_list_to_patch new_directoryes(car list_directory)))))
    (setq list_directory(cdr list_directory))
    )
  (if extension
    (progn
      (repeat (length list_files)
	(if (or(eq (vl-filename-extension (car list_files)) (strcat "."(strcase extension T)))
	       (eq (vl-filename-extension (car list_files)) (strcat "."(strcase extension nil))))
	  (setq result_list(append result_list(list(car list_files)))))
	(setq list_files (cdr list_files)))
      (setq list_files result_list)
      )
    )
  list_files
  )
