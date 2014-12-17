(defun file-list-in-folder (koren / list_directory new_directoryes new_files list_files)
  (setq list_directory (list koren))
  (while (not(eq list_directory nil))
    (setq new_directoryes (cddr(vl-directory-files (car list_directory) nil -1)))
    (setq new_files (vl-directory-files (car list_directory) nil 1))
    (if new_files (setq list_files(append list_files (file-add-patch-to-name-file new_files(car list_directory)))))
    (if new_directoryes (setq list_directory(append list_directory (file-add-patch-to-name-file new_directoryes(car list_directory)))))
    (setq list_directory(cdr list_directory))
    )
  list_files
  )

