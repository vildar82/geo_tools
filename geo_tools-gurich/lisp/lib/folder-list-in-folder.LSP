(defun folder-list-in-folder (koren / folder-list list_directory new_directoryes)
  (setq list_directory(list koren))
  (while (not(eq list_directory nil))
    (setq new_directoryes (cddr(vl-directory-files (car list_directory) nil -1)))
    (if new_directoryes(setq folder-list (append folder-list(file-add-patch-to-name-file new_directoryes(car list_directory)))))
    (if new_directoryes(setq list_directory(append list_directory(file-add-patch-to-name-file new_directoryes(car list_directory)))))
    (setq list_directory(cdr list_directory))
    )
  folder-list
  )