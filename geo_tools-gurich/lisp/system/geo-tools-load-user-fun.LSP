(defun geo-tools-load-user-fun (/ lsp_list lsp_item num-ignor num-error)
  ; загружает все пользовательские файлы *.lsp в каталоге \\lisp 
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
		(princ (strcat "\nВнимание! Geo Tools не удалось загрузить файл:\n" lsp_item))
		(setq num-error (1+ num-error))
		)
	      )
	    )
	  )
	(if (> (length lsp_list) 0)
	  (princ(strcat "\nОбнаружено " (itoa(length lsp_list)) " пользовательских lsp файлов")))
	(if (> num-ignor 0)
	  (princ(strcat "\nПроигнорировано " (itoa num-ignor) " пользовательских lsp файлов")))
	(if (> num-error 0)
	  (princ(strcat "\nНе загружено из за ошибок " (itoa num-error) " пользовательских lsp файлов")))
  (princ)
  )