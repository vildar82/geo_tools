(defun C:options_coordinate_katalog ( / dcl_id)
  (setq dcl_id (geo-tools-load-dialog "options_coordinate_katalog"))
  (new_dialog "options_coordinate_katalog" dcl_id)
  (setq path(get-patch-to-katalog))
  (set_tile "path" path)
  (action_tile "properties_punkts"
    (vl-prin1-to-string
      '(progn
	(properties_punkts_of_katalog path)
	)))
  (action_tile "find"
    (vl-prin1-to-string
      '(progn
	(setq temp (getfiled "Выбор файла каталога" (getvar "DWGPREFIX")  "xml" 0))
	(if temp
	 (progn
	  (setq path temp)
	  (set_tile "path" path)
	  )
	 )
	)))
  (action_tile "path"
    (vl-prin1-to-string
      '(progn
	(setq path(get_tile "path" ))
	)))
  (if(>(start_dialog)0)(put-patch-to-katalog path))
  (unload_dialog dcl_id)
  (princ)
  )