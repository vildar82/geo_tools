(defun C:geo_tools_uninstall ( / acad_ver support-paths QNewTemplate_File ToolPalette_Path file n word)
  (geo_tools-help "geo_tools_uninstall")
  (initget "Да Нет")(setq word (getkword "\nВ самом деле требуется удалить Geo Tools? [Да/Нет] <Нет>: "))
  (if (eq word "Да")
    (progn
      (setq acad_ver(getvar "ACADVER"))
      ; ----------------------------------------------------------------
      (setq support-paths (convert-string-to-list-be-delimiter (vla-get-SupportPath  (vla-get-files (vla-get-Preferences (vlax-get-acad-object)))) ";"))
      (foreach x (list
		   ""
		   "\\bloks"
		   "\\data"
		   "\\fonts"
		   "\\helps"
		   "\\lisp"
		   )
	(if (setq n(analysis-number-item-in-list (strcat *geo_tools_root_dir* x) support-paths))
	  (setq support-paths (edit-list-del-item-by-number n support-paths)))
	)
      (setq support-paths(apply 'strcat(listtext-to-string-add support-paths ";")))
      (vla-put-SupportPath  (vla-get-files (vla-get-Preferences (vlax-get-acad-object))) support-paths)
      ; удаление в путях поддержки папок Geo Tools
      
      (vla-put-QNewTemplateFile  (vla-get-files (vla-get-Preferences (vlax-get-acad-object)))
	(strcat (vla-get-TemplateDwgPath  (vla-get-files (vla-get-Preferences (vlax-get-acad-object))))"\\acad"(if (get-civil-or-not) "iso" "")".dwt"))
      
      (vla-put-ToolPalettePath  (vla-get-files (vla-get-Preferences (vlax-get-acad-object)))
	(strcat (substr
		  (vla-get-MenuFile  (vla-get-files (vla-get-Preferences (vlax-get-acad-object)))) 1
		  (1+(vl-string-position (ascii "\\")(vla-get-MenuFile  (vla-get-files (vla-get-Preferences (vlax-get-acad-object)))) 0 T)))
		"ToolPalette"))
      ; возвращение шаблона и путя для ToolPalette по умолчанию
      )
    )
  (progn)
  )
(vl-string-position (ascii "\\")(vla-get-MenuFile  (vla-get-files (vla-get-Preferences (vlax-get-acad-object)))) 0 T)