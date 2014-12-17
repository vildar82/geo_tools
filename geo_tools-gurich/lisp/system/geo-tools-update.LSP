(defun geo-tools-update ( / acad-files acad_ver qnewtemplate_file support-paths toolpalette_path geo_tools-server-patch n profile-list)
  ;выполняется на случай если чего изменилось
  (setq acad-files(vla-get-files (vla-get-Preferences (vlax-get-acad-object))))
  (setq acad_ver(get-acad-ver))
  (if get-curent-version-geo_tools
    (princ (strcat "\nGeo_Tools от " (car(get-curent-version-geo_tools))))
    )
  ; проверка на наличие в путях поддержки папок Geo Tools
  (setq support-paths (convert-string-to-list-be-delimiter (vla-get-SupportPath  acad-files) ";"))
  (foreach x (reverse(list
		       ""
		       "\\bloks"
		       "\\data"
		       "\\fonts"
		       "\\helps"
		       "\\lisp"
		       ))
    (if (not(member(strcat *geo_tools_root_dir* x)support-paths))
      (setq support-paths (append (list (strcat *geo_tools_root_dir* x))support-paths))
      )
    )
  (vla-put-SupportPath
    acad-files
    (apply 'strcat(listtext-to-string-add support-paths ";")))
  ; проверка наличия папок пользовательских данных и создание если не найдены
  (foreach x
	   (list""
		"\\bloks"
		"\\data"
		"\\fonts"
		"\\lisp"
		"\\template"
		"\\uninstall"
		)
    (folder-p-and-create (strcat *geo_tools_user_dat_dir* x))
    )
  (foreach x (folder-list-in-folder (strcat *geo_tools_root_dir* "\\template"))
    (folder-p-and-create (vl-string-subst *geo_tools_user_dat_dir* *geo_tools_root_dir* x))
    )
  
  ; проверка наличия файлов пользовательских данных и создание если не найдены
  (foreach x
	   (list "\\data\\basement-in-profile.dat"
		 "\\data\\baza.dat"
		 "\\data\\intersection-lines-list.dat"
		 "\\data\\profile-standart-str.dat"
		 "\\data\\USER.dat"
		 "\\uninstall\\ver.txt")
    (file-p-and-copy (strcat *geo_tools_user_dat_dir* x)(strcat *geo_tools_root_dir* x))
    )
  
  ; проверка наличия папок и файлов шаблонов, а также создания или копирования если таковые не обнаружены
  (foreach x (file_find_be_extension (strcat *geo_tools_root_dir* "\\template")nil)
    (file-p-and-copy (vl-string-subst *geo_tools_user_dat_dir* *geo_tools_root_dir* x)x)
    )
  (if (not(= (vla-get-QNewTemplateFile  acad-files)
	     (setq QNewTemplate_File(strcat *geo_tools_user_dat_dir* "\\Template\\" acad_ver "\\"(get-string-acad-name)".dwt"))))
    (vla-put-QNewTemplateFile  acad-files QNewTemplate_File)
    )
  
  ; проверка соответствия пути к палитрам инструментов Geo Tools
  (if (not(= (vla-get-ToolPalettePath  acad-files)
	     (setq ToolPalette_Path(strcat *geo_tools_root_dir* "\\ToolPalette"))))
    (vla-put-ToolPalettePath  acad-files ToolPalette_Path)
    )
  ;проверка наличия в параметрах профелей 10 пункта (точность отметок в мультивыноске)
  (setq profile-list(profile-get-sorting-list-profile-dat))
  (setq n 0)
  (repeat (length profile-list)
    (if (not (get-item-deep-list (list n 1 10) profile-list))
      (setq profile-list
	     (put-item-deep-list
	       (list n 1)
	       profile-list
	       (append (get-item-deep-list (list n 1) profile-list) (list "2"))
	       )
	    )
      )
    (setq n (1+ n))
    )
  (put-sorting-list-profile-dat profile-list)
  
  ; проверка на наличие в путях поддержки пользовательских папок Geo Tools
  (setq support-paths (convert-string-to-list-be-delimiter (vla-get-SupportPath  acad-files) ";"))
  (foreach x (reverse(list
		       "\\bloks"
		       "\\data"
		       "\\fonts"
		       "\\lisp"
		       ))
    (if (not(member(strcat *geo_tools_user_dat_dir* x)support-paths))
      (setq support-paths (append (list (strcat *geo_tools_user_dat_dir* x))support-paths))
      )
    )
  (vla-put-SupportPath
    acad-files
    (apply 'strcat(listtext-to-string-add support-paths ";")))
  
  ;проверка загружености группы меню "geo_tools"
  ;;;  (if(vl-catch-all-error-p(vl-catch-all-apply 'vla-Item (list (vla-get-MenuGroups (vlax-get-acad-object)) "geo_tools")))
  ;;;    (vla-load (vla-get-MenuGroups (vlax-get-acad-object)) (strcat *geo_tools_root_dir* "\\geo_tools.cui") :vlax-false))
  
  ;(setq geo_tools-server-patch (get-geo_tools-server-patch))
  ;(geo_tools-server-check-and-update)
  (princ)
  )