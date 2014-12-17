(defun C:geo_tools_update_user_date ( / acad_ver toggle-list dcl_id)
  ; —брос пользовательских данных до по умолчанию
  (geo_tools-help "geo_tools_update_user_date")
  (setq acad_ver(get-acad-ver))
  (setq dcl_id (geo-tools-load-dialog "d_geo_tools_update_user_date"))
  (new_dialog "d_geo_tools_update_user_date" dcl_id)
  (action_tile "accept"
    (vl-prin1-to-string
      '(progn
	(foreach x (list
		    "basement-in-profile"
		    "profile-standart-str"
		    "intersection-lines-list"
		    "USER"
		    "template"
		    )
	 
	 (setq toggle-list (append toggle-list (list(list x (get_tile x)))))
	 )
	(done_dialog 1)
	)
      )
    )
  (if (>(start_dialog)0)
    (foreach x toggle-list
      (if (convert-string-to-bool (cadr x))
	(if (= (car x) "template")
	  (vlax-invoke-method *file-system-object* 'CopyFile
	    (strcat *geo_tools_root_dir* "\\Template\\" acad_ver "\\"(get-string-acad-name)".dwt")
	    (strcat *geo_tools_user_dat_dir* "\\Template\\" acad_ver "\\"(get-string-acad-name)".dwt")t)
	  (vlax-invoke-method *file-system-object* 'CopyFile
	    (strcat *geo_tools_root_dir* "\\data\\"(car x)".dat")
	    (strcat *geo_tools_user_dat_dir* "\\data\\"(car x)".dat")t)
	  )
	)
      )
    )
  (unload_dialog dcl_id)
  (princ)
  )