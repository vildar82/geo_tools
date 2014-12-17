(defun C:profile_edit_properti_curent_basement ( / block-profile-obj curent-profile output-list profile-mode)
  (geo_tools-help "profile_edit_properti_curent_basement")
  (setq profile-mode (vlax-ldata-get "geo_tools_dictionary" "profile-mode"))
  (if(if (not profile-mode)(profile-seleckt-curent-mode)T)
    (if (=(vlax-ldata-get "geo_tools_dictionary" "profile-mode")1)
      (progn
	(setq curent-profile(vlax-ldata-get "geo_tools_dictionary" "curent-profile"))
	(if(profile-check-curent-profile curent-profile)
	  (progn
	    (princ (strcat"\nТекущий профиль: "(car curent-profile)))
	    (setq block-profile-obj (cdr curent-profile))
	    (setq output-list(profile-dialog-edit-properti-curent-basement
			       (list
				 (vlax-ldata-get block-profile-obj "profile-string-list")
				 (vlax-ldata-get block-profile-obj "profile-parameters")
				 )
			       block-profile-obj))
	    (if output-list
	      (progn
		(vlax-ldata-put block-profile-obj "profile-string-list" (nth 0 output-list))
		(vlax-ldata-put block-profile-obj "profile-parameters" (nth 1 output-list))
		(vlax-ldata-put block-profile-obj "picket-first" (nth 2 output-list))
		)
	      )
	    )
	  )
	)
      (alert "Данная команда не работает в режиме \"старый профиль\"!")
      )
    )
  (princ)
  )