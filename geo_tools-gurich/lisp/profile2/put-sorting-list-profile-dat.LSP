(defun put-sorting-list-profile-dat (input-list / result-list text-str)
  ; записывает сортированный список подвалов профилей в файл basement-in-profile.dat 
  (foreach profil input-list
    (foreach eliment-prof profil
      (if (eq (type eliment-prof) 'LIST)
	(progn
	  (foreach properties-element eliment-prof
	    (if text-str
	      (setq text-str (strcat text-str "|" properties-element))
	      (setq text-str properties-element)
	      )
	    )
	  (setq result-list(append result-list (list text-str)))
	  (setq text-str nil)
	  )
	(setq result-list(append result-list (list eliment-prof)))
	)
      )
    (setq result-list(append result-list (list "***")))
    )
  (file_write_list_to_file (strcat *geo_tools_user_dat_dir* "\\data\\basement-in-profile.dat") result-list)
  )