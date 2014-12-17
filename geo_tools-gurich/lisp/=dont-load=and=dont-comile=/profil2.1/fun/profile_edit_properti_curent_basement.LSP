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
	    (profile-dialog-edit-properti-curent-basement (cdr curent-profile))
	    )
	  )
	)
      (alert "Данная команда не работает в режиме \"старый профиль\"!")
      )
    )
  (princ)
  )

(defun profile-dialog-edit-properti-curent-basement (block-profile-obj / dcl_id
						     list-text-style
						     profile-parameters
						     profile-strings
						     sel-profile-strings-item
						     temp
						     )
  
  (defun fun-dialog-set_tile-properti-curent-basement
	 (sel-profile-strings-item profile-strings / temp)
    (foreach x (list
		 (list "name-str" T)
		 (list "h-str" T)
		 (list "sel-units-downgrade" nil)
		 (list "accuracy-text" nil)
		 (list "accuracy-downgrade" nil)
		 (list "h-text" nil)
		 (list "draw-circle" nil)
		 (list "min-angle" T)
		 )
      (if (setq temp(get-item-by-name-in-list (nth sel-profile-strings-item profile-strings) (car x)))
	(set_tile (car x) (convert-item-in-string temp 2))
	(set_tile (car x) "")
	)
      (if (or (cadr x) (not temp))
	(mode_tile (car x) 1)
	(mode_tile (car x) 0)
	)
      )
    )
  
  (defun fun-dialog-action_tile-properti-curent-basement
	 (sel-profile-strings-item profile-strings / temp)
    (foreach x (list
		 (list "name-str" T)
		 (list "h-str" T)
		 (list "sel-units-downgrade" nil)
		 (list "accuracy-text" nil)
		 (list "accuracy-downgrade" nil)
		 (list "h-text" nil)
		 (list "draw-circle" nil)
		 (list "min-angle" T)
		 )
      (setq temp(get-item-by-name-in-list (nth sel-profile-strings-item profile-strings) (car x)))
      (if (and (not(cadr x)) temp)
	(progn
	  (setq profile-strings
		 (edit_replase_in_list_eliment_be_number
		   (put-item-by-name-in-list
		     (nth sel-profile-strings-item profile-strings)
		     (car x)
		     (convert-string-in-item-by-sample (get_tile (car x)) temp))
		   (1+ sel-profile-strings-item)
		   profile-strings))
	  )
	)
      
      )
    profile-strings
    )
  
  (setq dcl_id (geo-tools-load-dialog "profile_dialog_edit_properti_curent_basement"))
  (new_dialog "profile_dialog_edit_properti_curent_basement" dcl_id)
  (setq profile-strings(vlax-ldata-get block-profile-obj"profile-strings"))
  (foreach x (list
	       "text-style"
	       "h-intersection-text"
	       "accuracy-mleader"
	       "picket-first"
	       )
    (setq profile-parameters(append profile-parameters (list (list x (vlax-ldata-get block-profile-obj x)))))
    )
  
  (setq list-text-style (append (get-text-style-list) (list "***Текущий***")))
  
  (start_list "profile-strings")
  (foreach x profile-strings (add_list (get-item-by-name-in-list x "name-str" )))
  (end_list)
  
  (start_list "text-style")
  (mapcar 'add_list list-text-style)
  (end_list)
  
  (setq sel-profile-strings-item 0)
  (set_tile "profile-strings" (itoa sel-profile-strings-item))
  
  (fun-dialog-action_tile-properti-curent-basement-set_tile
    sel-profile-strings-item
    profile-strings)
  
  (foreach x profile-parameters
    (cond
      ((= (car x) "text-style")
       (set_tile "text-style"(itoa (if (setq temp(analysis-number-item-in-list (cadr x) list-text-style))
				     temp
				     (1-(length list-text-style))
				     )
				   )
		 ))
      ((= (car x) "picket-first")
       (set_tile "picket-first" (convert-distance-to-pk-string(cadr x)2 nil))
       )
      (T
       (set_tile (car x) (convert-item-in-string (cadr x) 2)))
      )
    )
  
  (action_tile "profile-strings"
    (vl-prin1-to-string
      '(progn
	(setq profile-strings(fun-dialog-action_tile-properti-curent-basement sel-profile-strings-item profile-strings))
	(setq sel-profile-strings-item (atoi $value))
	(fun-dialog-set_tile-properti-curent-basement
	 sel-profile-strings-item
	 profile-strings)
	)
      )
    )
  
  (action_tile "accept"
    (vl-prin1-to-string
      '(progn
	(setq profile-strings(fun-dialog-action_tile-properti-curent-basement sel-profile-strings-item profile-strings))
	(foreach x profile-parameters
	 (cond
	  ((= (car x) "text-style")
	   (setq profile-parameters(put-item-by-name-in-list profile-parameters "text-style" (nth (atoi (get_tile "text-style"))list-text-style))))
	  ((= (car x) "picket-first")
	   (setq profile-parameters(put-item-by-name-in-list profile-parameters "picket-first" (convert-pk-string-to-distance (get_tile "picket-first")))))
	  (T
	   (setq profile-parameters(put-item-by-name-in-list profile-parameters (car x)
				    (convert-string-in-item-by-sample (get_tile (car x)) (get-item-by-name-in-list profile-parameters (car x)))
				    )))
	  )
	 )
	(done_dialog 1)
	)
      )
    )
  (if (>(start_dialog)0)
    (progn
      (vlax-ldata-put block-profile-obj"profile-strings"profile-strings)
      (foreach x profile-parameters
	(vlax-ldata-put block-profile-obj(car x) (cadr x))
	)
      )
    )
  (unload_dialog dcl_id)
  (princ)
  )