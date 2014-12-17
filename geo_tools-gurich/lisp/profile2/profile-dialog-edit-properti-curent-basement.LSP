(defun profile-dialog-edit-properti-curent-basement (input-list block-profile-obj / dcl_id key-list list-text-style
						     profile-parameters profile-string-list
						     sel-list-podval-str-item result-list picket-first)
  (setq dcl_id (geo-tools-load-dialog "profile_dialog_edit_properti_curent_basement"))
  (new_dialog "profile_dialog_edit_properti_curent_basement" dcl_id)
  (setq profile-string-list (nth 0 input-list))
  (setq profile-parameters (nth 1 input-list))
  (setq picket-first(vlax-ldata-get block-profile-obj"picket-first"))
  (setq list-text-style (append (get-text-style-list) (list "***Текущий***")))
  (start_list "list-podval-str")
  (foreach x profile-string-list (add_list (car x)))
  (end_list)
  
  (setq key-list (list
		   "name"
		   "code"
		   "h-str"
		   "sel-uklon"
		   "dec"
		   "dec-downgrade"
		   "h-txt"
		   "flag-circle"
		   "min-angle"
		   "obj"
		   "list-obj"))
  (start_list "text-style")
  (mapcar 'add_list list-text-style)
  (end_list)
  (set_tile "list-podval-str" (itoa 0))
  (setq sel-list-podval-str-item 0)
  (fun-dialog-action_tile-properti-curent-basement-set_tile
    sel-list-podval-str-item
    key-list
    profile-string-list)
  (set_tile "text-style" (itoa(analysis-number-item-in-list (nth 1 profile-parameters)list-text-style)))
  (set_tile "h-txt-intersect" (nth 9 profile-parameters))
  (set_tile "accuracy-mleader" (nth 10 profile-parameters))
  (set_tile "picket-first" (convert-distance-to-pk-string picket-first 2 nil))
  (action_tile "list-podval-str"
    (vl-prin1-to-string
      '(progn
	(setq sel-list-podval-str-item (atoi $value))
	(fun-dialog-action_tile-properti-curent-basement-set_tile
	 sel-list-podval-str-item
	 key-list
	 profile-string-list)
	)
      )
    )
  (action_tile "sel-uklon"
    (vl-prin1-to-string
      '(progn
	(setq profile-string-list
	 (edit_replase_in_list_eliment_be_number
	  (edit_replase_in_list_eliment_be_number
	   (get_tile "sel-uklon")
	   4
	   (nth sel-list-podval-str-item profile-string-list)
	   )
	  (1+ sel-list-podval-str-item)
	  profile-string-list
	  )
	 )
	)
      )
    )
  (action_tile "dec"
    (vl-prin1-to-string
      '(progn
	(setq profile-string-list
	 (edit_replase_in_list_eliment_be_number
	  (edit_replase_in_list_eliment_be_number
	   (get_tile "dec")
	   5
	   (nth sel-list-podval-str-item profile-string-list)
	   )
	  (1+ sel-list-podval-str-item)
	  profile-string-list
	  )
	 )
	)
      )
    )
  (action_tile "dec-downgrade"
    (vl-prin1-to-string
      '(progn
	(setq profile-string-list
	 (edit_replase_in_list_eliment_be_number
	  (edit_replase_in_list_eliment_be_number
	   (get_tile "dec-downgrade")
	   6
	   (nth sel-list-podval-str-item profile-string-list)
	   )
	  (1+ sel-list-podval-str-item)
	  profile-string-list
	  )
	 )
	)
      )
    )
  (action_tile "h-txt"
    (vl-prin1-to-string
      '(progn
	(setq profile-string-list
	 (edit_replase_in_list_eliment_be_number
	  (edit_replase_in_list_eliment_be_number
	   (get_tile "h-txt")
	   7
	   (nth sel-list-podval-str-item profile-string-list)
	   )
	  (1+ sel-list-podval-str-item)
	  profile-string-list
	  )
	 )
	)
      )
    )

  (action_tile "accept"
    (vl-prin1-to-string
      '(progn
	(setq profile-parameters(edit_replase_in_list_eliment_be_number (nth (atoi(get_tile "text-style")) list-text-style) 2 profile-parameters))
	(setq profile-parameters(edit_replase_in_list_eliment_be_number (get_tile "h-txt-intersect") 10 profile-parameters))
	(setq profile-parameters(edit_replase_in_list_eliment_be_number (get_tile "accuracy-mleader") 11 profile-parameters))
	(setq picket-first(convert-pk-string-to-distance (get_tile "picket-first")))
	(done_dialog 1)
	)
      )
    )
  (if (>(start_dialog)0)(setq result-list (list profile-string-list profile-parameters picket-first)))
  (unload_dialog dcl_id)
  result-list
  )