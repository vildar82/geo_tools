(defun profile-dialog-create-new-profile ( / dcl_id sort-list step data-list-dialog)
  (setq dcl_id (geo-tools-load-dialog "dialog_create_new_profile"))
  (setq step 2)
  (setq data-list-dialog (list "Имя нового профиля" 0 100 500 1 100 1 1 1 7 0.0 ))
;|
  (setq data-list-dialog (list
			   (list "profile-name" "Имя нового профиля")
			   (list "view-basement" 0)
			   (list "vertical-scale" 100)
			   (list "horizontal-scale" 500)
			   (list "auto-reference-level" 1)
			   (list "reference-level" 100.00)
			   (list "draw-intersection" 1)
			   (list "draw-points-name" 1)
			   (list "draw-angle" 1)
			   (list "elevation-buffer" 7.0)
			   (list "picket-first" 0.0)
			   ))
  |;
  (while (>= step 2)
    (setq sort-list (profile-get-sorting-list-profile-dat))
    (new_dialog "dialog_create_new_profile" dcl_id)
    
    (start_list "view-basement")
    (foreach x sort-list (add_list (car x)))
    (add_list "Редактировать...")
    (end_list)
    (set_tile "profile-name" (nth 0 data-list-dialog))
    (set_tile "view-basement" (itoa (nth 1 data-list-dialog)))
    (set_tile "vertical-scale" (itoa (nth 2 data-list-dialog)))
    (set_tile "horizontal-scale" (itoa (nth 3 data-list-dialog)))
    (set_tile "auto-reference-level" (itoa (nth 4 data-list-dialog)))
    (set_tile "reference-level" (itoa (nth 5 data-list-dialog)))
    (set_tile "draw_peres" (itoa (nth 6 data-list-dialog)))
    (set_tile "draw_points" (itoa (nth 7 data-list-dialog)))
    (set_tile "draw_ugli" (itoa (nth 8 data-list-dialog)))
    (set_tile "elevation-buffer" (itoa (nth 9 data-list-dialog)))
    (set_tile "picket-first" (convert-distance-to-pk-string (nth 10 data-list-dialog) 2 nil))
    (if (eq (nth 4 data-list-dialog) 1)
      (progn
	(mode_tile "reference-level" 1)
	(mode_tile "elevation-buffer" 0)
	)
      (progn
	(mode_tile "reference-level" 0)
	(mode_tile "elevation-buffer" 1)
	)
      )
    (action_tile "profile-name"
      (vl-prin1-to-string
	'(progn
	  (setq data-list-dialog(replase-item-in-list-by-number $value 0 data-list-dialog))
	  )
	)
      )
    (action_tile "view-basement"
      (vl-prin1-to-string
	'(progn
	  (if
	   (eq (atoi $value) (length sort-list))
	   (done_dialog 3)
	   (setq data-list-dialog(replase-item-in-list-by-number (atoi $value) 1 data-list-dialog))
	   )
	  )
	)
      )
    (action_tile "vertical-scale"
      (vl-prin1-to-string
	'(progn
	  (setq data-list-dialog(replase-item-in-list-by-number (atoi $value) 2 data-list-dialog))
	  )
	)
      )
    (action_tile "horizontal-scale"
      (vl-prin1-to-string
	'(progn
	  (setq data-list-dialog(replase-item-in-list-by-number (atoi $value) 3 data-list-dialog))
	  )
	)
      )
    (action_tile "auto-reference-level"
      (vl-prin1-to-string
	'(progn
	  (setq data-list-dialog(replase-item-in-list-by-number (atoi $value) 4 data-list-dialog))
	  (if (eq (nth 4 data-list-dialog) 1)
	   (progn
	    (mode_tile "reference-level" 1)
	    (mode_tile "elevation-buffer" 0)
	    )
	   (progn
	    (mode_tile "reference-level" 0)
	    (mode_tile "elevation-buffer" 1)
	    )
	   )
	  )
	)
      )
    (action_tile "reference-level"
      (vl-prin1-to-string
	'(progn
	  (setq data-list-dialog(replase-item-in-list-by-number (atoi $value) 5 data-list-dialog))
	  )
	)
      )
    (action_tile "draw_peres"
      (vl-prin1-to-string
	'(progn
	  (setq data-list-dialog(replase-item-in-list-by-number (atoi $value) 6 data-list-dialog))
	  )
	)
      )
    (action_tile "draw_points"
      (vl-prin1-to-string
	'(progn
	  (setq data-list-dialog(replase-item-in-list-by-number (atoi $value) 7 data-list-dialog))
	  )
	)
      )
    (action_tile "draw_ugli"
      (vl-prin1-to-string
	'(progn
	  (setq data-list-dialog(replase-item-in-list-by-number (atoi $value) 8 data-list-dialog))
	  )
	)
      )
    (action_tile "elevation-buffer"
      (vl-prin1-to-string
	'(progn
	  (setq data-list-dialog(replase-item-in-list-by-number (atoi $value) 9 data-list-dialog))
	  )
	)
      )
    (action_tile "set-intersection"
      (vl-prin1-to-string
	'(progn
	  (profile-dialog-set-intersection-lines-type)
	  )
	)
      )
    (action_tile "picket-first"
      (vl-prin1-to-string
	'(progn
	  (setq data-list-dialog(replase-item-in-list-by-number (convert-pk-string-to-distance $value) 10 data-list-dialog))
	  )
	)
      )
    (action_tile "accept"
      (vl-prin1-to-string
	'(progn
	  (if (member (nth 0 data-list-dialog) (profile-get-list-name-profile))
	   (alert "Профиль с таким именем уже существует!\nУкажи другое имя.")
	   (done_dialog 1)
	   )
	  )
	)
      )
    (setq step (start_dialog))
    (if (= step 3)
      (progn
	(setq data-list-dialog(replase-item-in-list-by-number(profile-editor-view-basement (nth 1 data-list-dialog)) 1 data-list-dialog))
	)
      )
    )
  (unload_dialog dcl_id)
  (if (not(= step 0))
    data-list-dialog
    )
  )

(defun chek-in-list-item (lst item / flag1 flag2)
  (setq lst (cddr lst))
  (setq n 0)
  (while flag1
    (if (= n (length lst))
      (setq flag1 T))
    (if (=(cadr (nth n lst))item)
      (progn
	(setq flag1 T)
	(setq flag2 T)
	)
      )
    (setq n (1+ n))
    )
  flag2
  )