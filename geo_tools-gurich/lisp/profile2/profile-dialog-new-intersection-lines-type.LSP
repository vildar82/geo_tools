(defun profile-dialog-new-intersection-lines-type (dcl_id sort-list-name-lines-type / file-ltype-list list-ltype red-ltype-list description sort-list-name-lines-type-lite)
  (new_dialog "new_intersection_lines_type" dcl_id)
  (setq file-ltype-list (get-ltype-list))
  (setq sort-list-name-lines-type-lite(acad_strlsort(mapcar 'car sort-list-name-lines-type)))
  (foreach x file-ltype-list
    (if (not(member x sort-list-name-lines-type-lite))
    (setq red-ltype-list(append red-ltype-list(list x)))
      )
    )
  (setq list-ltype 0)
  (start_list "list-ltype")
  (mapcar 'add_list red-ltype-list)
  (end_list)
  (set_tile "list-ltype" (itoa list-ltype))
  (action_tile "list-ltype"
    (vl-prin1-to-string
      '(progn
	(setq list-ltype (atoi $value))
	)
      )
    )
  (action_tile "description"
    (vl-prin1-to-string
      '(progn
	(setq description $value)
	)
      )
    )
  (if (>(start_dialog)0)(list (nth list-ltype red-ltype-list)description) nil)
  )