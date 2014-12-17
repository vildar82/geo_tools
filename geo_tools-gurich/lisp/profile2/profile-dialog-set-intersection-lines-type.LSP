(defun profile-dialog-set-intersection-lines-type (/ dcl_id sort-list-name-lines-type list-intersection-lines file-lines-type-list result-list)
  (setq dcl_id (geo-tools-load-dialog "set_intersection_lines_type"))
  (setq list-intersection-lines 0)
  (setq file-lines-type-list (profile-get-intersection-lines-type-list))
  (foreach x (acad_strlsort(mapcar 'car file-lines-type-list))
    (setq sort-list-name-lines-type(append sort-list-name-lines-type
					   (list(list x
						      (cadr(assoc x file-lines-type-list)))
						))))
  (new_dialog "set_intersection_lines_type" dcl_id)
  (start_list "list-intersection-lines")
  (foreach x sort-list-name-lines-type(add_list (strcat (car x) "                  [" (cadr x)"]")))
  (end_list)
  (set_tile "list-intersection-lines" (itoa list-intersection-lines))
  (set_tile "edit-discription" (cadr(nth list-intersection-lines sort-list-name-lines-type)))
  (action_tile "list-intersection-lines"
    (vl-prin1-to-string
      '(progn
	(setq list-intersection-lines (atoi $value))
	(set_tile "edit-discription" (cadr(nth list-intersection-lines sort-list-name-lines-type)))
	)
      )
    )
  (action_tile "edit-discription"
    (vl-prin1-to-string
      '(progn
	(setq sort-list-name-lines-type
	 (replase-item-in-list-by-number (list (car(nth list-intersection-lines sort-list-name-lines-type)) $value)  list-intersection-lines sort-list-name-lines-type)
	 )
	(start_list "list-intersection-lines")
	(foreach x sort-list-name-lines-type(add_list (strcat (car x) "                  [" (cadr x)"]")))
	(end_list)
	)
      )
    )
  (action_tile "del"
    (vl-prin1-to-string
      '(progn
	(setq sort-list-name-lines-type(edit-list-del-item-by-number list-intersection-lines sort-list-name-lines-type))
	(start_list "list-intersection-lines")
	(foreach x sort-list-name-lines-type(add_list (strcat (car x) "                  [" (cadr x)"]")))
	(end_list)
	(setq list-intersection-lines (if (eq list-intersection-lines (length sort-list-name-lines-type))
				       (1- list-intersection-lines)
				       list-intersection-lines)
	 )
	(set_tile "list-intersection-lines" (itoa list-intersection-lines))
	(set_tile "edit-discription" (cadr(nth list-intersection-lines sort-list-name-lines-type)))
	)
      )
    )
  (action_tile "new"
    (vl-prin1-to-string
      '(progn
	(if (setq new-intersection-lines-type(profile-dialog-new-intersection-lines-type dcl_id sort-list-name-lines-type))
	 (progn
	  (setq new-sort-list-name-lines-type(append sort-list-name-lines-type(list new-intersection-lines-type)))
	  (setq l-sort-list-name-lines-type(acad_strlsort(mapcar 'car new-sort-list-name-lines-type)))
	  (setq sort-list-name-lines-type nil)
	  (foreach x l-sort-list-name-lines-type
	   (setq sort-list-name-lines-type(append sort-list-name-lines-type
					   (list(list x
						 (cadr(assoc x new-sort-list-name-lines-type)))
					    ))))
	  (start_list "list-intersection-lines")
	  (foreach x sort-list-name-lines-type(add_list (strcat (car x) "                  [" (cadr x)"]")))
	  (end_list)
	  (setq list-intersection-lines (analysis-number-item-in-list new-intersection-lines-type sort-list-name-lines-type))
	  (set_tile "list-intersection-lines" (itoa list-intersection-lines))
	  )
	 )
	)
      )
    )
  (if (>(start_dialog)0)
    (progn
      (foreach x sort-list-name-lines-type
	(setq result-list(append result-list (list (car x ))))
	(setq result-list(append result-list (list (cadr x ))))
	)
      (put-intersection-lines-type-list result-list)
      )
    )
  (princ)
  )