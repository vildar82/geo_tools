(defun C:geo_tools_options (/ dcl_id list-fild toggle-list)
    (setq dcl_id (geo-tools-load-dialog "geo_tools_options"))
  (new_dialog "geo_tools_options" dcl_id)
  (setq list-fild (get-geo_tools_options-list))
  (foreach x list-fild
    (set_tile (car x) (cadr x))
    )
  (action_tile "accept"
    (vl-prin1-to-string
      '(progn
	(foreach x list-fild
	 (setq toggle-list (append toggle-list (list(list (car x) (get_tile (car x))))))
	 )
	(done_dialog 1)
	)
      )
    )
  (if (>(start_dialog)0)
    (foreach x toggle-list
      (file-dat-xml-write (car x) (cadr x))
      )
    )
  (unload_dialog dcl_id)
  (princ)
  )



