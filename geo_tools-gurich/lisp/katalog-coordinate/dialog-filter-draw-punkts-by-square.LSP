(defun dialog-filter-draw-punkts-by-square (o-doc current-filter-list / dcl_id)
  (setq dcl_id (geo-tools-load-dialog "dialog_filter_draw_punkts"))
  (new_dialog "dialog_filter_draw_punkts" dcl_id)
  (foreach x current-filter-list
    (if (= "list-tipe-punkts"(car x))
      (draw-list-box-tipe-punkts (cadr x))
      (set_tile (car x)(itoa (cadr x)))
      )
    )
  (mode-tile-filter-mode-dialog-filter-draw-punkts-by-square current-filter-list)
  
  (action_tile "filter-mode"
    (vl-prin1-to-string
      '(progn
	(setq current-filter-list (put-item-by-name-in-list current-filter-list "filter-mode" (atoi $value)))
	(mode-tile-filter-mode-dialog-filter-draw-punkts-by-square current-filter-list)
	)))
  (action_tile "no-z"
    (vl-prin1-to-string'(setq current-filter-list
			 (put-item-by-name-in-list current-filter-list "no-z" (atoi $value)))))
  (action_tile "yes-z"
    (vl-prin1-to-string'(setq current-filter-list
			 (put-item-by-name-in-list current-filter-list "yes-z" (atoi $value)))))
  (action_tile "no-year"
    (vl-prin1-to-string'(setq current-filter-list
			 (put-item-by-name-in-list current-filter-list "no-year" (atoi $value)))))
  (action_tile "years-limit"
    (vl-prin1-to-string
      '(progn
	(setq current-filter-list (put-item-by-name-in-list current-filter-list "years-limit" (atoi $value)))
	(mode-tile-filter-mode-dialog-filter-draw-punkts-by-square current-filter-list)
	)))
  (action_tile "year-min"
    (vl-prin1-to-string'(setq current-filter-list
			 (put-item-by-name-in-list current-filter-list "year-min" (atoi $value)))))
  (action_tile "year-max"
    (vl-prin1-to-string'(setq current-filter-list
			 (put-item-by-name-in-list current-filter-list "year-max" (atoi $value)))))
  (action_tile "tipe-filter"
    (vl-prin1-to-string
      '(progn
	(setq current-filter-list (put-item-by-name-in-list current-filter-list "tipe-filter" (atoi $value)))
	(mode-tile-filter-mode-dialog-filter-draw-punkts-by-square current-filter-list)
	)))
  (action_tile "list-tipe-punkts"
    (vl-prin1-to-string
      '(progn
	(setq current-str-tipe-list (atoi $value))
	(set_tile "on-off-punkt" (itoa (cadr(nth current-str-tipe-list(get-item-by-name-in-list current-filter-list "list-tipe-punkts")))))
	)))
  (action_tile "on-off-punkt"
    (vl-prin1-to-string
      '(progn
	(setq current-filter-list
	 (put-item-by-name-in-list current-filter-list "list-tipe-punkts"
	  (edit_replase_in_list_eliment_be_number
	   (list (car(nth current-str-tipe-list(get-item-by-name-in-list current-filter-list "list-tipe-punkts"))) (atoi $value))
	   (1+ current-str-tipe-list)
	   (get-item-by-name-in-list current-filter-list "list-tipe-punkts")
	   )
	  ))
	(draw-list-box-tipe-punkts (get-item-by-name-in-list current-filter-list "list-tipe-punkts"))
	(set_tile "list-tipe-punkts" (itoa current-str-tipe-list))
	)))
  (if(eq(start_dialog)0)
    (setq current-filter-list nil)
    (set-filter-katalog-punkts current-filter-list)
    )
  (unload_dialog dcl_id)
  current-filter-list
  )

(defun draw-list-box-tipe-punkts (list-tipe-punkts / result-list)
  (foreach x list-tipe-punkts
    (setq result-list (append result-list (list (strcat (if (= (cadr x) 1) "Выкл. | " "Испол. | ")(car x)))))
    )
  (start_list "list-tipe-punkts")
  (foreach y result-list (add_list y))
  (end_list)
  )

(defun get-list-tipe-punkts-in-katalog (o-doc / types type-puncts list-tipe-punkts)
  (setq types(XML-Get-Child (XML-Get-Child o-doc nil "properties") nil "type-puncts"))
  (foreach x (setq type-puncts(XML-Get-ChildList types))
    (setq list-tipe-punkts (append list-tipe-punkts (list (XML-Get-Attribute x "name" ""))))
    )
  list-tipe-punkts
  )

(defun get-current-filter-list  (filter-list list-tipe-punkts-in-katalog / CURRENT-FILTER-LIST LIST-TIPE-PUNKTS TEMP-LIST)
  (foreach x filter-list
    (if (= "list-tipe-punkts"(car x))
      (progn
	(setq list-tipe-punkts (cadr x))
	(setq car-list-tipe-punkts (mapcar 'car list-tipe-punkts))
	(foreach y list-tipe-punkts-in-katalog
	  (if (member y car-list-tipe-punkts)
	    (setq temp-list (append temp-list (list (assoc y list-tipe-punkts))))
	    (setq temp-list (append temp-list (list (list y 0))))
	    )
	  )
	(setq temp-list (append (list (assoc "не определен" list-tipe-punkts)) temp-list))
	(setq current-filter-list (append current-filter-list (list (list "list-tipe-punkts" temp-list))))
	)
      (setq current-filter-list (append current-filter-list (list x)))
      )
    )
  current-filter-list
  )


(defun mode-tile-filter-mode-dialog-filter-draw-punkts-by-square ( current-filter-list / mode)
  (setq mode (get-item-by-name-in-list current-filter-list "filter-mode"))
  (foreach x current-filter-list
    (if (/= "filter-mode" (car x))
      (mode_tile (car x) mode)
      )
    )
  (if (/= mode 1)
    (progn
      (if (= (get-item-by-name-in-list current-filter-list "years-limit")0)
	(setq mode 1)
	(setq mode 0)
	)
      (foreach x (list "year-min" "year-max")
	(mode_tile x mode)
	)
      (if (= (get-item-by-name-in-list current-filter-list "tipe-filter")0)
	(setq mode 1)
	(setq mode 0)
	)
      (foreach x (list "list-tipe-punkts" "on-off-punkt")
	(mode_tile x mode)
	)
      )
    )
  )