(defun get-list-dialog-design-trace ( / list-dialog)
  (setq list-dialog(vlax-ldata-get "geo_tools_dictionary" "dialog-design-trace"))
  (if (<(get-item-by-name-in-list list-dialog "version") 3.0)
    (setq list-dialog (list
			(list "version" 3.0)
			
			(list "draw-picket" 1)
			(list "accuracy-picket" 2)
			(list "draw-start-end-picket" 1)
			
			(list "draw-mleader" 1)
			(list "accuracy-angle" 2)
			(list "perfix-angle" "со ")
			(list "accuracy-picket-angle" 2)
			(list "draw-start-end-picket-angle" 0)
			(list "min-angle" 3.0)
			
			(list "draw-coor" 1)
			(list "numbers-coor" 0)
			(list "accuracy-coor" 2)
			
			(list "picket-first" 0.0)
			(list "width-poly" 0.3)
			(list "del-bak-2poly" 1)
			)
	  )
    )
  list-dialog
  )