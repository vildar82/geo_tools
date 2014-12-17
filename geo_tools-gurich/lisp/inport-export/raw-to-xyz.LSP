(defun C:proba--- (/ structure-list point-obj)
  (setq structure-list(convert-raw-file-to-structure "d:\\temp\\test_raw.txt"))
  (setq structure-list(calculation-coor-of-raw structure-list))
  (vla-StartUndoMark active_document)
  (foreach x (get-item-by-name-in-list structure-list "catalog-coord")
    (create-or-sel-layer (get-item-by-name-in-list x "data"))
    (setq point-obj (vla-add C3D_points
			       (vlax-3d-point
				 (get-item-by-name-in-list x "E")
				 (get-item-by-name-in-list x "N")
				 (get-item-by-name-in-list x "Z"))))
    (setq flag (if (/= (get-item-by-name-in-list x "code")"")T nil))
    (vlax-put-property point-obj 'Description (strcat
						(get-item-by-name-in-list x "code")
						(if flag "[" "")
						(get-item-by-name-in-list x "name")
						(if flag "[" "")
						))
    (foreach y (list
		 "name"
		 "genesis"
		 "h-target"
;		 "temp"
;		 "press"
		 "prism"
		 "data"
		 "time"
		 "code"
		 )
      (if (get-item-by-name-in-list x y)
      (vlax-ldata-put point-obj y (get-item-by-name-in-list x y)))
      )
    )
  (vla-EndUndoMark active_document)
  (princ)
  )