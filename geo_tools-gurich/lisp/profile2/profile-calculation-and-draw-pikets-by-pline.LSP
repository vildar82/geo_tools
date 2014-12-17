(defun profile-calculation-and-draw-pikets-by-pline (block-profile-obj 2Dpline list-select-string /
						     accuracy curent-text-style
						     up-y-coor-in-curent-string
						     down-y-coor-in-curent-string
						     height-text horizontal-scale list-picket
						     list-vertex number-string point profile-parameters profile-string-list
						     reference-level scale-coefficient start-point vertical-scale picket-first flag
						     buffer-zone
						     )
  ;чертит пикеты на профиле
  (if(=(vlax-ldata-get "geo_tools_dictionary" "profile-mode")1)
    (progn
      (setq up-y-coor-in-curent-string (nth 1 list-select-string))
      (setq down-y-coor-in-curent-string(nth 2 list-select-string))
      (setq number-string(nth 0 list-select-string))
      (setq profile-string-list(vlax-ldata-get block-profile-obj"profile-string-list"))
      (setq list-select-string(nth number-string profile-string-list))
      (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
      (setq vertical-scale (vlax-ldata-get block-profile-obj"vertical-scale"))
      (setq reference-level (vlax-ldata-get block-profile-obj"reference-level"))
      (setq profile-parameters(vlax-ldata-get block-profile-obj"profile-parameters"))
      (setq picket-first(vlax-ldata-get block-profile-obj"picket-first"))
      (setq buffer-zone(*(atof(nth 3 (vlax-ldata-get block-profile-obj"profile-parameters")))(/ 1.0 (/ 1000.0 horizontal-scale))))
      (setq start-point(mapcar'+ (vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj)))(list buffer-zone 0)))
      (setq curent-text-style (nth 1 profile-parameters))
      (if(or(eq curent-text-style "***Текущий***")(not(member curent-text-style(get-text-style-list))))
	(setq curent-text-style(getvar "TEXTSTYLE")))
      (setq height-text (*(atof(nth 6 list-select-string))(/ 1.0 (/ 1000.0 horizontal-scale))))
      (setq accuracy (atoi(nth 4 list-select-string)))
      (setq flag T)
      )
    (progn
      (if(setq start-point(getpoint"\nУкажи точку начального пикета на условном горизонте: "))
	(if(setq down-y-coor-in-curent-string(cadr(getpoint"\nУкажи точку на нижней линии строки: ")))
	  (if(setq up-y-coor-in-curent-string (cadr(getpoint "\nУкажи точку на верхней линии строки: ")))
	    (progn
	      (setq horizontal-scale (cadr(assoc "horizontal-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	      (setq vertical-scale (cadr(assoc "vertical-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	      (setq reference-level (cadr(assoc "reference-level" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	      (setq picket-first (cadr(assoc "picket-first" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	      (setq curent-text-style(getvar "TEXTSTYLE"))
	      (setq height-text (*
				  (cadr(assoc "height-text" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list")))
				  (/ 1.0 (/ 1000.0 horizontal-scale))))
	      (setq accuracy (cadr(assoc "accuracy" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	      (setq flag T)
	      )
	    )))
      )
    )
  (if flag
    (progn
      (setq scale-coefficient (/ (* vertical-scale (/ 1000.0 horizontal-scale))1000.00))
      (setq list-vertex (extract_coord_lwpolyline (entget 2Dpline)))
      (repeat (length list-vertex)
	(setq point (car list-vertex))
	(setq list-vertex (cdr list-vertex))
	(setq list-picket(append list-picket(list (list (convert-distance-to-pk-string(+(-(nth 0 point) (nth 0 start-point))picket-first)accuracy nil) point))))
	)
      
      (profile-draw-texts-by-list-limits
	list-picket
	(profile-analisis-limits (apply 'list (mapcar 'cadr list-picket))horizontal-scale)
	down-y-coor-in-curent-string
	up-y-coor-in-curent-string
	height-text
	accuracy
	horizontal-scale
	vertical-scale
	curent-text-style)
      ))
  )