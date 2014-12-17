(defun C:profile_add_new_line_be_profile ( /
					  3d-point-list 3dpline block-obj block-profile-obj buffer-zone
					  horizontal-scale reference-level scale-coefficient start-point
					  trace-2dpline vertical-scale
					  curent-profile profile-line-point-list
					  profile-mode temp)
  (load_global_variable)
  (geo_tools-help "profile_add_new_line_be_profile")
  (setq profile-mode (vlax-ldata-get "geo_tools_dictionary" "profile-mode"))
  (if(if (not profile-mode)(profile-seleckt-curent-mode)T)
    (if (=(vlax-ldata-get "geo_tools_dictionary" "profile-mode")1)
      (progn
	(setq curent-profile(vlax-ldata-get "geo_tools_dictionary" "curent-profile"))
	(if(profile-check-curent-profile curent-profile)
	  (progn
	    (princ (strcat"\nТекущий профиль: "(car curent-profile)))
	    (setq 3Dpline (car (entsel "\nВыберите 3D полилинию или поверхность: ")))
	    (cond
	      ((eq 3Dpline nil)
	       (alert "Ничего не выбрано!"))
	      ((not(or (eq (cdr(assoc 0 (entget 3Dpline))) "POLYLINE")(eq (cdr(assoc 0 (entget 3Dpline))) "AECC_TIN_SURFACE")))
	       (alert "Выбран не допустимый объект!"))
	      ((or (eq (cdr(assoc 0 (entget 3Dpline))) "POLYLINE")(eq (cdr(assoc 0 (entget 3Dpline))) "AECC_TIN_SURFACE"))
	       (setq 3Dpline (vlax-ename->vla-object 3Dpline))
	       (setq block-profile-obj (cdr curent-profile))
	       (setq trace-2Dpline (trace-create-2Dpline-by-block-profile block-profile-obj))
	       (setq block-obj (cdr(assoc "block"(vlax-ldata-get block-profile-obj"obj-list"))))
	       (setq vertical-scale (vlax-ldata-get block-profile-obj"vertical-scale"))
	       (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
	       (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
	       (setq buffer-zone(*(atof(nth 3 (vlax-ldata-get block-profile-obj"profile-parameters")))scale-coefficient))
	       (setq start-point(mapcar'+ (vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj)))(list buffer-zone 0)))
	       
	       (setq 3d-point-list
		      (vl-remove-if
			(function (lambda (x) (if (eq (caddr x)0)T nil)))
			(if(eq (vla-get-ObjectName 3Dpline) "AcDb3dPolyline")
			  (analisis-3d-and-2d-poly-by-general-3dpoly trace-2Dpline 3Dpline)
			  (analisis-surfese-and-2d-poly 3Dpline trace-2Dpline)
			  )
			)
		     )
	       
;| раньше необходимость строить линейку определялось по присутствии в obj-list раздела ruler-list
теперь для этого есть флаг ruler. Здесь предпологается использование предидущего варианта старого профиля
удаляется (если есть) ruler-list и вместо него ставится ruler=1 |;

	       (if (/= (cdr(assoc "ruler-list"(vlax-ldata-get block-profile-obj"obj-list")))nil)
		 (progn
		   (vlax-ldata-put block-profile-obj"ruler" 1)
		   (if(setq number (analysis-number-item-in-list
				     (assoc "ruler-list"(vlax-ldata-get block-profile-obj"obj-list"))
				     (vlax-ldata-get block-profile-obj"obj-list")))
		     (vlax-ldata-put block-profile-obj"obj-list"
		       (edit-list-del-item-by-number number(vlax-ldata-get block-profile-obj"obj-list")))
		     )
		   )
		 )
	       
	       (if (/=(vlax-ldata-get block-profile-obj"ruler")1)
		 (progn
		   (profile-draw-ruler-in-basement block-profile-obj 3d-point-list)
		   (vlax-ldata-put block-profile-obj"ruler" 1)
		   )
		 )
	       
	       (setq reference-level (vlax-ldata-get block-profile-obj"reference-level"))
	       (foreach x 3d-point-list
		 (setq profile-line-point-list
			(append profile-line-point-list
				(list (mapcar '+ start-point
					      (list
						(vlax-curve-getDistAtPoint trace-2Dpline (list (car x)(cadr x)))
						(/(- (nth 2 x) reference-level)(/ (* vertical-scale (/ 1000.0 horizontal-scale)) 1000.0))
						)
					      )))))
	       (setq temp(vla-addLightWeightPolyline model_spece
			   (convert-list-points-to-lwpolyline-safearray profile-line-point-list)))
	       (vla-put-Linetype temp "Continuous")
	       (if (eq (vla-get-Color 3Dpline)256)
		 (vla-put-Color temp (vla-get-Color(vla-Item (vla-get-Layers(vla-get-ActiveDocument(vlax-get-acad-object)))(vla-get-Layer 3Dpline))))
		 (vla-put-Color temp (vla-get-Color 3Dpline))
		 )
	       (vla-erase trace-2Dpline)
	       )
	      )
	    )
	  )
	)
      (alert "Данная команда не работает в режиме \"старый профиль\"!")
      )
    )
  (princ)
  )