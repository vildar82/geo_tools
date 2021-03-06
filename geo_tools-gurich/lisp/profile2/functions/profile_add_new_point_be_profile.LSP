(defun C:profile_add_new_point_be_profile ( /
					   block-obj block-profile-obj buffer-zone curent-profile
					   elevation horizontal-scale insert-point
					   lightweightpolyline point reference-level
					   scale-coefficient start-point trace-2dpline vertical-scale profile-mode plan-point)
  (load_global_variable)
  (geo_tools-help "profile_add_new_point_be_profile")
  (setq profile-mode (vlax-ldata-get "geo_tools_dictionary" "profile-mode"))
  (if(if (not profile-mode)(profile-seleckt-curent-mode)T)
    (if (=(vlax-ldata-get "geo_tools_dictionary" "profile-mode")1)
      (progn
	(setq curent-profile(vlax-ldata-get "geo_tools_dictionary" "curent-profile"))
	(if curent-profile
	  (if (vlax-erased-p (cdr curent-profile))
	    (alert "������! ������� ������� ��� ������.\n������� ������ ������� ������� ����� � ������� ������� \"������� ������� �������\"")
	    (progn
	      
	      (princ (strcat"\n������� �������: "(car curent-profile)))
	      (while (setq elevation (get-elivation-be-plan))
		(if (setq point (car elevation))
		(progn
		  (setq block-profile-obj (cdr curent-profile))
		  (setq trace-2Dpline (trace-create-2Dpline-by-block-profile block-profile-obj))
		  (setq block-obj (cdr(assoc "block"(vlax-ldata-get block-profile-obj"obj-list"))))
		  (setq vertical-scale (vlax-ldata-get block-profile-obj"vertical-scale"))
		  (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
		  (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
		  (setq height-text (*(atof(nth 9 (vlax-ldata-get block-profile-obj"profile-parameters")))scale-coefficient))
		  (setq reference-level (vlax-ldata-get block-profile-obj"reference-level"))
		  (setq buffer-zone(*(atof(nth 3 (vlax-ldata-get block-profile-obj"profile-parameters")))scale-coefficient))
		  (setq start-point(mapcar'+ (vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj)))(list buffer-zone 0)))
		  (setq plan-point (vlax-curve-getDistAtPoint trace-2Dpline (vlax-curve-getClosestPointTo trace-2Dpline (list (car point)(cadr point)0))))
		  (vla-erase trace-2Dpline)
;| ������ ������������� ������� ������� ������������ �� ����������� � obj-list ������� ruler-list
������ ��� ����� ���� ���� ruler. ����� �������������� ������������� ����������� �������� ������� �������
��������� (���� ����) ruler-list � ������ ���� �������� ruler=1 |;
		  
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
		  (vla-StartUndoMark active_document)
		  (if (/=(vlax-ldata-get block-profile-obj"ruler")1)
		    (progn
		      (profile-draw-ruler-in-basement block-profile-obj (convert-list-elevation2point-list(cadr elevation)))
		      (vlax-ldata-put block-profile-obj"ruler" 1)
		      )
		    )
		  (setq reference-level (vlax-ldata-get block-profile-obj"reference-level"))
		  
		  (if (=(vlax-ldata-get "geo_tools_dictionary" "profile-marker-type")nil)
		    (vlax-ldata-put "geo_tools_dictionary" "profile-marker-type" "�������������"))
		  (foreach x (cadr elevation)
		    (setq insert-point
			   (mapcar '+ start-point
				   (list
				     plan-point
				     (/(- (car x) reference-level)(/ (* vertical-scale (/ 1000.0 horizontal-scale)) 1000.0))
				     )))
		    (cond
		      ((=(vlax-ldata-get "geo_tools_dictionary" "profile-marker-type")"�����������")
		       (setq LightWeightPolyline (vla-addLightWeightPolyline model_spece
						   (convert-list-points-to-lwpolyline-safearray (list (mapcar '+ insert-point (list (* -1.25 scale-coefficient) (* -2.165 scale-coefficient)))
												      insert-point
												      (mapcar '+ insert-point (list (* 1.25 scale-coefficient) (* -2.165 scale-coefficient)))
												      ))))
		       (vla-put-Linetype LightWeightPolyline "Continuous")
		       (vla-put-Closed LightWeightPolyline -1)
		       )
		      ((=(vlax-ldata-get "geo_tools_dictionary" "profile-marker-type")"�������������")
		       (draw-mleader-vla-by-list-property
			    (list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
				 (cons "LeaderLineCoor" (list insert-point (mapcar '+ insert-point (list (/ 2.0 scale-coefficient) (/ 2.0 scale-coefficient)0))))
				 (cons "StyleName" "�������")
				 (cons "TextHeight" height-text)
				 (cons "TextString" (cadr x))
				 (cons "ArrowheadType" 1)
				 (cons "ArrowheadSize" (/ 1.0 scale-coefficient))
				 (cons "ArrowheadBlock" "_ClosedBlank")
				 ))
		       )
		      ((=(vlax-ldata-get "geo_tools_dictionary" "profile-marker-type")"�����")
		       (entmake (list '(0 . "POINT") (cons 10 insert-point)))
		       )
		      )
		    
		    )
		  (vla-EndUndoMark active_document)
		  )
		  )
		
		)
	      )
	    )
	  (alert "������! ������� ������� �� ���������.\n������� ������� ������� ����� � ������� ������� \"������� ������� �������\"")
	  )
	)
      (alert "������ ������� �� �������� � ������ \"������ �������\"!")
      )
    )
  (princ)
  )