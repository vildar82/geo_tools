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
	      (if (setq elevation (get-elivation-be-plan))
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

(defun get-elivation-be-plan (/ flag obj z temp point d marker)
  ;������ ������� � ��������
  (load_global_variable)
  (setq flag T)
  (if (=(vlax-ldata-get "geo_tools_dictionary" "profile-marker-type")nil)
    (vlax-ldata-put "geo_tools_dictionary" "profile-marker-type" "�������������")
    )
  (setq marker (vlax-ldata-get "geo_tools_dictionary" "profile-marker-type"))
  (while flag
    (initget "������ ��������� �����")
    (setq temp(entsel "\n������ �����,����� ��� ������������� � ��������[������/���������/�����]<������>: "))
    (if (eq(type temp)'list)
      (setq obj (car temp))
      (setq obj temp))
    (cond
      ((eq obj "���������")
       (setq z nil)
       (initget "����������� ������������� �����")
       (if
	 (setq marker(getkword (strcat"\n������ ������� �� �������[�����������/�������������/�����]<" (vlax-ldata-get "geo_tools_dictionary" "profile-marker-type") ">: ")))
	 (vlax-ldata-put "geo_tools_dictionary" "profile-marker-type" marker))
       (setq flag T)
       )
      ((eq obj "�����")
       (setq z nil)
       (setq flag nil)
       )
      ((or(eq obj nil)(eq obj "������"))
       (setq z (getreal"\n����� �������<0.00>: "))
       (if (eq z nil) (setq z 0.0))
       (setq point (getpoint "\n����� ����� �� �����: "))
       (if (= marker "�������������")
	 (setq d(getstring t "\n����� ��������: "))
	 (setq d""))
       (if (not d)(setq d""))
       (setq z (list point (list (list z (if d d "")))))
       (setq flag nil)
       )
      ((eq (cdr(assoc 0 (entget obj))) "TEXT")
       (setq point (getpoint "\n����� ����� �� �����: "))
       (setq z(atof(edit_text_clean_for_number(cdr(assoc 1(entget obj))))))
       (setq z (list point (list (list z (cdr(assoc 1(entget obj)))))))
       (setq flag nil)
       )
      ((eq (cdr(assoc 0 (entget obj))) "AECC_COGO_POINT")
       (setq point (vlax-safearray->list(vlax-variant-value(vlax-get-property  (vlax-ename->vla-object obj) "Location"))))
       (setq z(vlax-get-property  (vlax-ename->vla-object obj) "ELEVATION"))
       (setq z (list point (list(list z (vlax-get-property  (vlax-ename->vla-object obj) "RawDescription")))))
       (setq flag nil)
       )
      ((eq (cdr(assoc 0 (entget obj))) "MULTILEADER")
       (setq point (get-sel-point-multileader obj))
       (setq z (list point (get-string-multileader obj)))
       (setq flag nil)
       )
      (T
       (alert "������! ������ �� ������ �� ����������� ����. ����������� ������������ ����� � ����� COGO. �������� �����.")
       )
      )
    )
  z
  )

(defun get-sel-point-multileader (object / points)
  (if (eq (type object) 'ename)
    (setq object (vlax-ename->vla-object object)))
  (if (> (vla-get-LeaderCount object) 0)
    (progn
    (setq points (vlax-safearray->list(vlax-variant-value(vla-GetLeaderLineVertices object 0))))
    (list (nth 0 points)(nth 1 points)(nth 2 points))
    )
    (getpoint "\n����� ����� �� �����: ")
    )
  )

(defun get-string-multileader (object / points list-elevation pos)
  (if (eq (type object) 'ename)
    (setq object (vlax-ename->vla-object object)))
  (foreach x (convert-string-to-list-be-delimiter (vla-get-TextString object) "\\P")
    (if (and (>(ascii(substr x 1 1))47)(<(ascii(substr x 1 1))58))
      (progn
	(setq pos(vl-string-position (ascii " ") x))
	(setq elevation (atof(substr x 1 pos)))
	(if pos
	  (setq Description (substr x (+ pos 2)))
	  (setq Description x))
	(setq list-elevation (append list-elevation (list(list elevation Description))))
	)
      )
    )
  list-elevation
  )

(defun convert-list-elevation2point-list (input-list / result)
  (foreach x input-list
    (setq result (append result (list (list 0.0 0.0 (car x)))))
    )
  result)