(defun C:profile_draw_marker_be_elevation_to_profile (/ block-profile-obj curent-profile elivation
						      flag horizontal-scale profile-mode reference-level
						      reference-level-point start-point t2 t3 t4 vertical-scale)
  ; ������ ������  �� �������� ������� �� �������
  (geo_tools-help "profile_draw_marker_be_elevation_to_profile")
  (setq profile-mode (vlax-ldata-get "geo_tools_dictionary" "profile-mode"))
  (if(if(not profile-mode)(profile-seleckt-curent-mode)T)
    (progn
      (if(= profile-mode 1)
	(progn
	  (setq curent-profile(vlax-ldata-get "geo_tools_dictionary" "curent-profile"))
	  (setq flag (profile-check-curent-profile curent-profile))
	  (if flag
	    (progn
	      (princ(strcat"\n������� �������: "(car curent-profile)))
	      (setq block-profile-obj (cdr curent-profile))
	      (setq vertical-scale (vlax-ldata-get block-profile-obj"vertical-scale"))
	      (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
	      (setq reference-level (vlax-ldata-get block-profile-obj"reference-level"))
	      (setq start-point(vlax-safearray->list(vlax-variant-value(vla-get-InsertionPoint block-profile-obj))))
	      ))
	  )
	(progn
	  (setq horizontal-scale (cadr(assoc "horizontal-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	  (setq vertical-scale (cadr(assoc "vertical-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	  (setq reference-level (cadr(assoc "reference-level" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	  (princ(strcat
		  "\n�������� 1:" (itoa horizontal-scale)
		  " �������� 1:" (itoa vertical-scale)
		  " ������� ��������� ���������: " (rtos reference-level 2 2) "�"
		  ))
	  (setq flag T))
	)
      (if
	flag
	(while
	    (setq elivation (get_easy_elivation))
	    (setq reference-level-point (profile-get-last-select-point))
	    (if(= profile-mode 1)(setq reference-level-point(list(nth 0 reference-level-point)(nth 1 start-point)0.0)))
	    (setq t2 (mapcar '+ reference-level-point
			     (list 0 (/ (- elivation reference-level) (/ (* vertical-scale (/ 1000.0 horizontal-scale)) 1000.0)))))
	    (setq t3 (mapcar '+ t2 (list -1.25 -2.165)))
	    (setq t4 (mapcar '+ t2 (list 1.25 -2.165)))
	    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")
			   '(90 . 3) '(70 . 1) (cons 43 0)
			   (cons 10 t3) (cons 10 t2) (cons 10 t4)))
	    (setq elivation nil)
	    )
	)
      )
    )
  (princ)
  )