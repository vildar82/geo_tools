(defun C:profile_dimension_excess (/ t1 t2 h flag block-profile-obj curent-profile horizontal-scale vertical-scale profile-mode reference-level)
  ; ��������� ���������� ����� ����� ��������� ������� �� �������
  (geo_tools-help "profile_dimension_excess")
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
	(progn
	  (setq flag nil)
	  (while(not flag)
	    (initget)(setq t1(getpoint "\n����� ����� #1 �� �������: "))
	    (redraw)
	    (if t1
	      (progn
		(draw_temp_X t1 1)
		(initget)(setq t2(getpoint t1 "\n����� ����� #2 �� �������: "))
		(draw_temp_X t2 1)
		(setq h (rtos (* (-(nth 1 t2)(nth 1 t1)) (/ (* vertical-scale (/ 1000.0 horizontal-scale)) 1000.0)) 2 3))
		(princ (strcat "\n���������� " h "�"))
		)
	      (setq flag T)
	      )
	    )
	  )
	)
      )
    )
  (princ)
  )