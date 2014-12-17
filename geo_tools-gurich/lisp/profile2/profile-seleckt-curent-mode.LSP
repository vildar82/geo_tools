(defun profile-seleckt-curent-mode (/ curent-profile dcl_id flag list-name-block-profile number object old-item-list profile-mode step)
  ; позволяет выбрать (указателем или из падающего списка) текущий профиль, или задать параметры старого профиля
  (setq curent-profile(vlax-ldata-get "geo_tools_dictionary" "curent-profile"))
  (setq list-name-block-profile(profile-get-list-name-and-block-profile))
  (setq old-item-list(vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))
  (setq profile-mode(vlax-ldata-get "geo_tools_dictionary" "profile-mode"))
  (setq dcl_id (geo-tools-load-dialog "d_profile_seleckt_curent_mode"))
  (if (not old-item-list)
    (setq old-item-list (list (list "vertical-scale" 100)
			      (list "horizontal-scale" 500)
			      (list "reference-level" 100.00)
			      (list "height-text" 3.5)
			      (list "accuracy" 2)
			      (list "sel-uklon" 1)
			      (list "accuracy-downgrade" 3)
			      (list "picket-first" 0.0)))
    (if
      (= (assoc "picket-first" old-item-list ) nil)
      (setq old-item-list (append old-item-list (list (list "picket-first" 0.0)))))
    )
  (setq step 2)
  (while (>= step 2)
    (new_dialog "d_profile_seleckt_curent_mode" dcl_id)
    (foreach x old-item-list (set_tile (car x)(if
						(eq(type(cadr x))'real)
						(if
						  (=(car x)"picket-first")
						  (convert-distance-to-pk-string(cadr x)2 nil)
						  (rtos(cadr x)2 2))
						(itoa(cadr x)))))
    (if (or(= profile-mode 1)(= profile-mode nil))
      (cond
	((= list-name-block-profile nil)
	 (setq profile-mode 0)
	 (set_tile "new" "0")
	 (set_tile "old" "1")
	 (mode_tile "new" 1)
	 (mode_tile "seleckt-list" 1)
	 (mode_tile "seleckt-button" 1)
	 (set_tile "error" "Не найдено блоков профилей в чертеже!")
	 )
	((eq (length list-name-block-profile) 1)
	 (setq profile-mode 1)
	 (set_tile "new" "1")
	 (set_tile "old" "0")
	 (start_list "seleckt-list")
	 (add_list (caar list-name-block-profile))
	 (end_list)
	 (foreach x old-item-list (mode_tile (car x) 1))
	 (mode_tile "seleckt-list" 1)
	 (mode_tile "seleckt-button" 1)
	 (set_tile "error" "В чертеже только один блок профиля.")
	 )
	(T
	 (setq profile-mode 1)
	 (set_tile "new" "1")
	 (set_tile "old" "0")
	 (start_list "seleckt-list")
	 (foreach x list-name-block-profile (add_list (car x)))
	 (end_list)
	 (if (setq number(analysis-number-item-in-list (car curent-profile)(mapcar 'car list-name-block-profile)))
	   (set_tile "seleckt-list" (itoa number)))
	 (foreach x old-item-list (mode_tile (car x) 1))
	 )
	)
      (progn
	(cond
	  ((equal list-name-block-profile nil)
	   (mode_tile "new" 1)
	   (set_tile "error" "Не найдено блоков профилей в чертеже!"))
	  ((eq (length list-name-block-profile) 1)
	   (start_list "seleckt-list")
	   (add_list (caar list-name-block-profile))
	   (end_list)
	   )
	  (T
	   (start_list "seleckt-list")
	   (foreach x list-name-block-profile (add_list (car x)))
	   (end_list)
	   (if curent-profile
	     (if (setq number(analysis-number-item-in-list (car curent-profile)(mapcar 'car list-name-block-profile)))
	       (set_tile "seleckt-list" (itoa number)))
	     )
	   )
	  )
	(set_tile "new" "0")
	(set_tile "old" "1")
	(mode_tile "seleckt-list" 1)
	(mode_tile "seleckt-button" 1)
	)
      )
      
    (action_tile "new"
      (vl-prin1-to-string
	'(progn
	  (set_tile "new" "1")
	  (set_tile "old" "0")
	  (setq profile-mode 1)
	  (foreach x old-item-list (mode_tile (car x) 1))
	  (if (not(eq (length list-name-block-profile) 1))
	   (progn
	    (mode_tile "seleckt-list" 0)
	    (mode_tile "seleckt-button" 0)
	    )
	   )
	  )
	)
      )
    (action_tile "old"
      (vl-prin1-to-string
	'(progn
	  (set_tile "new" "0")
	  (set_tile "old" "1")
	  (setq profile-mode 0)
	  (foreach x old-item-list (mode_tile (car x) 0))
	  (mode_tile "seleckt-list" 1)
	  (mode_tile "seleckt-button" 1)
	  )
	)
      )
    (action_tile "seleckt-button"
      (vl-prin1-to-string
	'(done_dialog 3)))
    
    (action_tile "accept"
      (vl-prin1-to-string
	'(progn
	  (cond
	   ((= profile-mode 1)
	    (setq curent-profile
	     (nth (atoi(get_tile "seleckt-list")) list-name-block-profile))
	    )
	   ((= profile-mode 0)
	    (setq temp nil)
	    (foreach x old-item-list
	     (setq temp (append temp (list
				      (list
				       (car x)
				       (if (eq (type(cadr x))'real)
					(if
					 (=(car x)"picket-first")
					 (convert-pk-string-to-distance(get_tile (car x)))
					 (atof(get_tile (car x))))
					(atoi(get_tile (car x)))
					)
				       ))))
	     )
	    (setq old-item-list temp)
	    )
	   )
	  (done_dialog 1)
	  );progn
	)
      )
    
    (setq step (start_dialog))
    (if (= step 3)
      (progn
	(setq flag T)
	(while flag
	  (setq object (car (entsel "\nУкажи блок профиля: ")))
	  (cond
	    ((eq object nil)
	     (princ "\nОшибка! Ничего не выбрано.")
	     )
	    ((and(eq (cdr(assoc 0 (entget object)))"INSERT")(vlax-ldata-get(vlax-ename->vla-object object)"profile-name"))
	     (setq curent-profile(cons (vlax-ldata-get(vlax-ename->vla-object object)"profile-name") (vlax-ename->vla-object object)))
	     (setq flag nil)
	     )
	    ((eq (cdr(assoc 0 (entget object)))"INSERT")
	     (princ "\nОшибка! Блок не содержит данных профиля.")
	     )
	    (T
	     (princ "\nОшибка! Выбран не блок профиля.")
	     )
	    )
	  );while
	);progn
      );if
    )
  (unload_dialog dcl_id)
  (if (/= step 0)
    (progn
      (vlax-ldata-put "geo_tools_dictionary" "profile-mode" profile-mode)
      (cond
	((= profile-mode 1)
	 (vlax-ldata-put "geo_tools_dictionary" "curent-profile" curent-profile)
	 )
	((= profile-mode 0)
	 (vlax-ldata-put "geo_tools_dictionary" "old-profile-item-list" old-item-list)
	 )
	)
      T
      )
    nil
    )
  )