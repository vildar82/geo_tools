(defun profile-editor-view-basement ( number-curent-profile / dcl_id x sort-list list-text-style
				     sort-list-standart-str
				     flag_ruler
				     sel-list-podval-str-item
				     sel-standart-item
				     new-profile-name
				     edit-list-profile-str
				     number-text-style-in-list-style
				     edit-list-standart-str)
  ; Редактор вида подвала профиля
  (setq sort-list (profile-get-sorting-list-profile-dat))
  (setq list-text-style (append (get-text-style-list) (list "***Текущий***")))
  (setq sel-standart-item 0)
  (setq sel-list-podval-str-item 0)
  (setq flag_ruler(atoi(caadr(nth number-curent-profile sort-list))))
  (setq number-text-style-in-list-style
	 (if (member(cadadr(nth number-curent-profile sort-list))list-text-style)
	   (-(length list-text-style)(length(member (cadadr(nth number-curent-profile sort-list)) list-text-style)))
	   (1-(length list-text-style))
	   )
	)
  
  (setq sort-list-standart-str(profile-get-standart-list-str-dat))
  (setq dcl_id (geo-tools-load-dialog "d_profile_view"))
  (new_dialog "d_profile_view" dcl_id)
  
  (start_list "list-standart-str")
  (foreach x sort-list-standart-str (add_list (car x)))
  (end_list)
  
  (start_list "text-style")
  (mapcar 'add_list list-text-style)
  (end_list)
  
  (start_list "profile-name")
  (foreach x sort-list (add_list (car x)))
  (end_list)
  
  (set_tile "profile-name" (itoa number-curent-profile))
  
  (start_list "list-podval-str")
  (foreach x (cddr(nth number-curent-profile sort-list)) (add_list (car x)))
  (end_list)
  
  
  (set_tile "list-standart-str" (itoa sel-standart-item))
  
  (set_tile "list-podval-str" (itoa sel-list-podval-str-item))
  (set_tile "h-txt" (rtos (atof(nth 2(cadr(nth number-curent-profile sort-list))))2 1))
  
  (set_tile "buffer-dist" (nth 3(cadr(nth number-curent-profile sort-list))))
  (set_tile "d-basement" (nth 4(cadr(nth number-curent-profile sort-list))))
  (set_tile "str1" (nth 5(cadr(nth number-curent-profile sort-list))))
  (set_tile "str2" (nth 6(cadr(nth number-curent-profile sort-list))))
  (set_tile "str3" (nth 7(cadr(nth number-curent-profile sort-list))))
  (set_tile "str4" (nth 8(cadr(nth number-curent-profile sort-list))))
  (set_tile "h-txt-intersect" (rtos (atof(nth 9(cadr(nth number-curent-profile sort-list))))2 1))
  (set_tile "accuracy-mleader" (nth 10(cadr(nth number-curent-profile sort-list))))
  (mode_tile "up" 1)
  (mode_tile "down" 0)
  (set_tile "flag_ruler" (itoa flag_ruler))
  
  (set_tile "text-style" (itoa number-text-style-in-list-style))
  
  (action_tile "profile-name"
    (vl-prin1-to-string
      '(progn
	(start_list "list-podval-str")
	(setq number-curent-profile(atoi $value))
	(foreach x (cddr(nth number-curent-profile sort-list)) (add_list (car x)))
	(end_list)
	(setq flag_ruler (atoi(caadr(nth number-curent-profile sort-list))))
	(set_tile "flag_ruler" (itoa flag_ruler))
	(setq number-text-style-in-list-style
	 (if (member(cadadr(nth number-curent-profile sort-list))list-text-style)
	   (-(length list-text-style)(length(member (cadadr(nth number-curent-profile sort-list)) list-text-style)))
	   (1-(length list-text-style))
	   )
	)
	(set_tile "text-style" (itoa number-text-style-in-list-style))
	(set_tile "list-standart-str" (itoa 0))
	(set_tile "list-podval-str" (itoa 0))
	(setq sel-standart-item 0)
	(setq sel-list-podval-str-item 0)
	(set_tile "h-txt" (rtos (atof(nth 2(cadr(nth number-curent-profile sort-list)))) 2 1))
	(set_tile "buffer-dist" (nth 3(cadr(nth number-curent-profile sort-list))))
	(set_tile "d-basement" (nth 4(cadr(nth number-curent-profile sort-list))))
	(set_tile "str1" (nth 5(cadr(nth number-curent-profile sort-list))))
	(set_tile "str2" (nth 6(cadr(nth number-curent-profile sort-list))))
	(set_tile "str3" (nth 7(cadr(nth number-curent-profile sort-list))))
	(set_tile "str4" (nth 8(cadr(nth number-curent-profile sort-list))))
	(set_tile "h-txt-intersect" (rtos (atof(nth 9(cadr(nth number-curent-profile sort-list))))2 1))
	(set_tile "accuracy-mleader" (nth 10(cadr(nth number-curent-profile sort-list))))
	(mode_tile "up" 1)
	(mode_tile "down" 0)
	)
      )
    )
  
  (action_tile "list-standart-str"
    (vl-prin1-to-string
      '(progn
	(setq sel-standart-item (atoi $value))
	)
      )
    )
  
  (action_tile "list-podval-str"
    (vl-prin1-to-string
      '(progn
	(setq sel-list-podval-str-item (atoi $value))
	(if (eq sel-list-podval-str-item 0)(mode_tile "up" 1)(mode_tile "up" 0))
	(if (eq sel-list-podval-str-item (1-(length(cddr(nth number-curent-profile sort-list)))))(mode_tile "down" 1)(mode_tile "down" 0))
	)
      )
    )
  
  (action_tile "up"
    (vl-prin1-to-string
      '(progn
	(setq sort-list (replase-item-in-list-by-number
			 (append(list (car (nth number-curent-profile sort-list))(cadr (nth number-curent-profile sort-list)))
			  (edit-list-move-item-by-number sel-list-podval-str-item(cddr(nth number-curent-profile sort-list))T))
			 number-curent-profile
			 sort-list))
	(start_list "list-podval-str")
	(foreach x (cddr(nth number-curent-profile sort-list)) (add_list (car x)))
	(end_list)
	(setq sel-list-podval-str-item (1- sel-list-podval-str-item))
	(set_tile "list-podval-str" (itoa sel-list-podval-str-item))
	(if (eq sel-list-podval-str-item 0)(mode_tile "up" 1)(mode_tile "up" 0))
	(if (eq sel-list-podval-str-item (1-(length(cddr(nth number-curent-profile sort-list)))))(mode_tile "down" 1)(mode_tile "down" 0))
	)
      )
    )
  
  (action_tile "down"
    (vl-prin1-to-string
      '(progn
	(setq sort-list (replase-item-in-list-by-number
			 (append(list (car (nth number-curent-profile sort-list))(cadr (nth number-curent-profile sort-list)))
			  (edit-list-move-item-by-number sel-list-podval-str-item(cddr(nth number-curent-profile sort-list))nil))
			 number-curent-profile
			 sort-list))
	(start_list "list-podval-str")
	(foreach x (cddr(nth number-curent-profile sort-list)) (add_list (car x)))
	(end_list)
	(setq sel-list-podval-str-item (1+ sel-list-podval-str-item))
	(set_tile "list-podval-str" (itoa sel-list-podval-str-item))
	(if (eq sel-list-podval-str-item 0)(mode_tile "up" 1)(mode_tile "up" 0))
	(if (eq sel-list-podval-str-item (1-(length(cddr(nth number-curent-profile sort-list)))))(mode_tile "down" 1)(mode_tile "down" 0))
	)
      )
    )
  
  (action_tile "add"
    (vl-prin1-to-string
      '(progn
	(setq sort-list (replase-item-in-list-by-number
			 (append(list (car (nth number-curent-profile sort-list))(cadr (nth number-curent-profile sort-list)))
			  (edit-list-add-item-by-number
			   (nth sel-standart-item sort-list-standart-str)
			   sel-list-podval-str-item
			   (cddr(nth number-curent-profile sort-list))))
			 number-curent-profile
			 sort-list))
	(start_list "list-podval-str")
	(foreach x (cddr(nth number-curent-profile sort-list)) (add_list (car x)))
	(end_list)
	(set_tile "list-podval-str" (itoa (setq sel-list-podval-str-item (1+ sel-list-podval-str-item))))
	(if (eq sel-list-podval-str-item 0)(mode_tile "up" 1)(mode_tile "up" 0))
	(if (eq sel-list-podval-str-item (1-(length(cddr(nth number-curent-profile sort-list)))))(mode_tile "down" 1)(mode_tile "down" 0))
	(if
	 (> (length(cddr(nth number-curent-profile sort-list)))1)
	 (mode_tile "del" 0)
	 )
	
	)
      )
    )
  
  (action_tile "edit"
    (vl-prin1-to-string
      '(progn
	(princ)
	(setq edit-list-profile-str (profile-dialog-edit-properti-to-string-profile (nth sel-list-podval-str-item(cddr(nth number-curent-profile sort-list)))))
	(if
	 edit-list-profile-str
	 (progn
	  (setq sort-list (replase-item-in-list-by-number
			   (append(list (car (nth number-curent-profile sort-list))(cadr (nth number-curent-profile sort-list)))
			    (replase-item-in-list-by-number
			     edit-list-profile-str
			     sel-list-podval-str-item
			     (cddr(nth number-curent-profile sort-list))))
			   number-curent-profile
			   sort-list))
	  (start_list "list-podval-str")
	  (foreach x (cddr(nth number-curent-profile sort-list)) (add_list (car x)))
	  (end_list)
	  )
	 )
	)
      )
    )

  (action_tile "edit_standart_str"
    (vl-prin1-to-string
      '(progn
	(princ)
	(setq edit-list-standart-str (profile-dialog-edit-properti-to-string-profile (nth sel-standart-item sort-list-standart-str)))
	(if
	 edit-list-standart-str
	 (progn
	  (setq sort-list-standart-str (replase-item-in-list-by-number
					edit-list-standart-str
					sel-standart-item
					sort-list-standart-str
					))
	  (start_list "list-standart-str")
	  (foreach x sort-list-standart-str (add_list (car x)))
	  (end_list)
	  (set_tile "list-standart-str"(itoa sel-standart-item))
	  )
	 )
	)
      )
    )
  
  (action_tile "del"
    (vl-prin1-to-string
      '(progn
	(setq sort-list (replase-item-in-list-by-number
			 (append(list (car (nth number-curent-profile sort-list))(cadr (nth number-curent-profile sort-list)))
			  (edit-list-del-item-by-number
			   sel-list-podval-str-item
			   (cddr(nth number-curent-profile sort-list))))
			 number-curent-profile
			 sort-list))
	(start_list "list-podval-str")
	(foreach x (cddr(nth number-curent-profile sort-list)) (add_list (car x)))
	(end_list)
	(setq sel-list-podval-str-item (if (eq sel-list-podval-str-item (length(cddr(nth number-curent-profile sort-list))))
					(1- sel-list-podval-str-item)
					sel-list-podval-str-item)
	 )
	(set_tile "list-podval-str" (itoa sel-list-podval-str-item))
	(if (eq sel-list-podval-str-item 0)(mode_tile "up" 1)(mode_tile "up" 0))
	(if (eq sel-list-podval-str-item (1-(length(cddr(nth number-curent-profile sort-list)))))(mode_tile "down" 1)(mode_tile "down" 0))
	(if
	 (eq (length(cddr(nth number-curent-profile sort-list))) 1)
	 (mode_tile "del" 1)
	 )
	)
      )
    )
  
  (action_tile "flag_ruler"
    (vl-prin1-to-string
      '(progn
	(if (eq flag_ruler 0)
	 (setq flag_ruler 1)
	 (setq flag_ruler 0))
	(setq sort-list (replase-item-in-list-by-number
			 (append(list (car (nth number-curent-profile sort-list))
				 (replase-item-in-list-by-number (itoa flag_ruler) 0 (cadr (nth number-curent-profile sort-list))))
			  (cddr(nth number-curent-profile sort-list)))
			 number-curent-profile
			 sort-list))
	)
      )
    )
  
  (action_tile "text-style"
    (vl-prin1-to-string
      '(progn
	
	(setq sort-list (replase-item-in-list-by-number
			 (append(list (car (nth number-curent-profile sort-list))
				 (replase-item-in-list-by-number (nth (atoi $value) list-text-style) 1 (cadr (nth number-curent-profile sort-list))))
			  (cddr(nth number-curent-profile sort-list)))
			 number-curent-profile
			 sort-list))
	)
      )
    )
  (action_tile "buffer-dist"
    (vl-prin1-to-string
      '(progn
	
	(setq sort-list (replase-item-in-list-by-number
			 (append(list (car (nth number-curent-profile sort-list))
				 (replase-item-in-list-by-number $value 3 (cadr (nth number-curent-profile sort-list))))
			  (cddr(nth number-curent-profile sort-list)))
			 number-curent-profile
			 sort-list))
	)
      )
    )
  (action_tile "d-basement"
    (vl-prin1-to-string
      '(progn
	
	(setq sort-list (replase-item-in-list-by-number
			 (append(list (car (nth number-curent-profile sort-list))
				 (replase-item-in-list-by-number $value 4 (cadr (nth number-curent-profile sort-list))))
			  (cddr(nth number-curent-profile sort-list)))
			 number-curent-profile
			 sort-list))
	)
      )
    )
  (action_tile "h-txt"
    (vl-prin1-to-string
      '(progn
	(setq sort-list (replase-item-in-list-by-number
			 (append(list (car (nth number-curent-profile sort-list))
				 (replase-item-in-list-by-number (rtos(atof $value)2 1) 2 (cadr (nth number-curent-profile sort-list))))
			  (cddr(nth number-curent-profile sort-list)))
			 number-curent-profile
			 sort-list))
	)
      )
    )
  
  (action_tile "new"
    (vl-prin1-to-string
      '(progn
	(if (setq new-profile-name (dialog-get-string-not-in-list-item (mapcar 'car sort-list) "Имя нового вида профиля" ""))
	 (progn
	  (setq sort-list (append sort-list(list(append(list new-profile-name
							(list "1" "***Текущий***" "4" "20" "80" "Условный горизонт" "М горизонтальный" "М вертикальный" "%%uПродольный профиль" "2.5" "2")
							)sort-list-standart-str))))
	  (setq number-curent-profile(1-(length sort-list)))
	  (start_list "profile-name")
	  (foreach x sort-list (add_list (car x)))
	  (end_list)
	  (set_tile "profile-name" (itoa number-curent-profile))
	  (start_list "list-podval-str")
	  (foreach x (cddr(nth number-curent-profile sort-list)) (add_list (car x)))
	  (end_list)
	  (setq flag_ruler (atoi(caadr(nth number-curent-profile sort-list))))
	  (set_tile "flag_ruler" (itoa flag_ruler))
	  (set_tile "text-style" (itoa (-(length list-text-style)(length(member (cadadr(nth number-curent-profile sort-list)) list-text-style)))))
	  (set_tile "list-standart-str" (itoa 0))
	  (set_tile "list-podval-str" (itoa 0))
	  (setq sel-standart-item 0)
	  (setq sel-list-podval-str-item 0)
	  (set_tile "h-txt" (rtos (atof(nth 2(cadr(nth number-curent-profile sort-list)))) 2 1))
	  (set_tile "buffer-dist" (nth 3(cadr(nth number-curent-profile sort-list))))
	  (set_tile "d-basement" (nth 4(cadr(nth number-curent-profile sort-list))))
	  (set_tile "str1" (nth 5(cadr(nth number-curent-profile sort-list))))
	  (set_tile "str2" (nth 6(cadr(nth number-curent-profile sort-list))))
	  (set_tile "str3" (nth 7(cadr(nth number-curent-profile sort-list))))
	  (set_tile "str4" (nth 8(cadr(nth number-curent-profile sort-list))))
	  (set_tile "h-txt-intersect" (rtos (atof(nth 9(cadr(nth number-curent-profile sort-list)))) 2 1))
	  (set_tile "accuracy-mleader" (nth 10(cadr(nth number-curent-profile sort-list))))
	  (mode_tile "up" 1)
	  (mode_tile "down" 0)
	  (if
	   (> (length sort-list)1)
	   (mode_tile "delete" 0)
	   )
	  ))
	)
      )
    )

  (action_tile "delete"
    (vl-prin1-to-string
      '(progn
	(setq sort-list (edit-list-del-item-by-number number-curent-profile sort-list))
	(setq number-curent-profile (if (eq number-curent-profile (length sort-list))
				     (1- number-curent-profile)
				     number-curent-profile))
	(start_list "profile-name")
	(foreach x sort-list (add_list (car x)))
	(end_list)
	(set_tile "profile-name" (itoa number-curent-profile))
	(start_list "list-podval-str")
	(foreach x (cddr(nth number-curent-profile sort-list)) (add_list (car x)))
	(end_list)
	(setq flag_ruler (atoi(caadr(nth number-curent-profile sort-list))))
	(set_tile "flag_ruler" (itoa flag_ruler))
	(set_tile "text-style" (itoa (-(length list-text-style)(length(member (cadadr(nth number-curent-profile sort-list)) list-text-style)))))
	(set_tile "list-standart-str" (itoa 0))
	(set_tile "list-podval-str" (itoa 0))
	(setq sel-standart-item 0)
	(setq sel-list-podval-str-item 0)
	(set_tile "h-txt" (rtos (atof(nth 2(cadr(nth number-curent-profile sort-list)))) 2 1))
	(set_tile "h-txt-intersect" (rtos (atof(nth 9(cadr(nth number-curent-profile sort-list)))) 2 1))
	(mode_tile "up" 1)
	(mode_tile "down" 0)
	(if
	 (eq (length sort-list) 1)
	 (mode_tile "delete" 1)
	 )
	)
      )
    )

  (action_tile "rename"
    (vl-prin1-to-string
      '(progn
	(if (setq new-profile-name (dialog-get-string-not-in-list-item
				    (vl-remove(car(nth number-curent-profile sort-list))(mapcar 'car sort-list))
				    "Новое имя вида профиля"
				    (car(nth number-curent-profile sort-list))))
	 (progn
	  (setq sort-list (replase-item-in-list-by-number
			 (append(list new-profile-name)(cdr (nth number-curent-profile sort-list)))
			 number-curent-profile
			 sort-list))
	  (start_list "profile-name")
	  (foreach x sort-list (add_list (car x)))
	  (end_list)
	  (set_tile "profile-name" (itoa number-curent-profile))
	  ))
	)
      )
    )

  (action_tile "str1"
    (vl-prin1-to-string
      '(progn
	(setq sort-list (replase-item-in-list-by-number
			 (append(list (car (nth number-curent-profile sort-list))
				 (replase-item-in-list-by-number $value 5 (cadr (nth number-curent-profile sort-list))))
			  (cddr(nth number-curent-profile sort-list)))
			 number-curent-profile
			 sort-list))
	)
      )
    )
  
  (action_tile "str2"
    (vl-prin1-to-string
      '(progn
	(setq sort-list (replase-item-in-list-by-number
			 (append(list (car (nth number-curent-profile sort-list))
				 (replase-item-in-list-by-number $value 6 (cadr (nth number-curent-profile sort-list))))
			  (cddr(nth number-curent-profile sort-list)))
			 number-curent-profile
			 sort-list))
	)
      )
    )
  (action_tile "str3"
    (vl-prin1-to-string
      '(progn
	(setq sort-list (replase-item-in-list-by-number
			 (append(list (car (nth number-curent-profile sort-list))
				 (replase-item-in-list-by-number $value 7 (cadr (nth number-curent-profile sort-list))))
			  (cddr(nth number-curent-profile sort-list)))
			 number-curent-profile
			 sort-list))
	)
      )
    )
  (action_tile "str4"
    (vl-prin1-to-string
      '(progn
	(setq sort-list (replase-item-in-list-by-number
			 (append(list (car (nth number-curent-profile sort-list))
				 (replase-item-in-list-by-number $value 8 (cadr (nth number-curent-profile sort-list))))
			  (cddr(nth number-curent-profile sort-list)))
			 number-curent-profile
			 sort-list))
	)
      )
    )
  (action_tile "h-txt-intersect"
    (vl-prin1-to-string
      '(progn
	(setq sort-list (replase-item-in-list-by-number
			 (append(list (car (nth number-curent-profile sort-list))
				 (replase-item-in-list-by-number (rtos(atof $value)2 1) 9 (cadr (nth number-curent-profile sort-list))))
			  (cddr(nth number-curent-profile sort-list)))
			 number-curent-profile
			 sort-list))
	)
      )
    )
  (action_tile "accuracy-mleader"
      (vl-prin1-to-string
	'(progn
	  (setq sort-list (replase-item-in-list-by-number
			   (append(list (car (nth number-curent-profile sort-list))
				   (replase-item-in-list-by-number $value 10 (cadr (nth number-curent-profile sort-list))))
			    (cddr(nth number-curent-profile sort-list)))
			   number-curent-profile
			   sort-list))
	  )
	)
      )
  
  (if (>(start_dialog)0)
    (progn
      (put-sorting-list-profile-dat sort-list)
      (put-sorting-list-standart-str sort-list-standart-str)
      (unload_dialog dcl_id)
      )
    (unload_dialog dcl_id)
    )
  number-curent-profile
  )