(defun profile-dialog-seleckt-curent-profile (list-name-block-profile / dcl_id list-name-profile name-profile curent-profile)
  ; генерирует диалоговое окно для выбора нового текущего профиля
  (setq curent-profile(vlax-ldata-get "geo_tools_dictionary" "curent-profile"))
  
  (setq list-name-profile (acad_strlsort(car list-name-block-profile)))
  (if curent-profile
    (setq name-profile (analysis-number-item-in-list
			 (car(nth(analysis-number-item-in-list curent-profile
				   (nth 1 list-name-block-profile))
				 (nth 1 list-name-block-profile)))
			 list-name-profile))
    (setq name-profile 0)
    )
  (setq dcl_id (geo-tools-load-dialog "d_popup_list_sel"))
  (new_dialog "d_popup_list_sel" dcl_id)
  (set_tile "top" "Выбор текущего профиля")
  (start_list "popup_list_sel")
  (mapcar 'add_list list-name-profile)
  (end_list)
  (set_tile "popup_list_sel" (itoa name-profile))
  (action_tile "popup_list_sel" "(setq name-profile (atoi $value))")
  (if (eq (start_dialog) 0) (progn (unload_dialog dcl_id)(setq name-profile nil))(unload_dialog dcl_id))
;;;  (setq name-profile(if name-profile(nth name-profile list-name-profile)))
  (if name-profile(assoc (nth name-profile list-name-profile) (nth 1 list-name-block-profile)))
  )