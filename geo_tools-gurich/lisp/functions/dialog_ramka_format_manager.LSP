(defun C:dialog_ramka_format_manager
       ( / dcl_id typ_GOST typ_ISO typ_USER
	shtamp_GOST shtamp_BIG shtamp_SMAL shtamp_NET
	vibor_ramka shtamp step ins_smal_shtamp vibor_ramka_text
	data_dialog_ramka_format_manager list_format model-list block_name_list width height)
  (geo_tools-help "dialog_ramka_format_manager")

  (if (eq (setq data_dialog_ramka_format_manager (PL:GetD "data_dialog_ramka_format_manager")) nil)
    (setq data_dialog_ramka_format_manager (list 1 0 0 0 0 0 1 0 0 1 1))
    )
  (setq dcl_id (geo-tools-load-dialog "d_format_GOST_ISO"))
  (setq step 2)
  (while (>= step 2)
    (setq typ_GOST (nth 0 data_dialog_ramka_format_manager))
    (setq typ_ISO (nth 1 data_dialog_ramka_format_manager))
    (setq typ_USER (nth 2 data_dialog_ramka_format_manager))
    (setq shtamp_GOST (nth 3 data_dialog_ramka_format_manager))
    (setq shtamp_BIG (nth 4 data_dialog_ramka_format_manager))
    (setq shtamp_SMAL (nth 5 data_dialog_ramka_format_manager))
    (setq shtamp_NET (nth 6 data_dialog_ramka_format_manager))
    (setq vibor_ramka (nth 7 data_dialog_ramka_format_manager))
    (setq rotate_ramka (nth 8 data_dialog_ramka_format_manager))
    (setq ins_smal_shtamp (nth 9 data_dialog_ramka_format_manager))
    (setq model-list (nth 10 data_dialog_ramka_format_manager))
    (new_dialog "d_format_GOST_ISO" dcl_id)
    (start_list "vibor_ramka")
    (cond
	((eq typ_GOST 1)
	 (mapcar 'add_list (reading_file_dat_format_manager "ГОСТ.dat"))
	 )
	((eq typ_ISO 1)
	 (mapcar 'add_list (reading_file_dat_format_manager "ISO.dat"))
	 )
	((eq typ_USER 1)
	 (mapcar 'add_list (append (reading_file_dat_format_manager "USER.dat")(list "Добавить...")))
	 )
	)
        (end_list)
    (set_tile "typ_GOST" (itoa typ_GOST))
    (set_tile "typ_ISO" (itoa typ_ISO))
    (set_tile "typ_USER" (itoa typ_USER))
    (set_tile "shtamp_GOST" (itoa shtamp_GOST))
    (set_tile "shtamp_BIG" (itoa shtamp_BIG))
    (set_tile "shtamp_SMAL" (itoa shtamp_SMAL))
    (set_tile "shtamp_NET" (itoa shtamp_NET))
    (set_tile "vibor_ramka" (itoa vibor_ramka))
    (set_tile "rotate_ramka" (itoa rotate_ramka))
    (set_tile "ins_smal_shtamp" (itoa ins_smal_shtamp))
    (set_tile "model-list" (itoa model-list))

    (action_tile "typ_GOST"
      (vl-prin1-to-string
	'(progn
	  (setq list_format (reading_file_dat_format_manager "ГОСТ.dat"))
	  (start_list "vibor_ramka")
	  (mapcar 'add_list list_format)
	  (end_list)
	  (if (<(1-(length list_format))vibor_ramka)
	   (progn
	    (setq vibor_ramka (1-(length list_format)))
	    (set_tile "vibor_ramka" (itoa vibor_ramka))
	    )
	   (set_tile "vibor_ramka" (itoa vibor_ramka))
	   )
	  (setq typ_GOST 1)(setq typ_ISO 0)(setq typ_USER 0)
	  )
	)
      )
    (action_tile "typ_ISO"
      (vl-prin1-to-string
	'(progn
	  (setq list_format (reading_file_dat_format_manager "ISO.dat"))
	  (start_list "vibor_ramka")
	  (mapcar 'add_list list_format)
	  (end_list)
	  (if (<(1-(length list_format))vibor_ramka)
	   (progn
	    (setq vibor_ramka (1-(length list_format)))
	    (set_tile "vibor_ramka" (itoa vibor_ramka))
	    )
	   (set_tile "vibor_ramka" (itoa vibor_ramka))
	   )
	  (setq typ_GOST 0)(setq typ_ISO 1)(setq typ_USER 0)
	  )
	)
      )
    (action_tile "typ_USER"
      (vl-prin1-to-string
	'(progn
	  (setq list_format (reading_file_dat_format_manager "USER.dat"))
	  (start_list "vibor_ramka")
	  (mapcar 'add_list (append list_format(list "Добавить...")))
	  (end_list)
	  (if (<(1-(length list_format))vibor_ramka)
	   (progn
	    (setq vibor_ramka (1-(length list_format)))
	    (set_tile "vibor_ramka" (itoa vibor_ramka))
	    )
	   (set_tile "vibor_ramka" (itoa vibor_ramka))
	   )
	  (setq typ_GOST 0)(setq typ_ISO 0)(setq typ_USER 1)
	  )
	)
      )
    
    (action_tile "shtamp_GOST" "(setq shtamp_GOST 1)(setq shtamp_BIG 0)(setq shtamp_SMAL 0)(setq shtamp_NET 0)")
    (action_tile "shtamp_BIG"  "(setq shtamp_GOST 0)(setq shtamp_BIG 1)(setq shtamp_SMAL 0)(setq shtamp_NET 0)")
    (action_tile "shtamp_SMAL" "(setq shtamp_GOST 0)(setq shtamp_BIG 0)(setq shtamp_SMAL 1)(setq shtamp_NET 0)")
    (action_tile "shtamp_NET"  "(setq shtamp_GOST 0)(setq shtamp_BIG 0)(setq shtamp_SMAL 0)(setq shtamp_NET 1)")
    (action_tile "vibor_ramka"
      "(if
      (and (eq (atoi $value) (length(reading_file_dat_format_manager \"USER.dat\")))(eq typ_USER 1))
      (done_dialog 3)
      (setq vibor_ramka (atoi $value)))"
      )
    (action_tile "rotate_ramka"; повернуть рамку
      "(if (= rotate_ramka 0)
    (setq rotate_ramka 1)
    (setq rotate_ramka 0)
    )"
      )
    (action_tile "ins_smal_shtamp"; вставить маленький (слева) штамп
      "(if (= ins_smal_shtamp 0)
    (setq ins_smal_shtamp 1)
    (setq ins_smal_shtamp 0)
    )"
      )
    (action_tile "model-list"; без вспомогательных рамок
      "(if (= model-list 0)
    (setq model-list 1)
    (setq model-list 0)
    )"
      )
    
    (setq step (start_dialog))
    (setq data_dialog_ramka_format_manager (list typ_GOST typ_ISO typ_USER shtamp_GOST shtamp_BIG shtamp_SMAL shtamp_NET vibor_ramka rotate_ramka ins_smal_shtamp model-list))
    (if (= step 3)
      (progn
	(dialog_add_new_USER_ramka_format dcl_id)
	))
    )
  (unload_dialog dcl_id)
  (if (/= step 0)
    (progn
      (cond
	((eq typ_GOST 1)
	 (setq vibor_ramka_text (nth vibor_ramka (reading_file_dat_format_manager "ГОСТ.dat")))
	 )
	((eq typ_ISO 1)
	 (setq vibor_ramka_text (nth vibor_ramka (reading_file_dat_format_manager "ISO.dat")))
	 )
	((eq typ_USER 1)
	 (setq vibor_ramka_text (nth vibor_ramka (reading_file_dat_format_manager "USER.dat")))
	 )
	)
      (cond
	((eq shtamp_GOST 1)(setq shtamp "ГОСТ"))
	((eq shtamp_BIG 1)(setq shtamp "Большой"))
	((eq shtamp_SMAL 1)(setq shtamp "Маленький"))
	((eq shtamp_NET 1)(setq shtamp "Нет"))
	)
      (if (eq rotate_ramka 1)
	(setq vibor_ramka_text(edit_str_replace_h_d_ramka_format vibor_ramka_text))
	(setq vibor_ramka_text(vl-string-subst "" "("(vl-string-subst "" ")" vibor_ramka_text)))
	)
      (if (eq ins_smal_shtamp 1)
	(setq ins_smal_shtamp T)
	(setq ins_smal_shtamp nil)
	)
      (if (eq model-list 1)
	(setq model-list T)
	(setq model-list nil)
	)
      (PL:SetD "data_dialog_ramka_format_manager" data_dialog_ramka_format_manager)
      (draw_blok_ramka
	vibor_ramka_text; строка формата вида "ГОСТ А1 594x841"
	"Нет"           ; автоматическая вставка? если "Нет" будет предложено указать точку вставки блока рамкт иначе блок создастся но не вставится
	shtamp          ; какой штамп необходимо вставить в рамку "ГОСТ" "Большой" "Маленький" "Нет"
	ins_smal_shtamp ; флаг вставки маленького штампа с инвентарным номером: T или nil
	model-list      ; флаг отключения вспомогательных рамок в листе: T или nil
	)
      )
    )
  (princ)
  )

(defun reading_file_dat_format_manager (name / )
    (if (eq name "USER.dat")
      (file_read_to_list (strcat *geo_tools_user_dat_dir*"\\data\\" name))
      (file_read_to_list (strcat *geo_tools_root_dir*"\\data\\" name)))
    )