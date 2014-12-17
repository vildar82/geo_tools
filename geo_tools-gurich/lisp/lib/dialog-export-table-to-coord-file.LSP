(defun dialog-export-table-to-coord-file (table_name spisok / dcl_id
					  command-list name-list
					  )
  (setq command-list (vlax-ldata-get"geo_tools_dictionary" "data-dialog-export-table-to-file"))
  (if (eq command-list nil)
    (setq command-list (list "1" "1" "1" "1" "1" "_" "_" "0" "1" "2" "0")))

  (setq dcl_id (geo-tools-load-dialog "d_export_table_to_sdr"))
  (new_dialog "d_export_table_to_sdr" dcl_id)

  (setq name-list (list "flag1" "flag2" "flag3" "flag4" "flag5" "symbol1" "symbol2" "select1" "select2" "select3" "format"))
  (foreach x name-list
    (set_tile x (nth (analysis-number-item-in-list x name-list) command-list))
    )
  (set_tile_text_str_for_dialog_export_table_to_sdr command-list table_name spisok)
  (action_tile "flag1" "(setq command-list (replase-item-in-list-by-number $value 0 command-list))(set_tile_text_str_for_dialog_export_table_to_sdr command-list table_name spisok)")
  (action_tile "flag2" "(setq command-list (replase-item-in-list-by-number $value 1 command-list))(set_tile_text_str_for_dialog_export_table_to_sdr command-list table_name spisok)")
  (action_tile "flag3" "(setq command-list (replase-item-in-list-by-number $value 2 command-list))(set_tile_text_str_for_dialog_export_table_to_sdr command-list table_name spisok)")
  (action_tile "flag4" "(setq command-list (replase-item-in-list-by-number $value 3 command-list))(set_tile_text_str_for_dialog_export_table_to_sdr command-list table_name spisok)")
  (action_tile "flag5" "(setq command-list (replase-item-in-list-by-number $value 4 command-list))(set_tile_text_str_for_dialog_export_table_to_sdr command-list table_name spisok)")
  (action_tile "symbol1" "(setq command-list (replase-item-in-list-by-number $value 5 command-list))(set_tile_text_str_for_dialog_export_table_to_sdr command-list table_name spisok)")
  (action_tile "symbol2" "(setq command-list (replase-item-in-list-by-number $value 6 command-list))(set_tile_text_str_for_dialog_export_table_to_sdr command-list table_name spisok)")
  (action_tile "select1" "(setq command-list (replase-item-in-list-by-number $value 7 command-list))(set_tile_text_str_for_dialog_export_table_to_sdr command-list table_name spisok)")
  (action_tile "select2" "(setq command-list (replase-item-in-list-by-number $value 8 command-list))(set_tile_text_str_for_dialog_export_table_to_sdr command-list table_name spisok)")
  (action_tile "select3" "(setq command-list (replase-item-in-list-by-number $value 9 command-list))(set_tile_text_str_for_dialog_export_table_to_sdr command-list table_name spisok)")
  (action_tile "format" "(setq command-list (replase-item-in-list-by-number $value 10 command-list))(set_tile_text_str_for_dialog_export_table_to_sdr command-list table_name spisok)")
  (action_tile "save"
    (vl-prin1-to-string
	'(progn
	  (if (= (nth 0 command-list) (nth 1 command-list) (nth 2 command-list) (nth 3 command-list) (nth 4 command-list) "0")
	   (alert"Внимание!\nНедостаточно данных!\nВсе выключено!")(done_dialog 2))
	  )
	)
    )
  (if (eq (start_dialog) 2)
    (progn
      (vlax-ldata-put"geo_tools_dictionary" "data-dialog-export-table-to-file"command-list)
      command-list
      )
    )
  )