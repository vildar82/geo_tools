(defun dialog_action_tile_in_shtamp (d list_cell / n dcl_id)
  (setq rezerv list_cell)
  (setq dcl_id (geo-tools-load-dialog d))
  (if (not (new_dialog d dcl_id))(exit))
  (dialog_set_the_tile_all list_cell)
  (dialog_action_the_tile_all)
  
  (if (=(start_dialog)0)
    (progn
      (unload_dialog dcl_id)
      rezerv)
    (progn
      (unload_dialog dcl_id)
      list_cell)
    )
  )
