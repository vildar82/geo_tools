(defun yes_no_dialog (text_str / result dcl_id)
  (setq dcl_id (geo-tools-load-dialog "d_YES_NO"))
  (new_dialog "d_YES_NO" dcl_id)
  (set_tile "text_str" text_str)
  (if (eq (start_dialog) -1) (progn (unload_dialog dcl_id)(setq result nil))(progn (unload_dialog dcl_id)(setq result T)))
  result
  )