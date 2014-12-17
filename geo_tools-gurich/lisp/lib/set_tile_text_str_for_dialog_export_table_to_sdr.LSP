(defun set_tile_text_str_for_dialog_export_table_to_sdr
       (com_list table_name spisok / )
  (set_tile "text_str" (edit-list-to-strcat com_list (car spisok)table_name))
  )