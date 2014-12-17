(defun geo-tools-load-dialog (name-dialog / put)
  (setq put (strcat  *geo_tools_root_dir* "\\lisp\\dcl\\" name-dialog ".DCL"))
  (if file_check
    (load_dialog put)
    nil)
  )