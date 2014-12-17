(defun get_layers_list ( / tbl list_tbl)
  (setq tbl (tblnext "LAYER" T))
  (while tbl
    (setq list_tbl (append list_tbl (list(cdr(assoc 2 tbl)))))
    (setq tbl (tblnext "LAYER"))
    )
  (acad_strlsort list_tbl)
  )