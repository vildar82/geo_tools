(defun get-ltype-list ( / tbl list_tbl)
  (setq tbl (tblnext "LTYPE" T))
  (while tbl
    (setq list_tbl (append list_tbl (list(cdr(assoc 2 tbl)))))
    (setq tbl (tblnext "LTYPE"))
    )
  (acad_strlsort list_tbl)
  )