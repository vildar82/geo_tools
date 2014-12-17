(defun get-text-style-list ( / tbl list_tbl)
  ; Возвращает имена текстовых стилей чертежа в виде списка с текстами
  (setq tbl (tblnext "STYLE" T))
  (while tbl
    (setq list_tbl (append list_tbl (list(cdr(assoc 2 tbl)))))
    (setq tbl (tblnext "STYLE"))
    )
  (cdr(acad_strlsort list_tbl))
  )