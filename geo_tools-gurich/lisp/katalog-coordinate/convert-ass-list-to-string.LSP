(defun convert-ass-list-to-string (ass-list / )
  (convert-list-to-string-be-delimiter
    (list
      (if(cadr(assoc "name" ass-list))(cadr (assoc "name" ass-list))"")
      (if(cadr(assoc "N" ass-list))(cadr (assoc "N" ass-list))"")
      (if(cadr(assoc "E" ass-list))(cadr (assoc "E" ass-list))"")
      (if(cadr(assoc"Z"ass-list))(cadr(assoc"Z"ass-list))"")
      (if(cadr(assoc"type"ass-list))(cadr(assoc"type"ass-list))"")
      (if(cadr(assoc"address"ass-list))(cadr(assoc"address"ass-list))"")
      (if(cadr(assoc"year"ass-list))(cadr(assoc"year"ass-list))"")
      (if(cadr(assoc"comment"ass-list))(cadr(assoc"comment"ass-list))"")
      )"|" )
  )