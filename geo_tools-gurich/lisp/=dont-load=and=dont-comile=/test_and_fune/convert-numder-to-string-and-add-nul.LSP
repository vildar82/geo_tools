(defun convert-numder-to-string-and-add-nul (number l-string / )
  (setq number (itoa number))
  (while (not(or (= (strlen number) l-string)(> (strlen number) l-string)))
    (setq number (strcat "0" number))
    )
  number
  )