(defun convert-bool-to-string ( item / )
  ; конвертирует nil (ложь) "0", все остальное в "1"
  (if item
    "1"
    "0"
    )
  )