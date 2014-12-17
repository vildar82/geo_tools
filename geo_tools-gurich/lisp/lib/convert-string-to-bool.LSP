(defun convert-string-to-bool ( item / )
  ; конвертирует "1" в T (истина), все остальное в nil (ложь)
  (if (= item "1")
    T
    nil
    )
  )