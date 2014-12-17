(defun convert-string-to-list-be-delimiter (_str _br / _pos)
  ; конвертирует строку в список по указаному разделителю
  (if (setq _pos (vl-string-search _br _str))
    (cons (substr _str 1 _pos)
	  (convert-string-to-list-be-delimiter (substr _str (+ (strlen _br) _pos 1)) _br))
    (list _str)
    ))