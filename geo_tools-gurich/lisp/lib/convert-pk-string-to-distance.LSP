(defun convert-pk-string-to-distance (string / pos)
  ; перевод строки пикета в длинну
  ; "1+59.19" -> 159.19
  ; "2+0.6"   -> 200.60
  (if(setq pos(vl-string-search "+" string))
    (+ (*(atof(substr string 1 pos))100.0) (atof(substr string (+ 2 pos))))
    (atof string)
    )
  )