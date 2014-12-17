(defun analisis-st-raw-string-nikon (string current-param / result)
  ; преобразование строки с описанием станции в ассоциативный список с конвертацией
  (setq list-str (convert-string-to-list-be-delimiter string ","))
  (setq result(list
    (list "station-name" (nth 1 list-str))
    (list "base-name" (nth 3 list-str))
    (list "h-instrument" (atof(nth 5 list-str)))
    (list "base-azimuth" (convert-str-to-deg-grad(nth 6 list-str)))
    (list "base-horizon-angle" (convert-str-to-deg-grad(nth 7 list-str)))
    ))
  (if current-param
    (append
      result
      current-param)
    result)
  )