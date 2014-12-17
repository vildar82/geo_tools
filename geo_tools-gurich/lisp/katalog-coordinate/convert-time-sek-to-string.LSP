(defun convert-time-sek-to-string (seconds /  days hour minuts second string)
  ;конвертирует число секунд в строку вида "1д 15:35:02"
  (setq days (fix(/ seconds 86400)))
  (setq hour (fix(/(- seconds(* days 86400))3600)))
  (setq minuts (fix(/(- seconds(+(* hour 3600)(* days 86400)))60)))
  (setq second (fix(- seconds(+(* minuts 60)(* hour 3600)(* days 86400)))))
  (setq string(if(> days 0)(strcat(itoa days)"д ")""))
  (setq string(strcat string(if(> hour 0)(if(< hour 10)(strcat"0"(itoa hour)":")(strcat(itoa hour)":"))"")))
  (setq string(strcat string(if(> minuts 0)(if(< minuts 10)(strcat"0"(itoa minuts)":")(strcat(itoa minuts)":"))"0:")))
  (setq string(strcat string(if(> second 0)(if(< second 10)(strcat"0"(itoa second))(itoa second))"00")))
  string
  )