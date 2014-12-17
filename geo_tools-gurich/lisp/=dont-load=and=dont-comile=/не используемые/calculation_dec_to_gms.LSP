(defun calculation_dec_to_gms (dec /); переводит десятичные градусы в текстовую строку градусы минуты секунды 180d30'15"
  (strcat (itoa(fix dec))
	  "%%d"
	  (itoa(fix(*(- dec(fix dec))60)))
	  "'"
	  (itoa(fix(*(-(*(- dec(fix dec))60)(fix(*(- dec(fix dec))60)))60)))
	  "\"")
  )
