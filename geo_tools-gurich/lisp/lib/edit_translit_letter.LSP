(defun edit_translit_letter (cod / bukvi n resultat)
  (setq bukvi
	 (list
	   '("à" "a")'("á" "b")'("â" "v")'("ã" "g")'("ä" "d")'("å" "e")'("¸" "e")'("æ" "j")'("ç" "z")'("è" "i")'("é" "i")'("ê" "k")
	   '("ë" "l")'("ì" "m")'("í" "n")'("î" "o")'("ï" "p")'("ð" "r")'("ñ" "s")'("ò" "t")'("ó" "u")'("ô" "f")'("õ" "h")'("ö" "c")
	   '("÷" "ch")'("ø" "sh")'("ù" "sch")'("ú" "'")'("û" "y")'("ü" "'")'("ý" "e")'("þ" "yu")'("ÿ" "ya")'("À" "A")'("Á" "B")
	   '("Â" "V")'("Ã" "G")'("Ä" "D")'("Å" "E")'("¨" "E")'("Æ" "J")'("Ç" "Z")'("È" "I")'("É" "I")'("Ê" "K")'("Ë" "L")'("Ì" "M")
	   '("Í" "N")'("Î" "O")'("Ï" "P")'("Ð" "R")'("Ñ" "S")'("Ò" "T")'("Ó" "U")'("Ô" "F")'("Õ" "H")'("Ö" "C")'("×" "CH")'("Ø" "SH")
	   '("Ù" "SCH")'("Ú" "'")'("Û" "Y")'("Ü" "'")'("Ý" "E")'("Þ" "YU")'("ß" "YA")
	   )
	)
  (setq n 1)
  (while (not(eq n (length bukvi)))
    (if (eq cod (ascii(car(nth (1- n)bukvi))))
      (progn
      (setq resultat (cadr(nth (1- n)bukvi)))
      (setq n (length bukvi))
      )
      (setq n (1+ n))
      )
    )
  (if (eq resultat nil) (setq resultat (chr cod)))
  resultat
  )
