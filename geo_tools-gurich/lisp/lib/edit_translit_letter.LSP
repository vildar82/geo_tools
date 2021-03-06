(defun edit_translit_letter (cod / bukvi n resultat)
  (setq bukvi
	 (list
	   '("�" "a")'("�" "b")'("�" "v")'("�" "g")'("�" "d")'("�" "e")'("�" "e")'("�" "j")'("�" "z")'("�" "i")'("�" "i")'("�" "k")
	   '("�" "l")'("�" "m")'("�" "n")'("�" "o")'("�" "p")'("�" "r")'("�" "s")'("�" "t")'("�" "u")'("�" "f")'("�" "h")'("�" "c")
	   '("�" "ch")'("�" "sh")'("�" "sch")'("�" "'")'("�" "y")'("�" "'")'("�" "e")'("�" "yu")'("�" "ya")'("�" "A")'("�" "B")
	   '("�" "V")'("�" "G")'("�" "D")'("�" "E")'("�" "E")'("�" "J")'("�" "Z")'("�" "I")'("�" "I")'("�" "K")'("�" "L")'("�" "M")
	   '("�" "N")'("�" "O")'("�" "P")'("�" "R")'("�" "S")'("�" "T")'("�" "U")'("�" "F")'("�" "H")'("�" "C")'("�" "CH")'("�" "SH")
	   '("�" "SCH")'("�" "'")'("�" "Y")'("�" "'")'("�" "E")'("�" "YU")'("�" "YA")
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
