(defun analisis-addres-from-type (string /)
  (cond
    ((or(vl-string-search "СИГНАЛ" string )(vl-string-search "Сигнал" string )(vl-string-search "сигнал" string ))
     "сигнал"
     )
    ((or(vl-string-search "НАДСТРОЙКА" string )(vl-string-search "Надстройка" string )(vl-string-search "надстройка" string ))
     "надстройка"
     )
    ((or(vl-string-search "ПИРАМИДА" string )(vl-string-search "Пирамида" string )(vl-string-search "пирамида" string ))
     "пирамида"
     )
    ((or(vl-string-search "КЕРН" string )(vl-string-search "Керн" string )(vl-string-search "керн" string ))
     "керн"
     )
    ((vl-string-search "ПЗ" string )
     "ПЗ"
     )
    ((or(vl-string-search "ШПИЛЬ" string )(vl-string-search "Шпиль" string )(vl-string-search "шпиль" string )
	(vl-string-search "ГPОМООТВОД" string )(vl-string-search "Гpомоотвод" string )(vl-string-search "гpомоотвод" string ))
     "местн.предмет"
     )
    
    ((or(vl-string-search "МАРКА" string )(vl-string-search "Марка" string )(vl-string-search "марка" string ))
     "марка"
     )
    ((or(vl-string-search "РЕЛЬС" string )(vl-string-search "Рельс" string )(vl-string-search "рельс" string ))
     "рельс"
     )
    ((or(vl-string-search "ДЮБЕЛЬ" string )(vl-string-search "Дюбель" string )(vl-string-search "дюбель" string ))
     "дюбель"
     )
    ((or(vl-string-search "ГВОЗДЬ" string )(vl-string-search "Гвоздь" string )(vl-string-search "гвоздь" string ))
     "гвоздь"
     )
    )
  )