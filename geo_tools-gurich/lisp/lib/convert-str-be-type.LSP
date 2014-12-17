(defun convert-str-be-type (str-item  type-item /)
;на входе две строки на выходе значение в указаном во второй строке типе
  (cond
    ((= type-item "INT")
     (atoi str-item)
     )
    ((= type-item "REAL")
     (atof str-item)
     )
    ((= type-item "STR")
     str-item
     )
    (T
     nil)
    )
  )