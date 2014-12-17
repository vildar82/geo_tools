(defun convert-item-in-string (item accuracy /)
  ;на входе значение на выходе строка
  (cond
    ((= (type item) 'INT)
     (itoa item)
     )
    ((= (type item) 'REAL)
     (rtos item 2 accuracy)
     )
    ((= (type item) 'STR)
     item
     )
    (T
     nil)
    )
  )