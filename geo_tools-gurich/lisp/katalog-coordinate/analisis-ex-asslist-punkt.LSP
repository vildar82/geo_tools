(defun analisis-ex-asslist-punkt (asslist flag-type flag-year / address type_new year_new)
  ; попытка определения по адресу типа пункта и даты получения данных по пункту
  (setq address (get-item-by-name-in-list asslist "address"))
  (if (and flag-type address (= (get-item-by-name-in-list asslist "type") nil))
    (setq type_new (analisis-addres-from-type address))
    )
  (if (and flag-year address (= (get-item-by-name-in-list asslist "year") nil))
    (setq year_new (analisis-string-from-data address))
    )
  (if type_new (setq asslist (put-item-by-name-in-list asslist "type" type_new)))
  (if (> year_new 0) (setq asslist (put-item-by-name-in-list asslist "year" (itoa year_new))))
  asslist
  )