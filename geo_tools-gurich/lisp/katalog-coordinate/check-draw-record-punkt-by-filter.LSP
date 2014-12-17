(defun check-draw-record-punkt-by-filter (punkt-recorg filter-list / ass-list temp)
  (setq ass-list (convert-punkt-recorg-to-ass-list punkt-recorg))
  (cond
    ((= (get-item-by-name-in-list filter-list "filter-mode") 1)
     nil
     )
    ((and (= (get-item-by-name-in-list filter-list "no-z") 0)(=(get-item-by-name-in-list ass-list "Z")nil))
     nil
     )
    ((and (= (get-item-by-name-in-list filter-list "yes-z") 0)(/=(get-item-by-name-in-list ass-list "Z")nil))
     nil
     )
    ((and (= (get-item-by-name-in-list filter-list "no-year") 0)(=(get-item-by-name-in-list ass-list "year")nil))
     nil
     )
    ((and (= (get-item-by-name-in-list filter-list "years-limit") 1)
	  (/=(get-item-by-name-in-list ass-list "year")nil)
	  (or(<(atoi(get-item-by-name-in-list ass-list "year"))(get-item-by-name-in-list filter-list "year-min"))
	     (>(atoi(get-item-by-name-in-list ass-list "year"))(get-item-by-name-in-list filter-list "year-max")))
	  )
     nil
     )
    ((and (= (get-item-by-name-in-list filter-list "tipe-filter") 1)
	  (= (get-item-by-name-in-list (get-item-by-name-in-list filter-list "list-tipe-punkts")
	       (if (setq temp(get-item-by-name-in-list ass-list "type"))temp "не определен") )
	     1)
	  )
     nil
     )
    (T
     T)
    )
  )