(defun analysis_member_point_in_list_equal (item _list tochnost / number coorent_item result)
  ; сравнивает список координат точки со списком координат точек, z не учитывается
  (setq number 0)
  (while (not (eq number (length _list)))
    (setq coorent_item (nth number _list))
    (if (and (equal (car item)(car coorent_item) tochnost)(equal (cadr item)(cadr coorent_item) tochnost))
      (progn
	(setq result number)
	(setq number (length _list))
	)
      (setq number (1+ number))
      )
    )
  result)
