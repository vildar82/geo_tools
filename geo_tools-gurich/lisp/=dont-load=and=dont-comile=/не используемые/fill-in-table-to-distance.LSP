(defun C:fill-in-table-to-distance ( / line-obj table temp row column list-lengts)
  ; заполняет таблицу длиннами сегментов полилинии начиная с выбраной ячейки вниз
  (defun *error* (msg)
    (draw_osnap_on)
    (princ)
    )
  (if (setq line-obj(car(entsel "\nВыбери линию<Выход>:")))
    (if (eq (cdr(assoc 0 (entget line-obj)))"LWPOLYLINE"); контроль
      (progn
	(draw_osnap_off)
	(setq temp(get-table-cell-by-point(getpoint "\nУкажи ячейку в таблице:")))
	(draw_osnap_on)
	(if temp
	  (progn
	    (vla-StartUndoMark active_document)
	    (setq table (get-item-by-name-in-list temp "table"))
	    (setq row (get-item-by-name-in-list temp "row"))
	    (setq column (get-item-by-name-in-list temp "column"))
	    (setq list-lengts (list-lengts-lwpolyline line-obj))
	    (foreach x list-lengts
	      (if (=(vla-get-Rows table)row)
		(progn
		  (vla-InsertRows table row (vla-GetRowHeight table (1- row)) 1)
		  (vla-SetCellTextHeight table row column (vla-GetCellTextHeight table (1- row)column))
		  (vla-SetCellAlignment table row column (vla-GetCellAlignment table (1- row)column))
		  )
		)
	      (vlax-invoke-method table 'SetText row column (rtos x 2 2))
	      (vlax-invoke-method table 'Update)
	      
	      (setq row (1+ row))
	      )
	    (vla-EndUndoMark active_document)
	    )
	  (princ"\nНе попал(а) в таблицу! Прицелься лучше!")
	  )
	)
      (princ"\nВыбран недопустимый объект! Выбирать надо полилинию!")
      )
    (princ"\nНичего не выбрано!")
    )
  (princ)
  )