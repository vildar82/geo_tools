(defun create-table-style (/ item table_obj num_row)
  (vla-StartUndoMark active_document)
  (create-text-style "OTI" "ESKDU.shx")
   (setq
     item (vla-addobject
       (vla-item (vla-get-dictionaries
              (vla-get-activedocument (vlax-get-acad-object))
              );_ end of vla-get-dictionaries
            "acad_tablestyle"
            );_ end of vla-item
       "Координаты"
       "AcDbTableStyle"
       );_ end of vla-AddObject
     );_ end of setq
   (vla-put-name item "Координаты"); имя стиля
   (vla-put-description item "Таблица каталога координат"); описание стиля
   (vla-put-flowdirection item 0); направление (?)
   (vla-put-horzcellmargin item 0.5); гориз.отступ текста от границы ячейки
   (vla-put-vertcellmargin item 0.5); верт.отступ текста от границы ячейки
   (vla-settextheight 
     item 
     acdatarow 
     2.0
     ); высота текста по умолчанию для datacells 
   (vla-settextheight 
     item 
     acheaderrow 
     2.0
     ); высота текста для шапки таблицы 
   (vla-settextheight 
     item 
     actitlerow 
     2.0
     ); высота текста для заголовка таблицы 
   (vla-settextstyle 
     item 
     (+ acdatarow acheaderrow actitlerow)
     "OTI"
     ); стиль текста по умолчанию для всех ячеек
;;;   (vla-put-colorindex
;;;     (vla-getbackgroundcolor
;;;       item
;;;       (+ acdatarow acheaderrow actitlerow)
;;;       );_ end of vla-GetBackgroundColor
;;;     256
;;;     ); установка цвета "заднего фона"
   ;;(vla-setbackgroundcolor item 3 1)
;;;   (vla-setbackgroundcolornone item 7 :vlax-true)
;;;   (vla-put-headersuppressed item :vlax-false)
;;;   (vla-put-titlesuppressed item :vlax-false)
      (vla-setgridvisibility
     item
     (+ achorztop acvertleft acvertright)
     actitlerow
     :vlax-false
     );_ end of vla-SetGridVisibility
;;;     (vla-setgridvisibility
;;;     item
;;;     (+ achorzinside acvertinside achorztop achorzbottom acvertleft acvertright)
;;;     actitlerow
;;;     :vlax-false
;;;     );_ end of vla-SetGridVisibility
   (vla-setgridvisibility 
     item 
     (+ achorzinside acvertinside achorztop achorzbottom acvertleft acvertright)
     (+ acdatarow acheaderrow)
     :vlax-true 
     );_ end of vla-SetGridVisibility
;;;      (vla-setgridvisibility 
;;;     item 
;;;     achorztop
;;;     acheaderrow
;;;     :vlax-true 
;;;     );_ end of vla-SetGridVisibility

;;;   (vla-setgridlineweight
;;;     item 
;;;     (+ achorzinside acvertinside)
;;;     (+ acdatarow acheaderrow actitlerow)
;;;     aclnwt025 
;;;     );_ end of vla-SetGridLineWeight
;;;   (vla-setgridlineweight
;;;     item 
;;;     (+ achorztop achorzbottom acvertright acvertleft)
;;;     (+ acdatarow acheaderrow actitlerow)
;;;     aclnwt050
;;;     );_ end of vla-SetGridLineWeight




	  (setq table_obj (vla-AddTable model_spece (vlax-3D-point (list 0.0 0.0 0.0) )2 5 12 20))
	  (vla-put-StyleName table_obj "Координаты");(vla-put-StyleName table_obj "Standard")"Координаты"
	  (vla-SetColumnWidth table_obj 0 8)
	  
	  (vla-SetText table_obj 0 0 "Имя таблицы")
	  (vla-SetCellTextHeight table_obj 0 0 4.0)
	  (vla-SetCellAlignment table_obj 0 0 acMiddleCenter)
	  
	  (vla-SetText table_obj 1 0 "№\nп/п")
	  (vla-SetCellTextHeight table_obj 1 0 2.8)
	  (vla-SetCellAlignment table_obj 1 0 acMiddleCenter)
	  (vla-SetText table_obj 1 1 " Наимено-\n вание")
	  (vla-SetCellAlignment table_obj 1 1 acMiddleLeft)
	  (vla-SetCellTextHeight table_obj 1 1 2.4)
	  (vla-SetText table_obj 1 2 "X")
	  (vla-SetCellAlignment table_obj 1 2 acMiddleCenter)
	  (vla-SetCellTextHeight table_obj 1 2 2.8)
	  (vla-SetText table_obj 1 3 "Y")
	  (vla-SetCellAlignment table_obj 1 3 acMiddleCenter)
	  (vla-SetCellTextHeight table_obj 1 3 2.8)
	  (vla-SetText table_obj 1 4 "Длина\nлинии")
	  (vla-SetCellAlignment table_obj 1 4 acMiddleCenter)
	  (vla-SetCellTextHeight table_obj 1 4 2.8)
	  (vla-SetRowHeight table_obj 1 12)
;;;	  (vlax-dump-object table_obj T)
   (setq num_row 2)
    (vla-InsertRows table_obj (1+ num_row) 8 1)
    (vla-SetText table_obj num_row 0 (rtos 1.0 2 0))
    (vla-SetCellAlignment table_obj 0 0 acMiddleCenter)
    (vla-SetText table_obj num_row 1 "описание")
    (vla-SetCellAlignment table_obj num_row 1 acMiddleLeft)
    (vla-SetText table_obj num_row 2 (rtos 1000.0 2 2))
    (vla-SetCellAlignment table_obj num_row 2 acMiddleCenter)
    (vla-SetCellTextHeight table_obj num_row 2 2.8)
    (vla-SetText table_obj num_row 3 (rtos 2000.0 2 2))
    (vla-SetCellAlignment table_obj num_row 3 acMiddleCenter)
    (vla-SetCellTextHeight table_obj num_row 3 2.8)
(vla-SetRowHeight table_obj num_row 12)
(vla-EndUndoMark active_document)
   
   );_ end of defun
