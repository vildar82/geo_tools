(defun create-table-style (/ item)
  ; создает стиль таблиц каталога координат
  (if(vl-catch-all-error-p
       (vl-catch-all-apply 'vla-item
	 (list (vla-item (vla-get-dictionaries
			   (vla-get-activedocument (vlax-get-acad-object))
			   )"acad_tablestyle") " оординаты"))); провер€ем наличие отсутстви€ стил€ таблиц
    (progn
;;;      (create-text-style "OTI" "ESKDU.shx")
      (setq
	item (vla-addobject
	       (vla-item (vla-get-dictionaries
			   (vla-get-activedocument (vlax-get-acad-object))
			   );_ end of vla-get-dictionaries
			 "acad_tablestyle"
			 );_ end of vla-item
	       " оординаты"
	       "AcDbTableStyle"
	       );_ end of vla-AddObject
	);_ end of setq
      (vla-put-name item " оординаты"); им€ стил€
      (vla-put-description item "“аблица каталога координат"); описание стил€
      (vla-put-flowdirection item 0); направление (?)
      (vla-put-horzcellmargin item 0.5); гориз.отступ текста от границы €чейки
      (vla-put-vertcellmargin item 0.5); верт.отступ текста от границы €чейки
      (vla-settextheight
	item
	acdatarow
	2.0
	); высота текста по умолчанию дл€ datacells
      (vla-settextheight
	item
	acheaderrow
	2.0
	); высота текста дл€ шапки таблицы
      (vla-settextheight
	item
	actitlerow
	2.0
	); высота текста дл€ заголовка таблицы
      (vla-settextstyle
	item
	(+ acdatarow acheaderrow actitlerow)
	(getvar "TEXTSTYLE")
	); стиль текста по умолчанию дл€ всех €чеек
      (vla-setgridvisibility
	item
	(+ achorztop acvertleft acvertright)
	actitlerow
	:vlax-false
	);_ end of vla-SetGridVisibility отключаем рамку вокруг названи€
      (vla-setgridvisibility
	item
	(+ achorzinside acvertinside achorztop achorzbottom acvertleft acvertright)
	(+ acdatarow acheaderrow)
	:vlax-true
	);_ end of vla-SetGridVisibility включаем рамки вокруг всего остального (не знаю зачем:))
      T
      )
    nil
    )
  );_ end of defun