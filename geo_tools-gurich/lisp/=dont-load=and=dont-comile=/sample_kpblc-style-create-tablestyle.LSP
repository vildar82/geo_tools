;|============================================================================= 
 ******** Пример *************************************************************** 
 *    Функция создает стиль таблиц. Этот стиль используется для всех таблиц. 
 *    Параметры вызова: 
 *   нет 
 *    Примеры вызова: 
 (_kpblc-style-create-tablestyle)
 =============================================================================|; 
 (defun sample_kpblc-style-create-tablestyle (/ item *adoc*)
   (setq 
     item (vla-addobject 
       (vla-item (vla-get-dictionaries 
              (vla-get-activedocument (vlax-get-acad-object))
              );_ end of vla-get-dictionaries 
            "acad_tablestyle" 
            );_ end of vla-item 
       "SPDS" 
       "AcDbTableStyle" 
       );_ end of vla-AddObject 
     );_ end of setq 
   (vla-put-name item "SPDS")     ; имя стиля 
   (vla-put-description item "Штампы СПДС"); описание стиля 
   (vla-put-flowdirection item 0)  ; направление (?)
   (vla-put-horzcellmargin item 0.5)  ; гориз.отступ текста от границы ячейки 
   (vla-put-vertcellmargin item 0.5)  ; верт.отступ текста от границы ячейки 
   (vla-settextheight 
     item 
     acdatarow 
     2.5 
     )              ; высота текста по умолчанию для datacells 
   (vla-settextheight 
     item 
     acheaderrow 
     3.5 
     )              ; высота текста для шапки таблицы 
   (vla-settextheight 
     item 
     actitlerow 
     7 
     )              ; высота текста для заголовка таблицы 
   (vla-settextstyle 
     item 
     (+ acdatarow acheaderrow actitlerow)
     "standard" 
     )              ; стиль текста по умолчанию для всех ячеек 
   (vla-put-colorindex 
     (vla-getbackgroundcolor 
       item 
       (+ acdatarow acheaderrow actitlerow)
       );_ end of vla-GetBackgroundColor 
     256 
     )              ; установка цвета "заднего фона" 
   ;;(vla-setbackgroundcolor item 3 1)
   (vla-setbackgroundcolornone item 7 :vlax-true)
   (vla-put-headersuppressed item :vlax-false)
   (vla-put-titlesuppressed item :vlax-false)
   (vla-setgridvisibility 
     item 
     (+ achorzinside acvertinside achorztop achorzbottom acvertleft acvertright)
     (+ acdatarow acheaderrow)
     :vlax-true 
     );_ end of vla-SetGridVisibility 
   (vla-setgridvisibility 
     item 
     (+ achorzinside acvertinside achorztop achorzbottom acvertleft acvertright)
     actitlerow 
     :vlax-false 
     );_ end of vla-SetGridVisibility 
   (vla-setgridlineweight 
     item 
     (+ achorzinside acvertinside)
     (+ acdatarow acheaderrow actitlerow)
     aclnwt025 
     );_ end of vla-SetGridLineWeight 
   (vla-setgridlineweight 
     item 
     (+ achorztop achorzbottom acvertright acvertleft)
     (+ acdatarow acheaderrow actitlerow)
     aclnwt050 
     );_ end of vla-SetGridLineWeight 
   );_ end of defun