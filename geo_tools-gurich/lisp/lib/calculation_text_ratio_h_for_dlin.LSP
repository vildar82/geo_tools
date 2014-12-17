(defun calculation_text_ratio_h_for_dlin (stroka ht styl / ); вычисляет отношение высоты к длинне текста
 (/ ht(calculation_text_height_be_dxf_code (list '(0 . "TEXT") (cons 40 ht) (cons 1 stroka) (cons 7 styl))))
  )
