(defun calculation_str_long_be_h_stile (stroka ht styl / ); вычисляет длинну указаного текста
  (calculation_text_height_be_dxf_code (list '(0 . "TEXT") (cons 40 ht) (cons 1 stroka) (cons 7 styl)))
  )
