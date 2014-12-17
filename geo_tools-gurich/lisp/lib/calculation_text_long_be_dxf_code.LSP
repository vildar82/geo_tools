(defun calculation_text_long_be_dxf_code (text / tb); возвращает ширину текстового блока требуется список с dxf кодами
    (setq tb (textbox text))
                  (+ (abs (cadadr tb)); y2
                     (abs (cadar tb)); y1
		     ); +
  )
