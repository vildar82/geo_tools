(defun calculation_text_height_be_dxf_code (text / tb); возвращает высоту текстового блока требуется список с dxf кодами
  (setq tb (textbox text))
                  (- (caadr tb); x2
		     (caar tb); x1
		     ); +
 ); defun calculation_text_height_be_dxf_code
