(defun calculation_text_height_be_object (obj / tb); возвращает высоту текстового блока требуется объект
  (if (eq (type obj) 'VLA-OBJECT)
    (setq obj (vlax-vla-object->ename obj)))
  (setq tb (textbox (entget obj)))
                  (- (caadr tb); x2
		     (caar tb); x1
		     ); +
 ); defun calculation_text_height_be_object
