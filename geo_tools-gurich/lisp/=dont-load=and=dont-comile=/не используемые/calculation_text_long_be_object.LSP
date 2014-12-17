(defun calculation_text_long_be_object (obj / text tb); возвращает ширину текстового блока требуется объект
  (if (eq (type obj) 'VLA-OBJECT)
    (setq obj (vlax-vla-object->ename obj)))
  (setq text (entget obj))
  (setq tb (textbox text))
  (+ (abs (cadadr tb)); y2
     (abs (cadar tb)); y1
     ); +
  
  ); defun calculation_text_long_be_object
