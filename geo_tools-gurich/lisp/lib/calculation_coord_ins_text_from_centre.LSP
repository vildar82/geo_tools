(defun calculation_coord_ins_text_from_centre (t1 a text / tb dtx dty); вычисляет координаты точки вставки текста от центральной точки текстового блока
  (setq tb (textbox text))
  (setq dtx (+ (/ (- (caadr tb); x2
		     (caar tb); x1
		     ); -
		  2); /
	       (caar tb); x1
	       );+
	);setq dtx
  (setq dty (- (/ (+ (cadadr tb); y2
                     (cadar tb); y1
		     ); +
		  2); /
	       (cadar tb)
	       ); -
	); setq dty
  (polar (polar t1 (- a (/ pi 2)) dty) (+ a pi) dtx)
  )
