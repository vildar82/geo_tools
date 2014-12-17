(defun calculation_coord_centre_text (ttt / tb dtx dty); вычисляет координаты центральной точки текстового блока
  (setq tb (textbox ttt))
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
  (polar (polar (cdr (assoc 10 ttt)) (cdr (assoc 50 ttt)) dtx)  (+ (cdr (assoc 50 ttt)) (/ pi 2)) dty)
  )
