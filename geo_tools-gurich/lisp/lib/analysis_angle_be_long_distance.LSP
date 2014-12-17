(defun analysis_angle_be_long_distance (longa / long)
  (if (not (or
	     (= longa (/ pi 2))
	     (= longa 0)
	     (and
	       (< 0 longa)
	       (< longa (/ pi 2))
	       )
	     (and
	       (< (* pi 1.5) longa)
	       (< longa (* pi 2))
	       ); and
	     ); or
	   ); конец not и условия
    (setq long (if
		 (< (+ longa pi) (* pi 2))
		 (+ longa pi)
		 (- longa pi)
		 ); if
		); setq
    (setq long longa)
    ); if
  long
  )
