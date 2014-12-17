(defun find-multileader-in-list-be-point (input-list input-point / flag n point result)
  ; ищет мультивыноску в списке с "указателем" в указанную точку
  (setq input-point (list (car input-point)(cadr input-point)))
  (setq n 0)
  (while (not(or(> n(1-(length input-list)))
		(eq flag T)
		))
    (if (> (vla-get-LeaderCount (vlax-ename->vla-object(nth n input-list))) 0)
      (progn
	(setq point (vlax-safearray->list(vlax-variant-value(vla-GetLeaderLineVertices (vlax-ename->vla-object (nth n input-list)) 0))))
	(if (equal (list (car point)(cadr point)) input-point)
	  (progn
	    (setq result (nth n input-list))
	    (setq flag T)
	    )
	  (progn
	    (setq n (1+ n))
	    (setq result nil)
	    )
	  )
	)
      (setq n (1+ n))
      )
    )
  result
  )