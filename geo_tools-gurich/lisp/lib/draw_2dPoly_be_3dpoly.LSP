(defun draw_2dPoly_be_3dpoly ( 3Dpoly / 3Dspisokv 2Dspisokv)
  (setq 3Dspisokv (extract_coord_polyline 3Dpoly))
  (setq 2Dspisokv (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(6 . "Continuous") '(100 . "AcDbPolyline")(cons 90 (length 3Dspisokv)) '(70 . 0) (cons 43 0)))
  (mapcar '(lambda (x)
	     (setq 2Dspisokv (append 2Dspisokv (list (cons 10 (list (car x)(cadr x))))))
	     )
	  3Dspisokv)
  (entmake 2Dspisokv)
  (vlax-ename->vla-object (entlast))
  )
