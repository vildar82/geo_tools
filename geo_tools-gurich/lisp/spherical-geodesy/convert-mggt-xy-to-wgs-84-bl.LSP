(defun convert-mggt-xy-to-wgs-84-bl (x y / a-bessel a-wgs-84 b-bessel b-wgs-84 bl l0 point-bl point-xyz popravka-x transform-point-xyz)
  (setq popravka-x 6170810.069)
  (setq a-bessel 6377397.155)
  (setq b-bessel 6356078.963)
  (setq a-wgs-84 6378137.000)
  (setq b-wgs-84 6356752.3142)
  (setq L0 (convert-grad-rad (+ 37.0 (/ 30.0 60.0))))
  (setq x (+ x popravka-x))
  (setq bl (calculation-xy-gaus-to-bl x y L0 a-bessel b-bessel))
  (setq point-xyz(calculation-XYZ-by-BLH bl a-bessel b-bessel))
  (setq transform-point-xyz
	 (transform-xyz-be-7-property
	   396.0
	   165.0
	   557.7
	   (convert-grad-rad(/ -0.05 3600.0))
	   (convert-grad-rad(/ 0.04 3600.0))
	   (convert-grad-rad(/ 0.01 3600.0))
	   0.0
	   (nth 0 point-xyz)
	   (nth 1 point-xyz)
	   (nth 2 point-xyz)
	   )
	)
  (setq point-BL(calculation-BLH-by-XYZ transform-point-xyz a-wgs-84 b-wgs-84))
  (list (nth 0 point-BL)
	(nth 1 point-BL))
  )