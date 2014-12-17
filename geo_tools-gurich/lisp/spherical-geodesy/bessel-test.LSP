(defun C:probka (/ A-BESSEL A-WGS-84 B B-BESSEL B-WGS-84 L POINT-BL POINT-XYZ TRANSFORM-POINT-XYZ a-sk42 b-sk42)
  (setq a-bessel 6377397.155)
  (setq b-bessel 6356078.963)
  (setq a-wgs-84 6378137.000)
  (setq b-wgs-84 6356752.3142)
  (setq a-sk42 6378245.000)
  (setq b-sk42 6356863.019)
  (setq B (+ 55.0 (/ 40.0 60.0)))
  (setq L (+ 37.0 (/ 30.0 60.0)))
  (setq point-xyz(calculation-XYZ-by-BLH (list B L) a-bessel b-bessel))
  (setq transform-point-xyz; с форума http://gis-lab.info/forum/viewtopic.php?t=9386     +towgs84=396,165,557.7,-0.05,0.04,0.01,0 +no_defs ПОХОЖЕ!!!
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
;;;  (setq transform-point-xyz; с форума http://gis-lab.info/forum/viewtopic.php?f=34&t=7063     40.187 -110.741 -123.39 1.41378 -0.78147 0.7348 0.6121
;;;	 (transform-xyz-be-7-property
;;;	   40.187
;;;	   -110.741
;;;	   -123.39
;;;	   (convert-grad-rad(/ 1.41378 3600.0))
;;;	   (convert-grad-rad(/ -0.78147 3600.0))
;;;	   (convert-grad-rad(/ 0.7348 3600.0))
;;;	   (/ 0.6121 1000000.0)
;;;	   (nth 0 point-xyz)
;;;	   (nth 1 point-xyz)
;;;	   (nth 2 point-xyz)
;;;	   )
;;;	)
;;;  (setq transform-point-xyz; типо по почте из МГГТ
;;;	 (transform-xyz-be-7-property
;;;	   316.151
;;;	   78.924
;;;	   589.65
;;;	   (convert-grad-rad(/ 1.57273 3600.0))
;;;	   (convert-grad-rad(/ -2.69209 3600.0))
;;;	   (convert-grad-rad(/ -2.34693 3600.0))
;;;	   (/ 8.4507 1000000.0)
;;;	   (nth 0 point-xyz)
;;;	   (nth 1 point-xyz)
;;;	   (nth 2 point-xyz)
;;;	   )
;;;	)
;;;  (setq transform-point-xyz; эстонские параметры
;;;	 (transform-xyz-be-7-property
;;;	   374.0
;;;	   150.0
;;;	   588.0
;;;	   0.0
;;;	   0.0
;;;	   0.0
;;;	   0.0
;;;	   (nth 0 point-xyz)
;;;	   (nth 1 point-xyz)
;;;	   (nth 2 point-xyz)
;;;	   )
;;;	)
;;;  (setq transform-point-xyz; вторая трансформация если первая была на красовский
;;;	 (transform-xyz-be-7-property
;;;	   23.92
;;;	   -141.27
;;;	   -80.9
;;;	   0.0
;;;	   (convert-grad-rad(/ 0.35 3600.0))
;;;	   (convert-grad-rad(/ 0.82 3600.0))
;;;	   (/ -0.12 1000000.0)
;;;	   (nth 0 transform-point-xyz)
;;;	   (nth 1 transform-point-xyz)
;;;	   (nth 2 transform-point-xyz)
;;;	   )
;;;	)
;;;  (setq transform-point-xyz ; вторая трансформация если первая была на ПЗ-90
;;;	 (transform-xyz-be-7-property
;;;	   -1.10
;;;	   -0.30
;;;	   -0.90
;;;	   0.0
;;;	   0.0
;;;	   (convert-grad-rad(/ -0.20 3600.0))
;;;	   (/ -0.12 1000000.0)
;;;	   (nth 0 transform-point-xyz)
;;;	   (nth 1 transform-point-xyz)
;;;	   (nth 2 transform-point-xyz)
;;;	   )
;;;	)
  (setq point-BL(calculation-BLH-by-XYZ transform-point-xyz a-wgs-84 b-wgs-84))
  (terpri)(princ (rtos (nth 0 point-BL) 2 16))
  (terpri)(princ (rtos (nth 1 point-BL) 2 16))
  (princ)
  )

;;;(defun C:probka1 (/ A-BESSEL A-WGS-84 B B-BESSEL B-WGS-84 L POINT-BL POINT-XYZ TRANSFORM-POINT-XYZ)
;;;  (setq a-bessel 6377397.155)
;;;  (setq b-bessel 6356078.963)
;;;  (setq a-wgs-84 6378137.000)
;;;  (setq b-wgs-84 6356752.3142)
;;;  (setq B (+ 50.0 (/ 7.0 60.0)(/ 40.97 3600.0)))
;;;  (setq L (+ 23.0 (/ 45.0 60.0)(/ 13.43 3600.0)))
;;;  
;;;  (terpri)(princ (calculation_dec_to_gms B ))
;;;  (terpri)(princ (calculation_dec_to_gms L ))
;;;  
;;;  (setq point-xyz(calculation-XYZ-by-BLH (list B L) a-bessel b-bessel))
;;;  (setq transform-point-xyz
;;;	 (transform-xyz-be-7-property
;;;	   1.0
;;;	   1.0
;;;	   1.0
;;;	   0.0
;;;	   0.0
;;;	   0.0
;;;	   0.0
;;;	   (nth 0 point-xyz)
;;;	   (nth 1 point-xyz)
;;;	   (nth 2 point-xyz)
;;;	   )
;;;	)
;;;  (setq point-BL(calculation-BLH-by-XYZ transform-point-xyz a-bessel b-bessel))
;;;  (terpri)(princ (calculation_dec_to_gms (nth 0 point-BL) ))
;;;  (terpri)(princ (calculation_dec_to_gms (nth 1 point-BL) ))
;;;  (princ)
;;;  )