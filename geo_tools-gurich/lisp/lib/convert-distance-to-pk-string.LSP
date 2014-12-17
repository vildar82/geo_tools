(defun convert-distance-to-pk-string (input-distance accuracy flag-cut-0 / )
  ; выражение длинны в виде строки пикета с заданной точностью
  ; 159.189 -> "1+59.19"
  (setq piket (fix(/ input-distance 100.0)))
  (setq metrs (atof(rtos(rem input-distance 100.0)2 accuracy)))
  (if (or (> metrs 100.0) (= metrs 100.0))
    (progn
      (setq piket (1+ piket))
      (setq metrs (- metrs 100.0))
      )
    )
  (strcat
    (itoa piket)
    (if (and flag-cut-0(not(> (abs metrs) 0.0)))
      ""
      (strcat
	"+"
	(if (< metrs 10.0)"0" "")
	(rtos metrs 2 accuracy)
	)
      
      )
    )
  )