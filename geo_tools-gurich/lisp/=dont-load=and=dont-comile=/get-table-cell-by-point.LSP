(defun get-table-cell-by-point (pick / vHeight vWidth lwrLeft uprRight vector SS_TABLES cnt eMax table row col result)
  
  (setq vHeight (getvar "viewsize"))
  (setq vWidth (* (/ (nth 0 (getvar "screensize"))(nth 1 (getvar "screensize")))vHeight))
  
  (setq lwrLeft (list (- (nth 0 (getvar "viewctr"))(/ vWidth 2))(- (nth 1 (getvar "viewctr"))(/ vHeight 2))0))
  (setq uprRight (list (+ (nth 0 (getvar "viewctr"))(/ vWidth 2))(+ (nth 1 (getvar "viewctr"))(/ vHeight 2))0))
  
  (setq vector (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  (vlax-safearray-fill vector '(1 1 1))
  (setq vector (vlax-make-variant vector))
  (if (/= pick nil)
    (progn
      (setq pick (vlax-3d-point pick))
      (if (setq SS_TABLES (ssget "_C" lwrleft uprright (list (cons 0 "ACAD_TABLE"))))
	(progn
	  (setq cnt 0 eMax (sslength SS_TABLES))
	  
	  (while (> eMax cnt)
	    
	    (setq table (vlax-ename->vla-object (ssname SS_TABLES cnt)))
	    
	    ;; Return values for what cell was picked in
	    (setq row 0 col 0)
	    
	    ;; Check to see if a valid cell was picked
	    (if (= (vla-hittest table pick vector 'row 'col):vlax-true)
	      (progn
		;; Check to see what the Cell Type is (Text or Block)
		(if (= (vlax-invoke-method table 'GetCellType row col)acTextCell)
		  (progn
		    (setq result(list (list "table" table)
				      (list "column" col)
				      (list "row" row)))
		    (setq cnt eMax)
		    )
		  )
		)
	      )
	    (setq cnt (1+ cnt))
	    )
	  )
	)
      )
    )
  result
  )