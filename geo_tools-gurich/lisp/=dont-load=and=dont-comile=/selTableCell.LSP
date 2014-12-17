;; Begin code here 
;; Written by Lee Ambrosius 
;; Date: 3/24/04 
;; Checks for tables in current view to match up with HitTest 
;; This avoids the need for the user to select the table first. 

;; Program is provided AS-IS with no expressed written warranty 
;; This example demonstrates a couple of the new Table ActiveX properties 
;; HitTest, GetCellType, GetText and SetText 

(defun c:selTableCell ( / vector table pick row col cellValueOrg)
  (vl-load-com)
  (setq vHeight (getvar "viewsize"))
  (setq vWidth (* (/ (nth 0 (getvar "screensize"))(nth 1 (getvar "screensize")))vHeight))
  
  (setq lwrLeft (list (- (nth 0 (getvar "viewctr"))(/ vWidth 2))(- (nth 1 (getvar "viewctr"))(/ vHeight 2))0))
  (setq uprRight (list (+ (nth 0 (getvar "viewctr"))(/ vWidth 2))(+ (nth 1 (getvar "viewctr"))(/ vHeight 2))0))
  
  (setq vector (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  (vlax-safearray-fill vector '(1 1 1))
  (setq vector (vlax-make-variant vector))
  
  (prompt "\nSelect Cell to edit: ")
  (setq pick (vlax-3d-point (getpoint)))
  
  (if (/= pick nil)
    (progn
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
		    ;; Let's get the value out
		    (setq cellValueOrg (vlax-invoke-method table 'GetText row col))
		    
		    ;; Change the current value
		    (vlax-invoke-method table 'SetText row col (strcat (itoa col) " " (itoa row)))
		    (vlax-invoke-method table 'Update)
		    (alert "Cell text was changed")
		    
		    ;; Restore the original value
		    (vlax-invoke-method table 'SetText row col cellValueOrg)
		    (vlax-invoke-method table 'Update)
		    (alert "Cell text was changed back to the original value.")
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
  (princ)
  )
;; End Code here