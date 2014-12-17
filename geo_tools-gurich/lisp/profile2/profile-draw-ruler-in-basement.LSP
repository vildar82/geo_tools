(defun profile-draw-ruler-in-basement(block-profile-obj 3d-point-list /
				      block-obj curent-text-style h-text horizontal-scale
				      max-elevation mid-elevation min-elevation n
				      reference-level result-list scale-coefficient temp vertical-scale
				      elevation-buffer auto-reference-level
				      )
  (setq block-obj (cdr(assoc "block"(vlax-ldata-get block-profile-obj"obj-list"))))
  (setq vertical-scale (vlax-ldata-get block-profile-obj"vertical-scale"))
  (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
  (setq reference-level (vlax-ldata-get block-profile-obj"reference-level"))
  (setq auto-reference-level (vlax-ldata-get block-profile-obj"auto-reference-level"))
  (setq elevation-buffer (vlax-ldata-get block-profile-obj"elevation-buffer"))
  (setq scale-coefficient (* 1.0 (/ 1000.0 vertical-scale)))
  (setq profile-parameters(vlax-ldata-get block-profile-obj"profile-parameters"))
  (setq curent-text-style (nth 1 profile-parameters))
  (if(or(eq curent-text-style "***Текущий***")(not(member curent-text-style(get-text-style-list))))
    (setq curent-text-style(getvar "TEXTSTYLE")))
  (setq h-text (atof(nth 2 profile-parameters)))
  
  (setq min-elevation (apply 'min (mapcar 'caddr 3d-point-list)))
  (setq max-elevation (apply 'max (mapcar 'caddr 3d-point-list)))
  (setq mid-elevation (+(fix (/ (- max-elevation min-elevation)2))(fix min-elevation)))
  
  (if (and(not reference-level)(eq auto-reference-level 1))
    (progn
      (setq reference-level(-(fix min-elevation) elevation-buffer))
      (vlax-ldata-put block-profile-obj "reference-level" reference-level)
      (vla-put-textstring(cdr(assoc "reference-level"(vlax-ldata-get block-profile-obj"obj-list")))(strcat(nth 5 profile-parameters)" "(rtos reference-level 2 2)"м"))
      )
    )
  (if(not(tblobjname "block" "линейка"))(command "_insert" "линейка"(command)))
  (setq n 0)
  (repeat
    (fix(/(- (+ 5 max-elevation) reference-level)2))
    (setq temp(vla-insertblock block-obj (vlax-3D-point (list 0 (* (* 2 scale-coefficient) n))) "линейка" 1.0 scale-coefficient 1.0 0))
    (vla-put-Color temp 0)
    (vla-put-Layer temp "0")
    (setq result-list(append result-list (list temp)))
    (setq n (1+ n))
    )
  (setq result-list
	 (append result-list
		 (list
		   (setq temp
			  (draw-text-vla-by-list-property
			    (list(cons "spece" block-obj)
				 (cons "TextString" (rtos mid-elevation 2 2))
				 (cons "InsertionPoint" (list -2 (* (- mid-elevation reference-level) scale-coefficient)))
				 (cons "Height" h-text)
				 (cons "Rotation" 0)
				 (cons "Color" 0)
				 (cons "Alignment" 14)
				 (cons "StyleName" curent-text-style)
				 (cons "Layer" "0")
				 ))
			 )
		   )))
  (setq result-list
	 (append result-list
		 (list
		   (draw-line-vla-by-list-property
		     (list
		       (cons "spece" block-obj)
		       (cons "StartPoint" (list -1 (* (- mid-elevation reference-level) scale-coefficient)))
		       (cons "EndPoint" (list (- -3 (calculation_text_height_be_object temp))
					      (* (- mid-elevation reference-level) scale-coefficient)))
		       (cons "Linetype" "Continuous")
		       (cons "Color" 0)
		       (cons "Layer" "0")
		       ))
		   )))
  (vla-update block-profile-obj)
  result-list
  )