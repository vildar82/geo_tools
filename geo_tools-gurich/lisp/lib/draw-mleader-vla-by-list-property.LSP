(defun draw-mleader-vla-by-list-property(input-list / mleader leaderLineIndex)
  ;;;  создание примитива mleader средствами ActiveX
  ;|
  (draw-mleader-vla-by-list-property
    (list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	 (cons "LeaderLineCoor" (list '(0 0 0)'(10 10 0)))
	 (cons "StyleName" "Подписи")
	 (cons "TextHeight" 3.5)
	 (cons "TextString" "123.123")
	 (cons "ArrowheadType" 9)
	 (cons "ArrowheadSize" (/ 2.0 (/ 1000.0 vertical-scale)))
	 (cons "ArrowheadBlock" "_Open90")
	 ))
	 |;
  (if (not(setq leaderLineIndex(cdr(assoc "leaderLineIndex" input-list))))(setq leaderLineIndex 0))
  (setvar "CMLEADERSTYLE" (cdr(assoc "StyleName" input-list)))
  (setq mleader (vla-addmleader (cdr(assoc "spece" input-list))
		  (convert_coords_list_from_safearray_mleader(cdr(assoc "LeaderLineCoor" input-list)))leaderLineIndex))
  (foreach x input-list
    (if (vlax-property-available-p mleader (car x) T)
      (if (not(or(eq(car x)"spece")
		 (eq(car x)"LeaderLineCoor")
		 (eq(car x)"leaderLineIndex")
		 ))
	(eval(list(read(strcat "vla-put-" (car x)))mleader(cdr x)))
	)
      )
    )
  mleader
  )


