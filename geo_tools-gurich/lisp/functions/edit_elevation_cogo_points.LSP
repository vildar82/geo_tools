(defun C:edit_elevation_cogo_points ( / elev point-obj ssnab)
  ; изменение отметки выбранных COGO точек на указаное превышение
  (load_global_variable)
  (if
    C3D_active_document
    (progn
      (if (ssget "X" '((0 . "AECC_COGO_POINT")))
	(progn
	  (setq ssnab (ssget "_I"))
	  (if ssnab
	    (progn
	      (sssetfirst)
	      (if (not(convert-nabor-to-list-cogo-only ssnab))
		(setq ssnab(ssget '((0 . "AECC_COGO_POINT"))))
		)
	      )
	    (setq ssnab(ssget '((0 . "AECC_COGO_POINT"))))
	    )
	  (if ssnab
	    (progn
	      (setq elev (getreal "\nИзменение отметки: "))
	      (if (and elev (/=  elev 0))
		(progn
		  (setq n-error 0)
		  (vla-StartUndoMark active_document)
		  (foreach x (convert-nabor-to-list-cogo-only ssnab)
		    (setq point-obj (vlax-ename->vla-object x))
		    (setq z(fun-error-edit-cogo 'vlax-get (list point-obj 'elevation)))
		    (if z
		      (progn
			(if (= z -1.0e+020) (setq z 0.0))
			(setq z(+ z elev))
			(if (not(fun-error-edit-cogo 'vlax-put (list point-obj 'elevation z)))
			  (setq n-error (1+ n-error))
			  )
			)
		      (setq n-error (1+ n-error))
		      )
		    (vla-EndUndoMark active_document)
		    )
		  (if (> n-error 0)
		      (alert (strcat "Внимание! " (itoa n-error) " точек COGO не удалось изменить отметку!")))
		  )
		)
	      )
	    )
	  )
	(alert "Операция не возможна!\nТекущий чертеж не содержит точек COGO.")
	)
      )
    (alert "Выполнение комманды предполагает ее запуск в AutoCAD Civil 3D!\nТекущий AutoCAD им не является или представляет собой не поддерживаемую(временно?) версию.")
    )
  (princ)
  )


(defun convert-nabor-to-list-cogo-only (nabor / element n result)
  (setq n 0)
  (if nabor
    (progn
      (repeat (sslength nabor)
	(setq element (ssname nabor n))
	(if (= (cdr (assoc 0 (entget element)))"AECC_COGO_POINT")
	  (setq result (append result (list element)))
	  )
	(setq n (1+ n)))
      )
    )
  result
  )

(defun fun-error-edit-cogo (func lst / trap)
  (cond
    ((vl-catch-all-error-p
       (setq trap (vl-catch-all-apply func lst))
       )
     (setq trap "erorr")
     )
    )
  (cond
    ((= trap "erorr")
     nil)
    ((= trap nil)
     T)
    (T
     trap)
    )
  )