(defun C:proba ( / elevation-3dpoly-obj)
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  (while (not elevation-3dpoly-obj)
    (setq elevation-3dpoly-obj(car (entsel "\nВыбери трехмерную полилинию<Выход>: ")))
    (if elevation-3dpoly-obj
      (if (not(eq (cdr (assoc 0 (entget elevation-3dpoly-obj))) "POLYLINE"))
	(progn
	  (princ "\n Недопустимый выбор, целься лучше...")
	  (setq elevation-3dpoly-obj nil)
	  )
	)
      (princ "\n Недопустимый выбор, целься лучше...")
      )
    )
  (setq elevation-3dpoly-obj(vlax-ename->vla-object elevation-3dpoly-obj))
  (setq reaktor-elevation-3dpoly-obj (car(vlr-get-reactors-for-object elevation-3dpoly-obj)))
  (if reaktor-elevation-3dpoly-obj
    (progn
      (setq reaktor-text-3dpolyline-obj (car(vlr-get-reactors-for-object(car(vlr-data reaktor-elevation-3dpoly-obj)))))
      (vlr-remove reaktor-text-3dpolyline-obj)
      (vlr-pers-release reaktor-text-3dpolyline-obj)
      (vlr-remove reaktor-elevation-3dpoly-obj)
      (vlr-pers-release reaktor-elevation-3dpoly-obj)
      (setq list-text-3dpolyline-obj(vlr-owners reaktor-text-3dpolyline-obj))
      (foreach x list-text-3dpolyline-obj(vlr-owner-remove reaktor-text-3dpolyline-obj x))
      (foreach x list-text-3dpolyline-obj(vla-delete x))
      (vlr-owner-remove reaktor-elevation-3dpoly-obj elevation-3dpoly-obj)
      (princ "\n Подписи удалены.")
      (princ "\n Связи разорваны.")
      )
    (progn
      (setq list-text-3dpolyline-obj (draw-reactive-profile-text-3dpolyline-obj elevation-3dpoly-obj))
      (princ "\n Вершины подписаны.")
      (vlr-pers(vlr-object-reactor(list elevation-3dpoly-obj)list-text-3dpolyline-obj(list '(:vlr-objectClosed . fun-reaktor-elevation-3dpoly-obj))))
      (vlr-pers(vlr-object-reactor list-text-3dpolyline-obj elevation-3dpoly-obj(list '(:vlr-objectClosed . fun-reaktor-text-3dpolyline-obj))))
      (if (eq (vlr-reactors :VLR-Editor-Reactor)nil)
	(vlr-pers(vlr-editor-reactor nil '((:vlr-commandended . fun-commandended-for-edit-text-by-real-number))))
	)
      (princ "\n Связи установлены.")
      )
    )
  (princ)
  )



(defun fun-reaktor-elevation-3dpoly-obj (obj-run-reactor current-reactor arguments /
					 list-text-3dpolyline-obj
					 reaktor-text-3dpolyline-obj
					 )
  (princ "\n RUN fun-reaktor-elevation-3dpoly-obj")
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  
  (if flag-reaktor-elevation-3dpoly-obj
    (progn
      (princ "\n flag-reaktor-elevation-3dpoly-obj=YES")
      (setq flag-reaktor-elevation-3dpoly-obj nil)
      )
    (progn
      (princ "\n flag-reaktor-elevation-3dpoly-obj=NO")
      (setq list-text-3dpolyline-obj(vlr-data current-reactor))
      (setq reaktor-text-3dpolyline-obj (car(vlr-get-reactors-for-object (car list-text-3dpolyline-obj))))
      (setq list-text-3dpolyline-obj(vlr-owners reaktor-text-3dpolyline-obj))
      (progn (strcat "\n length list-text-3dpolyline-obj="(itoa(length list-text-3dpolyline-obj))))
      (vlr-remove reaktor-text-3dpolyline-obj)
      (foreach x list-text-3dpolyline-obj(vlr-owner-remove reaktor-text-3dpolyline-obj x))
      (foreach x list-text-3dpolyline-obj(vla-delete x))
      (setq list-text-3dpolyline-obj nil)
      (setq list-text-3dpolyline-obj(draw-reactive-profile-text-3dpolyline-obj  obj-run-reactor))
      (foreach x list-text-3dpolyline-obj (setq flag-reaktor-text-3dpolyline-obj T)(vlr-owner-add reaktor-text-3dpolyline-obj x))
      (vlr-add reaktor-text-3dpolyline-obj)
      (vlr-data-set current-reactor list-text-3dpolyline-obj)
      )
    )
  )

(defun fun-reaktor-text-3dpolyline-obj (obj-run-reactor current-reactor arguments /
					elevation-3dpoly-obj reaktor-elevation-3dpoly-obj
					)
  (princ "\n RUN fun-reaktor-text-3dpolyline-obj")
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  (if flag-reaktor-text-3dpolyline-obj
    (progn
      (princ "\n flag-reaktor-text-3dpolyline-obj=YES")
      (setq flag-reaktor-text-3dpolyline-obj nil)
      )
    (progn
      (princ "\n flag-reaktor-text-3dpolyline-obj=NO")
      
      (setq elevation-3dpoly-obj (vlr-data current-reactor))
      (setq reaktor-elevation-3dpoly-obj (car(vlr-get-reactors-for-object elevation-3dpoly-obj)))
      
      (vlr-remove reaktor-elevation-3dpoly-obj)
      (vla-put-Coordinate
	elevation-3dpoly-obj
	(1-(length(member obj-run-reactor (vlr-owners current-reactor))))
	(vlax-safearray-fill (vlax-make-safearray vlax-vbDouble '(0 . 2))
	  (list (car(vlax-safearray->list(vlax-variant-value(vla-get-TextAlignmentPoint obj-run-reactor))))
		(cadr(vlax-safearray->list(vlax-variant-value(vla-get-TextAlignmentPoint obj-run-reactor))))
		(atof(edit_text_clean_for_number(vla-get-TextString obj-run-reactor))))
	  )
	)
      (if flag-for-comm-fun-edit-text-by-real-number
	(if (eq (type flag-for-comm-fun-edit-text-by-real-number) 'LIST)
	  (setq flag-for-comm-fun-edit-text-by-real-number (append flag-for-comm-fun-edit-text-by-real-number(list obj-run-reactor)))
	  (setq flag-for-comm-fun-edit-text-by-real-number (append (list flag-for-comm-fun-edit-text-by-real-number)(list obj-run-reactor)))
	  )
	(setq flag-for-comm-fun-edit-text-by-real-number obj-run-reactor)
	)
      (setq flag-reaktor-elevation-3dpoly-obj T)
      (vlr-add reaktor-elevation-3dpoly-obj)
      )
    )
  
  )




(defun C:proba ( /
		angle-list-obj axis-trace-obj elevation-3dpoly-obj horizon-elevation horizon-scale
		list-text-3dpolyline-obj lwpolyline-obj profile-line-obj profile-main-obj
		reaktor-axis-trace reaktor-elevation-3dpoly-obj reaktor-profile-main-obj reaktor-text-3dpolyline-obj
		start-profile-point vertical-scale
		)
  
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  (setq axis-trace-obj (vlax-ename->vla-object (car (entsel "\nВыбери 2d ось: "))))
  (setq elevation-3dpoly-obj (vlax-ename->vla-object (car (entsel "\nВыбери 3d отметки: "))))
  (initget 1)(setq start-profile-point(getpoint "\nУкажи точку на условном горизонте: "))
  (setq horizon-elevation 100.000)
  (setq horizon-scale 1000.0)
  (setq vertical-scale 1000.0)
  (setq profile-main-obj (draw-reactive-profile-main-obj horizon-elevation start-profile-point))
  (setq profile-line-obj (draw-reactive-profile-line-obj start-profile-point axis-trace-obj))
  (setq angle-list-obj (draw-reactive-profile-angle-by-axis-trace axis-trace-obj start-profile-point))
  (setq lwpolyline-obj (vla-addLightWeightPolyline model_spece(draw-reactive-profile-lwpolyline-obj
								(analisis-3d-and-2d-poly-by-general-3dpoly axis-trace-obj elevation-3dpoly-obj)
								horizon-elevation start-profile-point horizon-scale vertical-scale)))
  (setq list-text-3dpolyline-obj (draw-reactive-profile-text-3dpolyline-obj elevation-3dpoly-obj))
  
  (setq list-text-lwpolyline-obj (draw-reactive-profile-text-lwpolyline-obj lwpolyline-obj start-profile-point horizon-elevation horizon-scale vertical-scale))
  (setq list-vertical-line-lwpolyline-obj (draw-reactive-profile-vertical-line-lwpolyline-obj lwpolyline-obj start-profile-point horizon-elevation horizon-scale vertical-scale))
  
  (setq reaktor-axis-trace (vlr-object-reactor(list axis-trace-obj)profile-main-obj(list '(:vlr-modified . fun-reaktor-axis-trace))))
  (setq reaktor-profile-main-obj (vlr-object-reactor(list profile-main-obj)nil(list '(:vlr-modified . fun-reaktor-profile-main-obj))))
  (setq reaktor-elevation-3dpoly-obj (vlr-object-reactor(list elevation-3dpoly-obj)profile-main-obj(list '(:vlr-objectClosed . fun-reaktor-elevation-3dpoly-obj))))
  (setq reaktor-text-3dpolyline-obj (vlr-object-reactor list-text-3dpolyline-obj profile-main-obj(list '(:vlr-objectClosed . fun-reaktor-text-3dpolyline-obj))))
  ;;;  (vlr-pers
  ;;;    (vlr-editor-reactor nil '((:vlr-commandended . fun-commandended-for-edit-text-by-real-number)))
  ;;;    )
  
  (vlr-remove reaktor-profile-main-obj)
  (vlax-ldata-put profile-main-obj "horizon-elevation" horizon-elevation)
  (vlax-ldata-put profile-main-obj "horizon-scale" horizon-scale)
  (vlax-ldata-put profile-main-obj "vertical-scale" vertical-scale)
  (vlax-ldata-put profile-main-obj "start-profile-point" start-profile-point)
  (vlax-ldata-put profile-main-obj "axis-trace-obj" axis-trace-obj)
  (vlax-ldata-put profile-main-obj "elevation-3dpoly-obj" elevation-3dpoly-obj)
  (vlax-ldata-put profile-main-obj "profile-line-obj" profile-line-obj)
  (vlax-ldata-put profile-main-obj "angle-list-obj" angle-list-obj)
  (vlax-ldata-put profile-main-obj "lwpolyline-obj" lwpolyline-obj)
  (vlax-ldata-put profile-main-obj "list-text-lwpolyline-obj" list-text-lwpolyline-obj)
  (vlax-ldata-put profile-main-obj "list-vertical-line-lwpolyline-obj" list-vertical-line-lwpolyline-obj)
  (vlax-ldata-put profile-main-obj "list-text-3dpolyline-obj" list-text-3dpolyline-obj)
  ;;;  (vlax-ldata-put profile-main-obj "reaktor-elevation-3dpoly-obj" reaktor-elevation-3dpoly-obj)
  ;;;  (vlax-ldata-put profile-main-obj "reaktor-text-3dpolyline-obj" reaktor-text-3dpolyline-obj)
  (vlr-add reaktor-profile-main-obj)
  
  ;;;  (vlr-pers reaktor-axis-trace)
  ;;;  (vlr-pers reaktor-profile-main-obj)
  ;;;  (vlr-pers reaktor-elevation-3dpoly-obj)
  ;;;  (vlr-pers reaktor-text-3dpolyline-obj)
  
  (princ)
  )

(defun draw-reactive-profile-text-lwpolyline-obj (lwpolyline-obj start-profile-point horizon-elevation horizon-scale vertical-scale / list_text_obj spisok text_obj text_point text_str)
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  (setq spisok(list-coordinates-lwpolyline lwpolyline-obj))
  (foreach point spisok
    (setq text_point (list (nth 0 point) (-(nth 1 start-profile-point)6)))
    (setq text_str (rtos(+(-(nth 1 point)(nth 1 start-profile-point))horizon-elevation)2 3))
    (setq text_obj (vla-AddText model_spece text_str (vlax-3D-point text_point) 2.5))
    (vla-put-Alignment text_obj 13)
    (vla-put-TextAlignmentPoint text_obj (vlax-3D-point text_point))
    (vla-put-Rotation text_obj (/ pi 2))
    (vla-put-Linetype text_obj "Continuous")
    (setq list_text_obj (append list_text_obj (list text_obj)))
    )
  list_text_obj
  )

(defun draw-reactive-profile-vertical-line-lwpolyline-obj (lwpolyline-obj start-profile-point horizon-elevation horizon-scale vertical-scale / line_obj list_line_obj spisok)
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  (setq spisok(list-coordinates-lwpolyline lwpolyline-obj))
  (foreach point spisok
    (setq line_obj(vla-AddLine model_spece (vlax-3D-point point)(vlax-3D-point (list (nth 0 point) (nth 1 start-profile-point)))))
    (vla-put-Linetype line_obj "Continuous")
    (setq list_line_obj (append list_line_obj (list line_obj)))
    )
  list_line_obj
  )




(defun fun-commandended-for-edit-text-by-real-number (current-reactor arguments / )
  (princ "\n RUN fun-comm")
  (if flag-for-comm-fun-edit-text-by-real-number
    (if (eq (type flag-for-comm-fun-edit-text-by-real-number) 'LIST)
      (progn
	(princ "\n YES LIST")
	(foreach x flag-for-comm-fun-edit-text-by-real-number
	  (if(not(vlax-erased-p x))
	  (vla-put-TextString x
	    (rtos
	      (atof
		(edit_text_clean_for_number
		  (vla-get-TextString x))
		)
	      2 3))
	    )
	  )
	(setq flag-for-comm-fun-edit-text-by-real-number nil)
	)
      (progn
	(princ "\n YES")
	(if(not(vlax-erased-p flag-for-comm-fun-edit-text-by-real-number))
	(vla-put-TextString flag-for-comm-fun-edit-text-by-real-number
	  (rtos
	    (atof
	      (edit_text_clean_for_number
		(vla-get-TextString flag-for-comm-fun-edit-text-by-real-number))
	      )
	    2 3))
	  )
	(setq flag-for-comm-fun-edit-text-by-real-number nil))
      )
    (princ "\n NO")
    )
  )





(defun fun-reaktor-axis-trace (obj-run-reactor current-reactor arguments /
			       angle-list-obj profile-line-obj start-profile-point
			       list-vertical-line-lwpolyline-obj list-text-lwpolyline-obj
			       horizon-elevation horizon-scale lwpolyline-obj vertical-scale
			       elevation-3dpoly-obj)
  (princ "\n RUN fun-reaktor-axis-trace")
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  
  (setq profile-line-obj(vlax-ldata-get (vlr-data current-reactor) "profile-line-obj"))
  (setq start-profile-point(vlax-ldata-get (vlr-data current-reactor) "start-profile-point"))
  (setq angle-list-obj(vlax-ldata-get (vlr-data current-reactor) "angle-list-obj"))
  (setq list-text-lwpolyline-obj(vlax-ldata-get (vlr-data current-reactor) "list-text-lwpolyline-obj"))
  (setq list-vertical-line-lwpolyline-obj(vlax-ldata-get (vlr-data current-reactor) "list-vertical-line-lwpolyline-obj"))
  (setq horizon-elevation(vlax-ldata-get (vlr-data current-reactor) "horizon-elevation"))
  (setq horizon-scale(vlax-ldata-get (vlr-data current-reactor) "horizon-scale"))
  (setq vertical-scale(vlax-ldata-get (vlr-data current-reactor) "vertical-scale"))
  (setq elevation-3dpoly-obj(vlax-ldata-get (vlr-data current-reactor) "elevation-3dpoly-obj"))
  (setq lwpolyline-obj(vlax-ldata-get (vlr-data current-reactor) "lwpolyline-obj"))
  
  (vla-put-EndPoint profile-line-obj (vlax-3D-point (list (+(nth 0 start-profile-point)(vla-get-Length obj-run-reactor)) (nth 1 start-profile-point))))
  (vla-put-StartPoint profile-line-obj (vlax-3D-point start-profile-point))
  
  (foreach x angle-list-obj (if(not(vlax-erased-p x))(vla-delete x)))
  (setq angle-list-obj (draw-reactive-profile-angle-by-axis-trace obj-run-reactor start-profile-point))
  (vlax-ldata-put (vlr-data current-reactor) "angle-list-obj" angle-list-obj)
  
  (vla-put-coordinates
    lwpolyline-obj
    (draw-reactive-profile-lwpolyline-obj
      (analisis-3d-and-2d-poly-by-general-3dpoly
	obj-run-reactor
	elevation-3dpoly-obj)
      horizon-elevation
      start-profile-point
      horizon-scale
      vertical-scale))
  
  (foreach x list-text-lwpolyline-obj (if(not(vlax-erased-p x))(vla-delete x)))
  (foreach x list-vertical-line-lwpolyline-obj (if(not(vlax-erased-p x))(vla-delete x)))
  (setq list-text-lwpolyline-obj (draw-reactive-profile-text-lwpolyline-obj
				   lwpolyline-obj
				   start-profile-point
				   horizon-elevation
				   horizon-scale
				   vertical-scale))
  (setq list-vertical-line-lwpolyline-obj (draw-reactive-profile-vertical-line-lwpolyline-obj
					    lwpolyline-obj
					    start-profile-point
					    horizon-elevation
					    horizon-scale
					    vertical-scale))
  (vlax-ldata-put (vlr-data current-reactor) "list-text-lwpolyline-obj" list-text-lwpolyline-obj)
  (vlax-ldata-put (vlr-data current-reactor) "list-vertical-line-lwpolyline-obj" list-vertical-line-lwpolyline-obj)

  )







(defun fun-reaktor-profile-main-obj (obj-run-reactor current-reactor arguments /
				     profile-line-obj start-profile-point angle-list-obj axis-trace-obj
				     list-text-lwpolyline-obj list-vertical-line-lwpolyline-obj
				     elevation-3dpoly-obj horizon-elevation horizon-scale lwpolyline-obj vertical-scale)
  (princ "\n RUN fun-reaktor-profile-main-obj")
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  
  (setq axis-trace-obj(vlax-ldata-get obj-run-reactor "axis-trace-obj"))
  (setq profile-line-obj(vlax-ldata-get obj-run-reactor "profile-line-obj"))
  (setq start-profile-point (vlax-safearray->list(vlax-variant-value(vla-get-TextAlignmentPoint obj-run-reactor))))
  (setq horizon-elevation(atof(edit_text_clean_for_number(vla-get-TextString obj-run-reactor))))
  (setq horizon-scale(vlax-ldata-get obj-run-reactor "horizon-scale"))
  (setq vertical-scale(vlax-ldata-get obj-run-reactor "vertical-scale"))
  (setq elevation-3dpoly-obj(vlax-ldata-get obj-run-reactor "elevation-3dpoly-obj"))
  (setq lwpolyline-obj(vlax-ldata-get obj-run-reactor "lwpolyline-obj"))
  (setq list-text-lwpolyline-obj(vlax-ldata-get obj-run-reactor "list-text-lwpolyline-obj"))
  (setq list-vertical-line-lwpolyline-obj(vlax-ldata-get obj-run-reactor "list-vertical-line-lwpolyline-obj"))

  
  (vlax-ldata-put obj-run-reactor "horizon-elevation" horizon-elevation)
  (vlax-ldata-put obj-run-reactor "start-profile-point" start-profile-point)
  
  (vla-put-EndPoint profile-line-obj (vlax-3D-point (list (+(nth 0 start-profile-point)(vla-get-Length axis-trace-obj)) (nth 1 start-profile-point))))
  (vla-put-StartPoint profile-line-obj (vlax-3D-point start-profile-point))
  
  (setq angle-list-obj(vlax-ldata-get obj-run-reactor "angle-list-obj"))
  (foreach x angle-list-obj (if(not(vlax-erased-p x))(vla-delete x)))
  (setq angle-list-obj nil)
  (setq angle-list-obj (draw-reactive-profile-angle-by-axis-trace axis-trace-obj start-profile-point))
  (vlax-ldata-put obj-run-reactor "angle-list-obj" angle-list-obj)
  
  (vla-put-coordinates
    lwpolyline-obj
    (draw-reactive-profile-lwpolyline-obj
      (analisis-3d-and-2d-poly-by-general-3dpoly
	axis-trace-obj
	elevation-3dpoly-obj)
      horizon-elevation
      start-profile-point
      horizon-scale
      vertical-scale))
  (vla-Update lwpolyline-obj)
  
  (princ "\n list-text-lwpolyline-obj=")
  (princ list-text-lwpolyline-obj)
  (foreach x list-text-lwpolyline-obj (if(not(vlax-erased-p x))(vla-delete x)))
  (foreach x list-vertical-line-lwpolyline-obj (if(not(vlax-erased-p x))(vla-delete x)))
  
  (setq list-text-lwpolyline-obj (draw-reactive-profile-text-lwpolyline-obj
				   lwpolyline-obj
				   start-profile-point
				   horizon-elevation
				   horizon-scale
				   vertical-scale))
  (setq list-vertical-line-lwpolyline-obj (draw-reactive-profile-vertical-line-lwpolyline-obj
					    lwpolyline-obj
					    start-profile-point
					    horizon-elevation
					    horizon-scale
					    vertical-scale))
  
  (vlax-ldata-put obj-run-reactor "list-text-lwpolyline-obj" list-text-lwpolyline-obj)
  (vlax-ldata-put obj-run-reactor "list-vertical-line-lwpolyline-obj" list-vertical-line-lwpolyline-obj)
  
  )






(defun fun-reaktor-elevation-3dpoly-obj (obj-run-reactor current-reactor arguments /
					 list-text-3dpolyline-obj reaktor-text-3dpolyline-obj
					 axis-trace-obj elevation-3dpoly-obj
					 horizon-elevation horizon-scale list-text-lwpolyline-obj
					 list-vertical-line-lwpolyline-obj lwpolyline-obj
					 start-profile-point vertical-scale)
  (princ "\n RUN fun-reaktor-elevation-3dpoly-obj")
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  
  (setq horizon-scale(vlax-ldata-get (vlr-data current-reactor) "horizon-scale"))
  (setq vertical-scale(vlax-ldata-get (vlr-data current-reactor) "vertical-scale"))
  (setq elevation-3dpoly-obj(vlax-ldata-get (vlr-data current-reactor) "elevation-3dpoly-obj"))
  (setq lwpolyline-obj (vlax-ldata-get (vlr-data current-reactor) "lwpolyline-obj"))
  (setq start-profile-point(vlax-ldata-get (vlr-data current-reactor) "start-profile-point"))
  (setq horizon-elevation(vlax-ldata-get (vlr-data current-reactor) "horizon-elevation"))
  (setq axis-trace-obj(vlax-ldata-get (vlr-data current-reactor) "axis-trace-obj"))
  (setq list-text-lwpolyline-obj(vlax-ldata-get (vlr-data current-reactor) "list-text-lwpolyline-obj"))
  (setq list-vertical-line-lwpolyline-obj(vlax-ldata-get (vlr-data current-reactor) "list-vertical-line-lwpolyline-obj"))
  
  (vla-put-coordinates
    lwpolyline-obj
    (draw-reactive-profile-lwpolyline-obj
      (analisis-3d-and-2d-poly-by-general-3dpoly
	axis-trace-obj
	obj-run-reactor)
      horizon-elevation
      start-profile-point
      horizon-scale
      vertical-scale))
  (vla-Update lwpolyline-obj)
  
  (if flag-reaktor-elevation-3dpoly-obj
    (progn
      (princ "\n flag-reaktor-elevation-3dpoly-obj=YES")
      (setq flag-reaktor-elevation-3dpoly-obj nil)
      )
    (progn
      (princ "\n flag-reaktor-elevation-3dpoly-obj=NO")
      (setq list-text-3dpolyline-obj(vlax-ldata-get (vlr-data current-reactor) "list-text-3dpolyline-obj"))
      (setq reaktor-text-3dpolyline-obj (car(vlr-get-reactors-for-object (car list-text-3dpolyline-obj))))
      (setq list-text-3dpolyline-obj(vlr-owners reaktor-text-3dpolyline-obj))
      (progn (strcat "\n length list-text-3dpolyline-obj="(itoa(length list-text-3dpolyline-obj))))
      (vlr-remove reaktor-text-3dpolyline-obj)
      (foreach x list-text-3dpolyline-obj(vlr-owner-remove reaktor-text-3dpolyline-obj x))
      (foreach x list-text-3dpolyline-obj(vla-delete x))
      (setq list-text-3dpolyline-obj nil)
      (setq list-text-3dpolyline-obj(draw-reactive-profile-text-3dpolyline-obj  obj-run-reactor))
      (foreach x list-text-3dpolyline-obj (setq flag-reaktor-text-3dpolyline-obj T)(vlr-owner-add reaktor-text-3dpolyline-obj x))
      (vlr-add reaktor-text-3dpolyline-obj)
      (vlax-ldata-put (vlr-data current-reactor) "list-text-3dpolyline-obj" list-text-3dpolyline-obj)
      )
    )
  
  
  
  (foreach x list-text-lwpolyline-obj (if(not(vlax-erased-p x))(vla-delete x)))
  (foreach x list-vertical-line-lwpolyline-obj (if(not(vlax-erased-p x))(vla-delete x)))
  (setq list-text-lwpolyline-obj (draw-reactive-profile-text-lwpolyline-obj
				   lwpolyline-obj
				   start-profile-point
				   horizon-elevation
				   horizon-scale
				   vertical-scale))
  (setq list-vertical-line-lwpolyline-obj (draw-reactive-profile-vertical-line-lwpolyline-obj
					    lwpolyline-obj
					    start-profile-point
					    horizon-elevation
					    horizon-scale
					    vertical-scale))
  
  (vlax-ldata-put (vlr-data current-reactor) "list-text-lwpolyline-obj" list-text-lwpolyline-obj)
  (vlax-ldata-put (vlr-data current-reactor) "list-vertical-line-lwpolyline-obj" list-vertical-line-lwpolyline-obj)
  
  )









(defun fun-reaktor-text-3dpolyline-obj (obj-run-reactor current-reactor arguments / elevation-3dpoly-obj reaktor-elevation-3dpoly-obj)
  (princ "\n RUN fun-reaktor-text-3dpolyline-obj")
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))

  (if flag-reaktor-text-3dpolyline-obj
    (progn
      (princ "\n flag-reaktor-text-3dpolyline-obj=YES")
      (setq flag-reaktor-text-3dpolyline-obj nil)
      )
    (progn
      (princ "\n flag-reaktor-text-3dpolyline-obj=NO")
      
      (setq elevation-3dpoly-obj (vlax-ldata-get (vlr-data current-reactor) "elevation-3dpoly-obj"))
      (setq reaktor-elevation-3dpoly-obj (car(vlr-get-reactors-for-object elevation-3dpoly-obj)))
      
      (vlr-remove reaktor-elevation-3dpoly-obj)
      (vla-put-Coordinate
	elevation-3dpoly-obj
	(1-(length(member obj-run-reactor (vlr-owners current-reactor))))
	(vlax-safearray-fill (vlax-make-safearray vlax-vbDouble '(0 . 2))
	  (list (car(vlax-safearray->list(vlax-variant-value(vla-get-TextAlignmentPoint obj-run-reactor))))
		(cadr(vlax-safearray->list(vlax-variant-value(vla-get-TextAlignmentPoint obj-run-reactor))))
		(atof(edit_text_clean_for_number(vla-get-TextString obj-run-reactor))))
	  )
	)
      (if flag-for-comm-fun-edit-text-by-real-number
	(if (eq (type flag-for-comm-fun-edit-text-by-real-number) 'LIST)
	  (setq flag-for-comm-fun-edit-text-by-real-number (append flag-for-comm-fun-edit-text-by-real-number(list obj-run-reactor)))
	  (setq flag-for-comm-fun-edit-text-by-real-number (append (list flag-for-comm-fun-edit-text-by-real-number)(list obj-run-reactor)))
	  )
	(setq flag-for-comm-fun-edit-text-by-real-number obj-run-reactor)
	)
      (setq flag-reaktor-elevation-3dpoly-obj T)
      (vlr-add reaktor-elevation-3dpoly-obj)
      )
    )
  
  )





(defun fun-reaktor-lwpolyline (vlao reac args / usl_z_point spisok point text_point text_str text_obj x line_obj list_text_obj list_line_obj reaktor-texts)
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  ;;;  (princ "\n RUN fun-reaktor-lwpolyline")
  (setq usl_z_point (car (vlr-data reac)))
  (setq list_text_obj (cadr (vlr-data reac)))
  (setq reaktor-texts(car(vlr-get-reactors-for-object (car list_text_obj))))
  (setq spisok(list-Coordinates-lwpolyline vlao))
  (foreach x list_text_obj (if(not(vlax-erased-p x))(vlr-owner-remove reaktor-texts x)))
  (foreach x list_text_obj (if(not(vlax-erased-p x))(vla-delete x)))
  (setq list_text_obj nil)
  (foreach point spisok
    (setq text_point (list (nth 0 point) (-(nth 1 usl_z_point)5)))
    (setq text_str (rtos(+(-(nth 1 point)(nth 1 usl_z_point))100)2 2))
    (setq text_obj (vla-AddText model_spece text_str (vlax-3D-point text_point) 2.5))
    (vla-put-Alignment text_obj 13)
    (vla-put-TextAlignmentPoint text_obj (vlax-3D-point text_point))
    (vla-put-Rotation text_obj (/ pi 2))
    (vla-put-Linetype text_obj "Continuous")
    (setq list_text_obj (append list_text_obj (list text_obj)))
    )
  (foreach x list_text_obj (vlr-owner-add reaktor-texts x))
  (vlr-data-set reac (list usl_z_point list_text_obj))
  (princ)
  )

(defun fun-reaktor-texts (vlao reac args / usl_z_point lwpolyline n f_point t_point list_text_obj reaktor-lwpolyline)
  (if (not model_spece)(progn(vl-load-com)(setq model_spece(vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))))
  ;;;  (princ "\n RUN fun-reaktor-texts")
  (setq usl_z_point (car (vlr-data reac)))
  (setq lwpolyline (cadr (vlr-data reac)))
  (setq reaktor-lwpolyline(car(vlr-get-reactors-for-object lwpolyline)))
  (setq list_text_obj (vlr-owners reac))
  (setq n(1-(length(member vlao list_text_obj))))
  (setq f_point(vlax-safearray->list(vlax-variant-value(vla-get-Coordinate lwpolyline n))))
  (setq t_point(list(nth 0 (vlax-safearray->list(vlax-variant-value(vla-get-TextAlignmentPoint vlao))))
		    (+(-(atof(edit_text_clean_for_number(vla-get-TextString vlao)))100)(nth 1 usl_z_point))))
  (if (not(eq f_point t_point))
    (progn
      (vlr-owner-remove reaktor-lwpolyline lwpolyline)
      (vla-put-Coordinate lwpolyline n (vlax-safearray-fill (vlax-make-safearray vlax-vbDouble '(0 . 1)) t_point))
      (vlr-owner-add reaktor-lwpolyline lwpolyline)
      )
    )
  (princ)
  )

(defun vlr-get-reactors-for-object (vla:object /); возвращает список реакторов указанного объекта
  (vl-remove-if-not
    (function (lambda (x)
      (member   vla:object
         (VLR-Owners x)
      ) ;_ end of member
         ) ;_ end of lambda
    ) ;_ end of function
    (cdar (vlr-reactors ':vlr-object-reactor))
  ) ;_ end of vl-remove-if-not
)



;;;  (setq spisok(list-Coordinates-lwpolyline lwpolyline-vla-obj))
;;;  (foreach point spisok
;;;    (setq text_point (list (nth 0 point) (-(nth 1 usl_z_point)5)))
;;;    (setq text_str (rtos(+(-(nth 1 point)(nth 1 usl_z_point))100)2 2))
;;;    (setq text_obj (vla-AddText model_spece text_str (vlax-3D-point text_point) 2.5))
;;;    (vla-put-Alignment text_obj 13)
;;;    (vla-put-TextAlignmentPoint text_obj (vlax-3D-point text_point))
;;;    (vla-put-Rotation text_obj (/ pi 2))
;;;    (vla-put-Linetype text_obj "Continuous")
;;;    (setq list_text_obj (append list_text_obj (list text_obj)))
;;;    )
;;;  (setq reaktor-lwpolyline (vlr-object-reactor(list lwpolyline-vla-obj)nil(list '(:vlr-modified . fun-reaktor-lwpolyline))))
;;;  (setq reaktor-texts (vlr-object-reactor list_text_obj nil(list '(:vlr-modified . fun-reaktor-texts))))
;;;  (vlr-data-set reaktor-lwpolyline (list usl_z_point list_text_obj))
;;;  (vlr-data-set reaktor-texts (list usl_z_point lwpolyline-vla-obj))
;;;  (vlr-pers reaktor-lwpolyline)
;;;  (vlr-pers reaktor-texts)