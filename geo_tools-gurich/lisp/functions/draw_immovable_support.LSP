(defun C:draw_immovable_support ( / curent-profile insert-point map-profile scale-coefficient second-point third-point flag temp1 temp2)
  ; ������� ����������� �����
  (geo_tools-help "draw_immovable_support")
  (load_global_variable)
  (initget 1 "���� �������")(setq map-profile (getkword "\n���� ��������� �����? [����/�������]: "))
  (setq curent-profile(vlax-ldata-get "geo_tools_dictionary" "curent-profile"))
  (cond
    ((eq map-profile"�������")
     (if (profile-check-curent-profile curent-profile)
       (progn
	 (setq scale-coefficient (/ 1.0 (/ 1000.0 (vlax-ldata-get(cdr curent-profile)"horizontal-scale"))));(vlax-ldata-get(cdr (vlax-ldata-get "geo_tools_dictionary" "curent-profile"))"horizontal-scale")
	 (setq flag T)
	 )
       (setq flag nil)
       )
     )
    ((eq map-profile"����")
     (setq scale-coefficient (/ 1.0 (getvar "CANNOSCALEVALUE")))
     (setq flag T)
     )
    )
  (if flag
    (progn
      (setq insert-point(getpoint "\n����� ��� ����������� �����: "))
      (if insert-point
	(progn
	  (setq second-point(getpoint insert-point"\n����� ������ (����) ����������� �����: "))
	  (vla-StartUndoMark active_document)
;;;	  (princ(vlax-ldata-get(cdr curent-profile)"horizontal-scale"))
	  (setq third-point(polar second-point (angle second-point insert-point)(*(distance second-point insert-point)2)))
	  (vla-insertblock model_spece (vlax-3D-point insert-point) "�����" 1.0 1.0 1.0 0)
	  (draw-line-vla-by-list-property(list
					   (cons "spece" model_spece)
					   (cons "StartPoint" second-point)
					   (cons "EndPoint" third-point)
					   (cons "Linetype" "Continuous")
					   ))
	  (foreach x (list second-point third-point)
	    (setq temp1 (polar x (+(angle x insert-point)(/ pi 2))(* 2.5 scale-coefficient)))
	    (setq temp2 (polar x (-(angle x insert-point)(/ pi 2))(* 2.5 scale-coefficient)))
	    (draw-line-vla-by-list-property(list
					     (cons "spece" model_spece)
					     (cons "StartPoint" temp1)
					     (cons "EndPoint" temp2)
					     (cons "Linetype" "Continuous")
					     ))
	    
	    (draw-line-vla-by-list-property(list
					     (cons "spece" model_spece)
					     (cons "StartPoint" x)
					     (cons "EndPoint" (polar x (+(angle temp2 temp1)(/ pi 5))(* 2.5 scale-coefficient)))
					     (cons "Linetype" "Continuous")
					     ))
	    (draw-line-vla-by-list-property(list
					     (cons "spece" model_spece)
					     (cons "StartPoint" temp1)
					     (cons "EndPoint" (polar temp1 (+(angle temp2 temp1)(/ pi 5))(* 2.5 scale-coefficient)))
					     (cons "Linetype" "Continuous")
					     ))
	    (draw-line-vla-by-list-property(list
					     (cons "spece" model_spece)
					     (cons "StartPoint" temp2)
					     (cons "EndPoint" (polar temp2 (+(angle temp2 temp1)(/ pi 5))(* 2.5 scale-coefficient)))
					     (cons "Linetype" "Continuous")
					     ))
	    )
	  (vla-EndUndoMark active_document)
	  )
	)
      )
    )
  (princ)
  )