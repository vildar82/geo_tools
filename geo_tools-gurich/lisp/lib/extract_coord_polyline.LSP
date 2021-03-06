(defun extract_coord_polyline (obj)
  ; ������� ���������� ��������� ������ 3D���������
  (
   (lambda (data)
     (cond
       ((= (cdr (assoc 0 data)) "POLYLINE")
	(extract_coord_polyline (entnext obj)))
       ((= (cdr (assoc 0 data)) "VERTEX")
	(cons
	  (cdr (assoc 10 data))
	  (extract_coord_polyline (entnext obj))))
       (T nil)
       )
     )
    (entget obj)
    )
  )
