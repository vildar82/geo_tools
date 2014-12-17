(defun list-coordinates-lwpolyline (lwpolyline / listing list_coord)
  ; возвращает список координат вершин 2dполилинии
  (if (eq (type lwpolyline) 'ename)
    (setq lwpolyline (vlax-ename->vla-object lwpolyline)))
  (setq listing(vlax-safearray->list(vlax-variant-value(vla-get-Coordinates lwpolyline))))
  
  (repeat (/(length listing)2)
    (setq list_coord (append list_coord (list(list (car listing)(cadr listing)(vla-get-Elevation lwpolyline)))))
    (setq listing (cddr listing)))
  list_coord
  )