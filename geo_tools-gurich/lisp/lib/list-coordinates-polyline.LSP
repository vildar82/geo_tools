(defun list-coordinates-polyline (polyline / listing list_coord); ���������� ������ ��������� ������ vla ������� 3d���������
  (if (eq (type polyline) 'ename)
    (setq polyline (vlax-ename->vla-object polyline)))
  (setq listing(vlax-safearray->list(vlax-variant-value(vla-get-Coordinates polyline))))
  (repeat (/(length listing)3)
    (setq list_coord (append list_coord (list(list (car listing)(cadr listing)(caddr listing)))))
    (setq listing (cdddr listing)))
  list_coord
  )