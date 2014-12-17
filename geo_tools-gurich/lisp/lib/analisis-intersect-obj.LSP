(defun analisis-intersect-obj (obj1 obj2 / var result)
  ; рассматриваются пересечение двух объектов результат список точек пересечения
  (if (eq (type obj1) 'ENAME)
    (setq obj1 (vlax-ename->vla-object obj1)))
  (if (eq (type obj2) 'ENAME)
    (setq obj2 (vlax-ename->vla-object obj2)))
  (setq var (vla-intersectWith  obj1 obj2 acExtendNone))
  (if var
    (if (not(eq (vlax-safearray-get-u-bound (vlax-variant-value var) 1)-1))
      (setq result (convert-safearray-to-list-points(vlax-variant-value var)3))
      )
    )
  result
  )