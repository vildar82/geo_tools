(defun analysis_centr_limit_be_elevation ( g1 g2 2Dpline / pline spisokv point_list p)
  (setq spisokv (extract_coord_lwpolyline (entget 2Dpline)))
  (repeat (length spisokv)
    (setq p (car spisokv))
    (setq spisokv (cdr spisokv))
    (if (and(or(>(nth 0 p)g1)(=(nth 0 p)g1)) (<(nth 0 p)(+ g2(/ (* 1.6 (/ 5.0 3.0)) (/ 1000 mg)))))
      (setq point_list (append point_list(list p)))
      )
    )
  (+(/(-(nth 0 (last point_list))(nth 0 (car point_list)))2)(nth 0 (car point_list)))
  );defun
