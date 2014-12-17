(defun analysis_limit_create (2Dpline / spisokv p1 p2 Dgranicy list_granic)
  (setq spisokv (extract_coord_lwpolyline (entget 2Dpline)))
  (repeat (1-(length spisokv))
    (setq p1 (car spisokv))
    (setq spisokv (cdr spisokv))
    (setq p2 (car spisokv))
    (if (<(- (nth 0 p2) (nth 0 p1))(/ (* 2 (/ 4.0 3.0)) (/ 1000 mg)))
      (setq list_granic (append list_granic (list (list (nth 0 p1)(nth 0 p2)))))
      )
    )
  list_granic
  );defun
