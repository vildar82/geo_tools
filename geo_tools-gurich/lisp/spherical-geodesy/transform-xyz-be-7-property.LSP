(defun transform-xyz-be-7-property (delta-x delta-y delta-z r-x r-y r-z delta-m X Y Z / new-X new-Y new-Z)
  (setq new-X (+ delta-x (* (+ 1.0 delta-m) (+ X (* r-z Y) (* (* -1.0 r-z) Z)))))
  (setq new-Y (+ delta-y (* (+ 1.0 delta-m) (+ (* (* -1.0 r-z) X) Y (* r-x Z)))))
  (setq new-Z (+ delta-z (* (+ 1.0 delta-m) (+ (* r-y X) (* (* -1.0 r-x) Y) Z))))
  (list new-X new-Y new-Z)
  )

