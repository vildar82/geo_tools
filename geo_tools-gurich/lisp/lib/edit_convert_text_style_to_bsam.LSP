(defun edit_convert_text_style_to_bsam ( / text ttt t1 txy); Преобразование в OTI
  ;(format_check_text_style)
  (setq text (get_text_only))
  (setq ttt (entget text))
  (setq t1 (calculation_coord_centre_text ttt))
  (cond
    ((> (cdr (assoc 40 ttt)) (/ (/ 3 (format_scale_kof)) 1.05994))
     (entmod (list (cons -1 text) (cons 40 (/ (/ 3 (format_scale_kof)) 1.05994))  '(41 . 1.0) '(51 . 0.0) '(7 . "OTI")))
     )
    ((< (cdr (assoc 40 ttt)) (/ 1.6 (format_scale_kof)))
     (entmod (list (cons -1 text) (cons 40 (/ 1.6 (format_scale_kof)))  '(41 . 1.0) '(51 . 0.0) '(7 . "OTI")))
     )
    (T
     (entmod (list (cons -1 text)  '(41 . 1.0) '(51 . 0.0) '(7 . "OTI")))
     )
    ); cond
  (setq ttt (entget text))
  (setq txy (calculation_coord_ins_text_from_centre t1 (cdr (assoc 50 ttt)) ttt))
  (entmod (list (cons -1 text) (cons 10 txy)))
  (entupd text)
  (princ)
)
