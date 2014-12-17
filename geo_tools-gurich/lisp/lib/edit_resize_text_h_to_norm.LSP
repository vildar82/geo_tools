(defun edit_resize_text_h_to_norm ( / text ttt t1 txy); Нормальный размер шрифта
 (setq text (get_text_or_cogo_point))
  (setq ttt (entget text))
  (if (eq (cdr (assoc 0 (entget text))) "TEXT")
    (progn
      (setq t1 (calculation_coord_centre_text ttt))
  (cond
    ((eq (cdr (assoc 7 ttt)) "OTI")
     (entmod (list (cons -1 text) (cons 40 (/ (/ 2 (format_scale_kof)) 1.07372))))
     )
    ((eq (cdr (assoc 7 ttt)) "OTI")
     (entmod (list (cons -1 text) (cons 40 (/ (/ 2 (format_scale_kof)) 1.05994))))
     )
    ((eq (cdr (assoc 7 ttt)) "OTI")
     (entmod (list (cons -1 text) (cons 40 (/ 1.8 (format_scale_kof)))))
     )
    ((eq (cdr (assoc 7 ttt)) "OTI")
     (entmod (list (cons -1 text) (cons 40 (/ 2 (format_scale_kof)))))
     )
    (T (princ "ВНИМАНИЕ! Этот стиль текста не поддерживается."))
    ); cond
  (setq ttt (entget text))
  (setq txy (calculation_coord_ins_text_from_centre t1 (cdr (assoc 50 ttt)) ttt))
  (entmod (list (cons -1 text) (cons 10 txy)))  
  ); text
    (entmod (subst (cons 140 (/ 1.8 (format_scale_kof)))(assoc 140 ttt) ttt))
    );if
  (entupd text)
  (princ)
  )
