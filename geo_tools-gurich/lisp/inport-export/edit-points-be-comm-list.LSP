(defun edit-points-be-comm-list (point-list comm-list / code-string result)
  ;редактирует список точек в соответствии с командным списком из диалога
  (if point-list
    (progn
      (setq code-string(cdr(assoc "code" comm-list)))
      (foreach x point-list
	(setq x (subst (cons "code" (edit-string-replase-substr (edit-string-replase-substr code-string "[D]" (cdr(assoc "code" x))) "[P]" (cdr(assoc "name" x))))
		       (assoc "code" x) x))
	(setq result (append result (list x)))
	)
      result
      )
    )
  )