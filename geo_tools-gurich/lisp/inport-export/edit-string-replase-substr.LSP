(defun edit-string-replase-substr (string old-str new-str /)
  (if (not new-str) (setq new-str ""))
  (while (vl-string-search old-str string)
    (setq string (vl-string-subst new-str old-str string)))
  string
  )