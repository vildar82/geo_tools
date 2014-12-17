(defun extract-function-of-macro (macro / pos fun)
  ;вытаскивает из макрокоманды(без ^C^C) имя вызываемой функции
  (setq macro (vl-string-trim " " macro))
  (if (setq pos(vl-string-search ";"macro))
    (setq fun (substr macro 1 pos))
    (setq fun macro)
    )
  (cond
    ((= fun "_TPNAVIGATE")
     (setq fun "tpnavigate")
     )
    ((or(= fun "format_check_line_typ")(= fun "_-Linetype"))
     (setq fun "draw_line_macro")
     )
    ((= fun "set_CMLSCALE_1")
     (setq fun "draw_mline_macro"))
    )
  fun
  )