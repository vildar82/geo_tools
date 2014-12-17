(vl-load-com)
(setq *geo_tools_root_dir* (strcat (getenv "ProgramFiles") "\\geo_tools"))
(setq *geo_tools_user_dat_dir* (strcat (getenv "APPDATA") "\\geo_tools"))
(if
  (vl-catch-all-error-p(vl-catch-all-apply 'load(list(strcat *geo_tools_root_dir* "\\lisp\\system\\geo-tools-load-all.lsp"))))
  (princ "\nОшибка попытки загрузки исходных функций Geo Tools!")
  (geo-tools-load-all)
  )
(if load_global_variable
  (progn
    (load_global_variable)
    (geo-tools-update)
    )
  )