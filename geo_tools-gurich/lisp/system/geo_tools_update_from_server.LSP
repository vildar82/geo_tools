(defun C:geo_tools_update_from_server ( / word)
  (princ (strcat "\n"(car (get-curent-version-geo_tools))" текущая версия"))
  (princ (strcat "\n"(car (get-server-version-geo_tools))" версия на сервере"))
  (if (<(cadr (get-curent-version-geo_tools))(cadr (get-server-version-geo_tools)))
    (progn
      (initget "Да Нет")(setq word (getkword (strcat "\nНадо обновиться, будем? [Да/Нет] <Да>: ")))
      (if (not word) (setq word "Да"))
      (if (eq word "Да")
	(geo_tools-update-from-server)
	)
      )
    (princ "\nОбновление не требуется.")
    )
  (princ)
  )

(defun geo_tools-update-from-server ( / DIR FLS WS)
  (setq geo_tools-server-patch (get-geo_tools-server-patch))
  ; ***************************** удаление содержимого *geo_tools_root_dir* **************************
  (setq dir (cddr(vl-directory-files *geo_tools_root_dir* nil -1)))
  (setq fls (cddr(vl-directory-files *geo_tools_root_dir* nil 1)))
  (if dir(foreach x dir (folder-delete (strcat *geo_tools_root_dir* x))))
  (if fls(foreach x fls(vl-file-delete (strcat *geo_tools_root_dir* x))))
  ; ***************************************************************************************************
  (vlax-invoke-method
    (setq ws (vlax-get-or-create-object "wscript.shell"))
    "run"
    (strcat "xcopy "  "\"" geo_tools-server-patch "\" \"" *geo_tools_root_dir* "\"" " /E /Y /R /H")
    1
    :vlax-true ;_дождаться завершения сеанса
    ) ;_ end of vlax-invoke-method
  (vlax-release-object ws)
  )