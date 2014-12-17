(defun C:geo_tools_save_to_server ( / curent-version-string date day dwl2_list dwl_list h m month old-version old-version-string word ws yeat geo_tools-server-patch)
  (setq date (rtos (getvar "CDATE") 2 4))
  (setq yeat (substr date 1 4))
  (setq month (substr date 5 2))
  (setq day (substr date 7 2))
  (setq h (substr date 10 2))
  (setq m (substr date 12 2))
  (setq curent-version-string (strcat yeat "." month "." day " " h ":" m))
  (setq geo_tools-server-patch (get-geo_tools-server-patch))
  (initget "Да Нет")(setq word (getkword (strcat "\nЗаписать на сервер версию "curent-version-string " там сейчас "(car(get-server-version-geo_tools))"? [Да/Нет] <Нет>: ")))
  (if (eq word "Да")
    (progn
      ; ******************* копирование (с заменой) пользовательских данных в основную папку ************
      (vlax-invoke-method *file-system-object* 'CopyFolder
	(strcat *geo_tools_user_dat_dir* "\\data")
	(strcat *geo_tools_root_dir* "\\data")t)
      (vlax-invoke-method *file-system-object* 'CopyFolder
	(strcat *geo_tools_user_dat_dir* "\\template")
	(strcat *geo_tools_root_dir* "\\template")t)
      (setq dwl_list (file_find_be_extension (strcat *geo_tools_root_dir* "\\template") "dwl"))
      (setq dwl2_list (file_find_be_extension (strcat *geo_tools_root_dir* "\\template") "dwl2"))
      (if dwl_list(foreach x dwl_list(vl-file-delete x)))
      (if dwl2_list(foreach x dwl2_list(vl-file-delete x)))
      ; ***************************************************************************************************
      (file_write_list_to_file (strcat *geo_tools_root_dir* "\\version.txt") (list(strcat yeat month day h m)))
      (folder-delete geo_tools-server-patch)
      (folder-p-and-create geo_tools-server-patch)
      (vlax-invoke-method
	(setq ws (vlax-get-or-create-object "wscript.shell"))
	"run"
	(strcat "xcopy "  "\"" *geo_tools_root_dir* "\" \"" geo_tools-server-patch "\"" " /E /Y /R /H")
	1
	:vlax-true ;_дождаться завершения сеанса
	) ;_ end of vlax-invoke-method
      (vlax-release-object ws)
      )
    )
  (princ)
  )