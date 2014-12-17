(defun C:geo_tools_packing ( / date day file-name folder-name h m month yeat dwl2_list dwl_list ws)
  (geo_tools-help "geo_tools_packing")
  (initget "Да Нет")(setq word (getkword "\nБудет произведена упаковка текущей версии Geo Tools, продолжить ? [Да/Нет] <Нет>: "))
  (if (eq word "Да")
    (progn
      (setq date (rtos (getvar "CDATE") 2 4))
      (setq *homedrive*(vl-registry-read "HKEY_CURRENT_USER\\Volatile Environment" "HOMEDRIVE"))
      
      (setq yeat (substr date 1 4))
      (setq month (substr date 5 2))
      (setq day (substr date 7 2))
      (setq h (substr date 10 2))
      (setq m (substr date 12 2))
      (setq folder-name (strcat *homedrive* "\\" "geo_tools (" yeat "." month "." day " " h "-" m ")ligth"))
      (setq file-name (strcat  "geo_tools " yeat "." month "." day ".zip"))
;;;      (setq folder-name (strcat *homedrive* "\\geo_tools"))
;;;      (setq file-name (strcat  "geo_tools.zip"))
      (folder-p-and-create folder-name)

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
      
      (folder-p-and-create(strcat folder-name "\\geo_tools"))
      
      (foreach x (list "\\bloks"
		       "\\data"
		       "\\fonts"
		       "\\icons"
		       "\\template"
		       "\\ToolPalette"
		       "\\uninstall")
	(terpri)
	(princ x)
	(packing-copy-folder
	  *geo_tools_root_dir*
	  (strcat folder-name "\\geo_tools")
	  x)
	)
      
      (folder-p-and-create(strcat folder-name "\\geo_tools\\helps"))
      (folder-p-and-create(strcat folder-name "\\geo_tools\\lisp"))
      (packing-copy-folder
	  (strcat *geo_tools_root_dir*"\\helps")
	  (strcat folder-name "\\geo_tools\\helps")
	  "\\tutorial")
      
      (vlax-invoke-method *file-system-object* 'CopyFile
	(strcat *geo_tools_root_dir* "\\acaddoc.lsp")
	(strcat folder-name "\\geo_tools\\acaddoc.lsp")t)
      
      (vlax-invoke-method *file-system-object* 'CopyFile
	(strcat *geo_tools_root_dir* "\\lisp\\all-compile-fun.VLX")
	(strcat folder-name "\\geo_tools\\lisp\\all-compile-fun.VLX")t)
      
      (vlax-invoke-method *file-system-object* 'CopyFile
	(strcat *geo_tools_root_dir* "\\helps\\geo_tools.chm")
	(strcat folder-name "\\geo_tools\\helps\\geo_tools.chm")t)
      
      (vlax-invoke-method *file-system-object* 'CopyFile
	(strcat *geo_tools_root_dir* "\\helps\\pltools.chm")
	(strcat folder-name "\\geo_tools\\helps\\pltools.chm")t)
      
      (vlax-invoke-method *file-system-object* 'CopyFile
	(strcat *geo_tools_root_dir* "\\helps\\uz_5000-500.chm")
	(strcat folder-name "\\geo_tools\\helps\\uz_5000-500.chm")t)
      
      (vlax-invoke-method
	(setq ws (vlax-get-or-create-object "wscript.shell"))
	"run"
	(strcat (strcat"\"" *geo_tools_root_dir* "\\uninstall\\7z.exe\"")
		(strcat "a -tzip -mx=9 \"" folder-name "\\" file-name"\" \""(strcat folder-name "\\geo_tools") "\""))
	1
	:vlax-true ;_дождаться завершения сеанса
	) ;_ end of vlax-invoke-method
      (vlax-release-object ws)
      
      
;;;      (startapp (strcat"\"" *geo_tools_root_dir* "\\uninstall\\7z.exe\"")
;;;		(strcat "a -tzip -mx=9 \"" folder-name "\\" file-name"\" \""(strcat folder-name "\\geo_tools") "\"")
;;;		)
      
      (vlax-invoke-method
	*file-system-object*
	'DeleteFolder
	(strcat folder-name "\\geo_tools")t)
      )
    )
  (princ)
  )

(defun packing-copy-folder (input output folder / )
  (vlax-invoke-method *file-system-object* 'CopyFolder
    (strcat input folder)
    (strcat output folder)
    t)
  )