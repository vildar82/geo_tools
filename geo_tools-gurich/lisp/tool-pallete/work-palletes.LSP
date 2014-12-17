(defun C:work-palletes ( / list-pallets n name new-patch o-doc palettes patch result-list)
  ; Обработка файла каталога инструментальных палитр 
  (setq o-doc (XML-Get-XMLObject (strcat *geo_tools_root_dir* "\\ToolPalette_\\AcTpCatalog.atc")))
  (setq Palettes (XML-Get-Child o-doc nil "Palettes"))
  
;;; =============================== Сортировка инстр.палитр в соответствии с профилем ===============================
  (setq list-pallets (list-pallets-in-profile))
  (setq n 0)
  (repeat  (length list-pallets)
    (while (not(eq (nth n list-pallets)(get-name-tool-pallete(nth n (XML-Get-ChildList Palettes)))))
      (move-child-be-number-to-end Palettes n)
      )
    (setq n (1+ n))
    )
  (XML-Save o-doc)
;;; =================================================================================================================
  
  (foreach x (XML-Get-ChildList Palettes)
    (setq patch (XML-Get-Attribute(XML-Get-Child x nil "Url")"href" ""))
    (setq name(get-name-tool-pallete x))
    
;;; ********************** Переименование файлов инстр.палитр в соответствии с именем палитры **********************************
    (if (/= name (vl-filename-base patch))
      (progn
	(setq new-patch (strcat (vl-filename-directory patch)"\\"name(vl-filename-extension patch)))
	(XML-Put-Attribute(XML-Get-Child x nil "Url")"href"new-patch)
	(vl-file-rename
	  (strcat *geo_tools_root_dir* "\\ToolPalette_\\" patch)
	  (strcat *geo_tools_root_dir* "\\ToolPalette_\\" new-patch)
	  )
	(XML-Save o-doc)
	(setq patch new-patch)
	)
      )
;;; *****************************************************************************************************************************
    
    (setq result-list (append result-list (list  patch)))
    )
  (vlax-release-object Palettes)
  (vlax-release-object o-doc)
  (foreach y result-list
    (work-file-pallet (strcat *geo_tools_root_dir* "\\ToolPalette_\\"y))
    )
  (princ)
  )


(defun work-file-pallet (file / description-booton macro name-booton o-doc patch-image properties result-list tools id-booton new-patch-image)
  ; обработка файла инструментальной палитры 
  (setq o-doc (XML-Get-XMLObject file))
  (setq result-list (list(XML-Get-Child-Value(XML-Get-Child o-doc nil "Properties")nil "ItemName")))
  (setq Tools (XML-Get-Child o-doc nil "Tools"))
  (foreach x (XML-Get-ChildList Tools)
    (if (=(XML-Get-Attribute x "option" "")"24")
      (progn
	(setq properties (XML-Get-Child x nil "Properties"))
	(if (=(setq name-booton (XML-Get-Child-Value properties nil "ItemName"))nil)(setq name-booton ""))
	(if (=(setq description-booton (XML-Get-Child-Value properties nil "Description"))nil)(setq description-booton ""))
	(setq patch-image (XML-Get-Attribute(XML-Get-Child(XML-Get-Child properties nil "Images")nil "Image")"src" ""))
	(setq id-booton (XML-Get-Attribute(XML-Get-Child x nil "ItemID")"idValue" ""))
	(if (=(setq macro (XML-Get-Child-Value(XML-Get-Child(XML-Get-Child x nil "Data") nil "Command") nil "Macro"))nil)(setq macro ""))
	
;;; ********************** Переименование файлов картинок кнопок в соответствии имени кнопки **********************************
	(if (/= (edit_translit_str name-booton) (vl-filename-base patch-image))
	  (progn
	    (cond
	      ((= name-booton "")
	       (setq new-patch-image (strcat (vl-filename-directory patch-image)"\\blank"(vl-filename-extension patch-image))))
	      ((and (vl-string-search "ГОСТ" name-booton)(vl-string-search "draw_insert_format_ramka" macro))
	       (setq new-patch-image (strcat (vl-filename-directory patch-image)"\\GOST"(vl-filename-extension patch-image))))
	      ((and (vl-string-search "ISO" name-booton)(vl-string-search "draw_insert_format_ramka" macro))
	       (setq new-patch-image (strcat (vl-filename-directory patch-image)"\\ISO"(vl-filename-extension patch-image))))
	      (T(setq new-patch-image (strcat (vl-filename-directory patch-image)"\\"(edit_translit_str name-booton)(vl-filename-extension patch-image))))
	      )
	    (XML-Put-Attribute(XML-Get-Child(XML-Get-Child properties nil "Images")nil "Image")"src"new-patch-image)
	    (if(not
		 (vl-file-rename
		   (strcat *geo_tools_root_dir* "\\ToolPalette_\\Palettes\\" (vl-string-left-trim "."patch-image))
		   (strcat *geo_tools_root_dir* "\\ToolPalette_\\Palettes\\" (vl-string-left-trim "."new-patch-image))
		   ))
	      (vl-file-delete (strcat *geo_tools_root_dir* "\\ToolPalette_\\Palettes\\" (vl-string-left-trim "."patch-image))))
	    (XML-Save o-doc)
	    (setq patch-image new-patch-image)
	    )
	  )
;;;	(XML-Put-Attribute(XML-Get-Child(XML-Get-Child properties nil "Images")nil "Image")"src"
;;;	  (strcat (vl-filename-directory patch-image)"\\"(vl-filename-base patch-image)".png"))
;;;	(XML-Save o-doc)
;;; ***************************************************************************************************************************
	
	(setq result-list (append result-list (list name-booton description-booton id-booton patch-image macro)))
	))
    )
  (vlax-release-object properties)
  (vlax-release-object Tools)
  (vlax-release-object o-doc)
;;;  (if result-list(file_write_list_to_file(strcat *geo_tools_root_dir* "\\lisp\\" (car result-list) ".txt")result-list))
  (princ)
  )

(defun list-pallets-in-profile ( / catalogview o-doc result)
  ; возвращает список инструментальных палитр в том порядке в каком он записан в файле профиля
  (setq o-doc (XML-Get-XMLObject (strcat(vl-filename-directory(vla-get-MenuFile(vla-get-files(vla-get-Preferences(vlax-get-acad-object)))))"\\Profiles\\C3D_Metric\\Profile.aws")))
  (foreach x (XML-Get-ChildList (XML-Get-Child
				  (XML-Get-Child
				    (XML-Get-Child
				      (XML-Get-Child
					(XML-Get-Child
					  (XML-Get-Child o-doc nil"StorageRoot")
					  nil"ToolPaletteScheme")
					nil"ToolPaletteSets")
				      nil"ToolPaletteSet")
				    nil"CAcTcUiToolPaletteSet")
				  nil"ToolPalettes"))
    (setq CatalogView
	   (XML-Get-Child
	     (XML-Get-Child x nil"CAcTcUiToolPalette")
	     nil"CatalogView"))
    (setq result
	   (append
	     result
	     (list(XML-Get-Child-Value
		    (XML-Get-Child
		      CatalogView
		      nil"ToolSource")
		    nil"Palette"))))
    )
  result
  )

(defun get-name-tool-pallete (tool-pallete / name)
  ; возвращает имя инструментальной палитры
  (if (=(setq name (XML-Get-Child-Value (XML-Get-Child tool-pallete nil "Properties") nil "ItemName"))nil)(setq name ""))
  name
  )

(defun move-child-be-number-to-end (parent number / ChildList)
  ; перемещает элемент по номеру в конец
  (XML-Add-Child parent "zaglushka")
  (setq ChildList(XML-Get-ChildList parent))
  (vlax-invoke-method parent 'replaceChild
    (nth number ChildList)
    (nth (1-(length ChildList)) ChildList))
  )

;;;
;;;(vlax-dump-object(vla-item(vla-get-menubar(vlax-get-acad-object))0)T)
;;;
;;;(vl-filename-base (vl-string-left-trim "." ".\\Images\\Инструмент работы с командами_0D63CF6C-A681-49EA-BD14-A5AD6C182A7D.BMP"))
;;;(vl-filename-base ".\\Images\\Инструмент работы с командами_0D63CF6C-A681-49EA-BD14-A5AD6C182A7D.BMP")
;;;(vl-filename-extension (vl-string-left-trim "." ".\\Images\\Инструмент работы с командами_0D63CF6C-A681-49EA-BD14-A5AD6C182A7D.BMP"))
;;;(vl-filename-directory (vl-string-left-trim "." ".\\Images\\Инструмент работы с командами_0D63CF6C-A681-49EA-BD14-A5AD6C182A7D.BMP"))
;;;(vl-filename-directory (vl-string-left-trim "." ".\\Images\\Инструмент работы с командами_0D63CF6C-A681-49EA-BD14-A5AD6C182A7D.BMP"))
;;;
;;;  (setq file (strcat *geo_tools_root_dir* "\\ToolPalette_\\test.xml"))
;;;  (setq file (strcat *geo_tools_root_dir* "\\ToolPalette_\\AcTpCatalog.atc"))
;;;  (setq file (strcat *geo_tools_root_dir* "\\ToolPalette_\\Palettes\\02 Пункты геодезические.atc"))
;;;  (setq doc (vlax-get-or-create-object "MSXML2.DOMDocument.3.0"));создание объекта xml
;;;  (vlax-put-property doc 'async :vlax-false); изменение какого то параметра
;;;  (vlax-invoke-method doc 'load file);загрузка в объект xml файла
;;;  (vlax-dump-object doc T)
;;;
;;;  (vlax-dump-object(setq xml-top(vlax-get-property doc 'childnodes))T)
;;;  (vlax-dump-object(vlax-invoke-method xml-top "nextNode")T)
;;;  (vlax-dump-object(vlax-get-property xml-top 'item 0)T)
;;;  (vlax-dump-object(XML-Get-ChildList(vlax-get-property xml-top 'item 0))T)
;;;  (vlax-dump-object(vlax-get-property (vlax-get-property xml-top 'item 0) 'childnodes)T)
;;;  (vlax-dump-object(vlax-get-property (vlax-get-property (vlax-get-property xml-top 'item 0) 'childnodes)'item 0)T)
;;;  (vlax-dump-object(vlax-get-property(vlax-get-property(vlax-get-property (vlax-get-property (vlax-get-property xml-top 'item 0) 'childnodes)'item 1)'childnodes)'item 0)T)
;;;  (vlax-dump-object(vlax-get-property(vlax-get-property (vlax-get-property(vlax-get-property(vlax-get-property (vlax-get-property (vlax-get-property xml-top 'item 0) 'childnodes)'item 1)'childnodes)'item 0)'childnodes)'item 0)T)
;;;  "Url href" "Palettes\01 Главная.atc"
;;;  (vlax-dump-object(XML-Get-Children(vlax-get-property xml-top 'item 0) "Tools")T)
;;;  (vlax-release-object doc)
;;;  (vlax-dump-object doc T)
;;;(vlax-dump-object(vlax-invoke-method (vlax-get-property doc 'firstChild) "nextNode")T)
;;;(vlax-dump-object(vlax-get-property doc 'firstChild)T)
;;;(vlax-dump-object xml-top T)
;;;(vlax-get-property(vlax-get-property doc 'firstChild) 'childNodes)
;;;
;;;(vlax-get-property xml-top 'childnodes)
;;;(setq list-1 (XML-Get-ChildList Tools))
;;;(vlax-dump-object(XML-Get-Child(XML-Get-Child(XML-Get-Child (nth 0 list-1) nil "Data") nil "Command") nil "Macro")T)
;;;(XML-Get-Child (nth 0 list-1) nil "Properties")
;;;(vlax-dump-object(XML-Get-Child (nth 0 list-1) nil "Properties")T)
;;;(vlax-dump-object(XML-Get-Child(XML-Get-Child (nth 0 list-1) nil "Properties")nil "ItemName")T)
;;;(vlax-dump-object(XML-Get-Child-Value(XML-Get-Child (nth 0 list-1) nil "Properties")nil "ItemName")T)
;;;(XML-Get-Child(XML-Get-Child x nil "Properties")nil "ItemName")
;;;(XML-Get-Document projfile 'XMLDoc)
;;;
;;;(setq n 0)
;;;(while (not(> n 5))
;;;      (princ (setq n (1+ n)))
;;;      )