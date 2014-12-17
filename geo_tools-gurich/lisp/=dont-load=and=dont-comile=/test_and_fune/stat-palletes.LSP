(defun C:stat-palletes ( / o-doc palettes result patch result-list FLAG LIST-MACRO n NUMERIK-LIST-MACRO NUMERIK-LIST-PALLETS)
  ; Обработка файла каталога инструментальных палитр
  (setq o-doc (XML-Get-XMLObject (strcat *geo_tools_root_dir* "\\ToolPalette\\AcTpCatalog.atc")))
  (setq Palettes (XML-Get-Child o-doc nil "Palettes"))
  (foreach x (XML-Get-ChildList Palettes)
    ; получение списка путей до файлов инструментальных палитр
    (setq patch (XML-Get-Attribute(XML-Get-Child x nil "Url")"href" ""))
    (setq result-list (append result-list (list  patch)))
    )
  (vlax-release-object Palettes)
  (vlax-release-object o-doc)
  (foreach y (cdr result-list)
    ; обработка файлов инструментальных палитр - получение ассоциированного списка инструментов ("Name" "HelpString" "Image" "Command")
    (setq result(append result(list(stat-file-pallet (strcat *geo_tools_root_dir* "\\ToolPalette\\"y))))))
;;;  (foreach z result
;;;    ; получение из общего списка (разбитого на палитры) списка не повторяющихся инструментов
;;;    (setq flag nil)
;;;    (foreach r z
;;;      (if flag
;;;	(if (not (member-name-in-list r list-macro))
;;;	  (setq list-macro(append list-macro(list r)))
;;;	  )
;;;	(setq flag T)
;;;	)
;;;      )
;;;    )
  
;;;  (setq n 1)
;;;  (foreach macro list-macro
;;;    ;нумерация команд
;;;    (setq numerik-list-macro(append numerik-list-macro(list (append macro (list (list "number" n))))))
;;;    (setq n (1+ n))
;;;    )
;;;  (setq numerik-list-pallets ;из списка команд делаем список номеров команд
;;;	 (convert-list-pallets-to-list-numbers numerik-list-macro result))
;;;  (setq *num-buton* 1)
;;;  (setq n 1)
;;;  
;;;  (foreach a numerik-list-macro ; создаются команды в cui
;;;    (add-macro-to-cui a)
;;;    (setq n (1+ n))
;;;    )
;;;  (setq n 1)
;;;  (foreach b numerik-list-pallets; создаются панели инструментов в cui
;;;    (add-toolbar-to-cui b n numerik-list-macro)
;;;    (setq n (1+ n))
;;;    )
  (list-to-file-xml "D:\\temp\\test.xml" result )
  (princ)
  )

(defun convert-list-pallets-to-list-numbers (numerik-list-macro ass-list-pallets / result)
  ;из списка команд делает список номеров команд
  (foreach pallet ass-list-pallets
    (setq result (append result (list (numberik-pallet pallet numerik-list-macro))))
    )
  result
  )

(defun numberik-pallet (list-pallet number-macro-list / name-list RESULT)
  ; обрабатывает одну панель
  (foreach x number-macro-list
    (setq name-list (append name-list (list (list (cadr(assoc "Command" x))(cadr(assoc "number" x))))))
    )
  (setq result(list(car list-pallet)))
  (setq list-pallet(cdr list-pallet))
  (foreach y list-pallet
    (setq result(append result(list(cadr(assoc(cadr(assoc"Command"y))name-list)))))
    )
  result
  )

(defun member-name-in-list (item all-list / name-list)
  (foreach x all-list
    (setq name-list (append name-list (list(cadr(assoc "Command" x)))))
    )
  (if (member (cadr(assoc "Command" item))name-list)
    T
    nil)
  )

(defun stat-file-pallet (file / description-booton macro name-booton o-doc patch-image properties result-list tools id-booton new-patch-image)
  ; обработка файла инструментальной палитры, возвращает ассоциированный список
  (setq o-doc (XML-Get-XMLObject file))
  (setq result-list (list(XML-Get-Child-Value(XML-Get-Child o-doc nil "Properties")nil "ItemName")))
  (setq Tools (XML-Get-Child o-doc nil "Tools"))
  (foreach x (XML-Get-ChildList Tools)
    (if (=(XML-Get-Attribute x "option" "")"24")
      (progn
	(setq properties (XML-Get-Child x nil "Properties"))
	(if (=(setq name-booton (XML-Get-Child-Value properties nil "ItemName"))nil)(setq name-booton ""))
	(if (=(setq description-booton (XML-Get-Child-Value properties nil "Description"))nil)(setq description-booton "Описание команды не указано."))
	(setq patch-image (XML-Get-Attribute(XML-Get-Child(XML-Get-Child properties nil "Images")nil "Image")"src" ""))
	(if (=(setq macro (XML-Get-Child-Value(XML-Get-Child(XML-Get-Child x nil "Data") nil "Command") nil "Macro"))nil)(setq macro ""))
	(if (and (/= name-booton "") (not (vl-string-search "ипперейти" macro)))
	  (setq result-list (append result-list (list
						  (list (list "Name" name-booton)
							(list "HelpString" description-booton)
							(list "Image" (strcat(vl-filename-base patch-image)(vl-filename-extension patch-image)))
							(list "Command" macro))
						  )))
	  )
	))
    )
  (vlax-release-object properties)
  (vlax-release-object Tools)
  (vlax-release-object o-doc)
  result-list
  )





(defun add-macro-to-cui (input-list / n command_ helpstring largeimage macro macrogroup menumacro name o-doc smallimage)
  ; добавляет команды в cui
  (setq n (convert-numder-to-string-and-add-nul (cadr(assoc "number" input-list)) 4))
  (setq o-doc (XML-Get-XMLObject (strcat *geo_tools_root_dir* "\\geo_tools.cui")))
  (setq MacroGroup (XML-Get-Child-ByAttribute (XML-Get-Child o-doc nil "MenuGroup") nil "Name" "geo_tools"))
  
  (setq MenuMacro (XML-Add-Child MacroGroup "MenuMacro"))
  (XML-Add-Attribute MenuMacro "UID" (strcat "geo_tools_macro_" n))
  (setq Macro (XML-Add-Child MenuMacro "Macro"))
  (XML-Add-Attribute Macro "type" "Any")
  (setq Name (XML-Add-Child Macro "Name"))
  (XML-Add-Attribute Name "xlate" "true")
  (XML-Add-Attribute Name "UID" (strcat "geo_tools_macro_n_" n))
  (api-error 'vlax-put-property (list Name 'text (cadr(assoc "Name" input-list))) T)
  
  (setq Command_ (XML-Add-Child Macro "Command"))
  (api-error 'vlax-put-property (list Command_ 'text (cadr(assoc "Command" input-list))) T)
  
  (setq HelpString (XML-Add-Child Macro "HelpString"))
  (XML-Add-Attribute HelpString "xlate" "true")
  (XML-Add-Attribute HelpString "UID" (strcat "geo_tools_macro_h_" n))
  (api-error 'vlax-put-property (list HelpString 'text (cadr(assoc "HelpString" input-list))) T)
  
  (setq SmallImage (XML-Add-Child Macro "SmallImage"))
  (XML-Add-Attribute SmallImage "Name" (cadr(assoc "Image" input-list)))

  (setq LargeImage (XML-Add-Child Macro "LargeImage"))
  (XML-Add-Attribute LargeImage "Name" (cadr(assoc "Image" input-list)))
  
  (XML-Save o-doc)
  (foreach x (list command_ helpstring largeimage macro macrogroup menumacro name o-doc smallimage)
    (vlax-release-object x)
    )
  (princ)
  )




(defun add-toolbar-to-cui (input-list n numerik-list-macro / alias name o-doc toolbar toolbarroot)
  ;добавляет панель инструментов в cui
;;;  (setq input-list (list "Название панели" 1 2 3 4 5))
  (setq n (convert-numder-to-string-and-add-nul n 3))
  (setq o-doc (XML-Get-XMLObject (strcat *geo_tools_root_dir* "\\geo_tools.cui")))
  (setq ToolbarRoot (XML-Get-Child (XML-Get-Child (XML-Get-Child o-doc nil "MenuGroup") nil "MenuRoot")nil "ToolbarRoot"))
  (setq Toolbar (XML-Add-Child ToolbarRoot "Toolbar"))
  (XML-Add-Attribute Toolbar "ToolbarOrient" "floating")
  (XML-Add-Attribute Toolbar "ToolbarVisible" "hide")
  (XML-Add-Attribute Toolbar "xval" "200")
  (XML-Add-Attribute Toolbar "yval" "200")
  (XML-Add-Attribute Toolbar "rows" "1")
  (XML-Add-Attribute Toolbar "UID" (strcat "geo_tools_toolbar_" n))
  (setq Alias (XML-Add-Child Toolbar "Alias"))
  (api-error 'vlax-put-property (list Alias 'text (strcat "geo_tools_toolbar_" n)) T)
  (setq Name (XML-Add-Child Toolbar "Name"))
  (XML-Add-Attribute Name "xlate" "true")
  (XML-Add-Attribute Name "UID" (strcat "geo_tools_toolbar_n_" n))
  (api-error 'vlax-put-property (list Name 'text (car input-list)) T)
  
  (foreach z (cdr input-list)
    (add-toolbar-button-to-cui Toolbar z numerik-list-macro)
    )
  
  (XML-Save o-doc)
  (foreach x (list alias name o-doc toolbar toolbarroot)
    (vlax-release-object x)
    )
  (princ)
  )

(defun add-toolbar-button-to-cui (toolbar number numerik-list-macro / MENUITEM N NAME TOOLBARBUTTON MACROREF)
  ; добавляет кнопки на панель инструментов
  (setq n (convert-numder-to-string-and-add-nul *num-buton* 4))
  (setq *num-buton* (1+ *num-buton*))
  (setq ToolbarButton (XML-Add-Child toolbar "ToolbarButton"))
  (XML-Add-Attribute ToolbarButton "IsSeparator" "false")
  (XML-Add-Attribute ToolbarButton "UID" (strcat "geo_tools_button_" n))
  (setq Name (XML-Add-Child ToolbarButton "Name"))
  (XML-Add-Attribute Name "xlate" "true")
  (XML-Add-Attribute Name "UID" (strcat "geo_tools_toolbar_n_" n))
  (api-error 'vlax-put-property (list Name 'text (find-macro-name-by-number-ass number numerik-list-macro)) T)
  (setq MenuItem (XML-Add-Child ToolbarButton "MenuItem"))
  (setq MacroRef (XML-Add-Child MenuItem "MacroRef"))
  (XML-Add-Attribute MacroRef "MenuMacroID" (strcat "geo_tools_macro_" (convert-numder-to-string-and-add-nul number 4)))
  
  )

(defun find-macro-name-by-number-ass (number numerik-list-macro / )
  (setq n 0)
  (while (setq item (nth n numerik-list-macro))
    (if (=(cadr(assoc "number" item))number)
      (progn
	(setq name (cadr(assoc "Name" item)))
	(setq n 10000))
      (setq n (1+ n))
      )
    )
  name
  )