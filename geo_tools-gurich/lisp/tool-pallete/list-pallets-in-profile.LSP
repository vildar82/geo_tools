(defun list-pallets-in-profile ( / catalogview o-doc result)
  (setq o-doc (XML-Get-XMLObject (strcat(vl-filename-directory(vla-get-MenuFile(vla-get-files(vla-get-Preferences(vlax-get-acad-object)))))"\\Profiles\\Профиль без имени\\Profile.aws")))
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
  (vlax-release-object o-doc)
  result
  )