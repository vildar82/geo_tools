(defun command-chek (commanda / name-fun put-fun)
  ; �������� ���� �� ���� � ��������� ��������� ������� ���� ��� ��������� � ����������� ����������
  (setq name-fun(extract-function-of-macro (substr(get-item-by-name-in-list commanda "macro") 5)))
  (setq put-fun(strcat *geo_tools_root_dir* "\\helps\\txt\\ToolPalettes\\InDetail\\"name-fun".html"))
  (if (not(file-p put-fun))
    (file_write_list_to_file put-fun
      (list
	(get-item-by-name-in-list commanda "name")
	(strcat "<p>"(get-item-by-name-in-list commanda "description")"</p>")
	)
      )
    )
  )