(defun C:create_html_help (/ temp palletes button-list html-dir tool-palettes-dir in-detail-dir
			 num-palettes commanda list-pallete-strings name-pallete
			 fun in-detail-dir-txt list-html-file list-txt-file gif tool-palettes-dir-txt txt-dir
			 in-instr-dir in-instr-dir-txt)
  ;создание справки в HTML формате на основк txt и данных палитр инструментов
  (setq temp (tool-pallete-to-ass-list))
  (setq palletes (nth 0 temp))
  (setq button-list (nth 1 temp))
  (setq html-dir (strcat *geo_tools_root_dir* "\\helps\\HTML"))
  (setq txt-dir (strcat *geo_tools_root_dir* "\\helps\\txt"))
  (setq tool-palettes-dir (strcat *geo_tools_root_dir* "\\helps\\HTML\\ToolPalettes"))
  (setq tool-palettes-dir-txt (strcat *geo_tools_root_dir* "\\helps\\txt\\ToolPalettes"))
  (setq in-detail-dir (strcat *geo_tools_root_dir* "\\helps\\HTML\\ToolPalettes\\InDetail"))
  (setq in-detail-dir-txt (strcat *geo_tools_root_dir* "\\helps\\txt\\ToolPalettes\\InDetail"))
  (foreach x (list html-dir tool-palettes-dir in-detail-dir)(folder-p-and-create x))
  (if (file-p (strcat *geo_tools_root_dir* "\\helps\\txt\\style.css"))
    (file-p-and-copy (strcat html-dir "\\style.css")
      (strcat *geo_tools_root_dir* "\\helps\\txt\\style.css"))
    )
  (foreach gif (list "acad" "macr" "user")
    (if (file-p (strcat in-detail-dir-txt "\\" gif ".gif"))
      (file-p-and-copy (strcat in-detail-dir "\\" gif ".gif")
	(strcat in-detail-dir-txt "\\" gif ".gif"))
      )
    )
  (file-p-and-copy (strcat tool-palettes-dir "\\01.png")(strcat tool-palettes-dir-txt "\\01.png"))
  
  (setq num-palettes 0)
  (foreach y palletes
    (setq num-palettes (1+ num-palettes))
    (setq name-pallete (get-item-by-name-in-list y "name-pallete"))
    
    (if (/= num-palettes 1)
      (progn
	(setq list-pallete-strings
	       (list
		 "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
		 "<html>"
		 "<head>"
		 "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1251\">"
		 "<title>"
		 name-pallete
		 "</title>"
		 "<link rel=\"stylesheet\" type=\"text/css\" href=\"../style.css\">"
		 "</head>"
		 "<body>"
		 "<table>"
		 )
	      )
	(foreach z (get-item-by-name-in-list y "pallete-commands")
	  (setq commanda (nth z button-list))
	  (setq fun (extract-function-of-macro (substr(get-item-by-name-in-list commanda "macro") 5)))
	  (setq list-pallete-strings
		 (append list-pallete-strings
			 (list
			   "<tr>"
			   (strcat "<td><IMG class=\"icon\" src=\"" (get-item-by-name-in-list commanda "image") "\" width=\"32\" height=\"32\" alt=\""
				   (get-item-by-name-in-list commanda "name")  "\"></td>")
			   (strcat "<td><p>"
				   (if (= fun "tpnavigate")"<a href=\"01.html\" >" "")
				   "<span class=\"bold\">" (get-item-by-name-in-list commanda "name") "</span>"
				   (if (= fun "tpnavigate")"</a>" "")
				   "</p><p>" (get-item-by-name-in-list commanda "description")
				   " <a href=\"InDetail/" fun ".html\" >Детали</a></p></td>")
			   "</tr>"
			   )
			 )
		)
	  (file-p-and-copy (strcat tool-palettes-dir "\\"(get-item-by-name-in-list commanda "image"))
	    (strcat *geo_tools_root_dir* "\\ToolPalette\\Palettes\\Images\\"(get-item-by-name-in-list commanda "image")))
	  (command-chek commanda)
	  (create-html-article (strcat in-detail-dir-txt "\\" fun ".html") in-detail-dir 2 nil)
	  
	  )
	(setq list-pallete-strings
	       (append list-pallete-strings
		       (list
			 "</table>"
			 "</body>"
			 "</html>"
			 )
		       )
	      )
	)
      (progn
	(setq list-pallete-strings
	       (list
		 "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
		 "<html>"
		 "<head>"
		 "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1251\">"
		 "<title>"
		 name-pallete
		 "</title>"
		 "<link rel=\"stylesheet\" type=\"text/css\" href=\"../style.css\">"
		 "</head>"
		 "<body>"
		 "<img src=\"01.png\" alt=\"Главная палитра\"
		 usemap=\"#Navigation\"></div>
   <p><map id=\"Navigation\" name=\"Navigation\">
   <area shape=\"rect\" COORDS=\"42, 14, 250, 38\" href=\"02.html\" alt=\"Пункты геодезические\">
   <area shape=\"rect\" COORDS=\"42, 45, 250, 69\" href=\"03.html\" alt=\"Дороги\">
   <area shape=\"rect\" COORDS=\"42, 76, 250, 100\" href=\"04.html\" alt=\"Здания, строения\">
   <area shape=\"rect\" COORDS=\"42, 107, 250, 162\" href=\"05.html\" alt=\"Заборы, границы\">
   <area shape=\"rect\" COORDS=\"42, 169, 250, 224\" href=\"06.html\" alt=\"Шрифты, подписи\">
   <area shape=\"rect\" COORDS=\"42, 231, 250, 286\" href=\"07.html\" alt=\"Столбы\">
   <area shape=\"rect\" COORDS=\"42, 293, 250, 317\" href=\"08.html\" alt=\"Люки\">
   <area shape=\"rect\" COORDS=\"42, 324, 250, 348\" href=\"09.html\" alt=\"Коммуникации и сооружения при них\">
   <area shape=\"rect\" COORDS=\"42, 355, 250, 410\" href=\"10.html\" alt=\"Расстительность\">
   <area shape=\"rect\" COORDS=\"42, 417, 250, 441\" href=\"11.html\" alt=\"Рельеф\">
   <area shape=\"rect\" COORDS=\"42, 448, 250, 472\" href=\"12.html\" alt=\"Поверхность\">
   <area shape=\"rect\" COORDS=\"42, 479, 250, 503\" href=\"13.html\" alt=\"Импорт, Экспорт\">
   <area shape=\"rect\" COORDS=\"42, 510, 250, 534\" href=\"14.html\" alt=\"Прочие\">
   <area shape=\"rect\" COORDS=\"42, 541, 250, 565\" href=\"15.html\" alt=\"Проект\">
   <area shape=\"rect\" COORDS=\"42, 572, 250, 596\" href=\"16.html\" alt=\"Предпрофильные кнопки\">
   <area shape=\"rect\" COORDS=\"42, 603, 250, 627\" href=\"17.html\" alt=\"Профиль\">
   <area shape=\"rect\" COORDS=\"42, 634, 250, 658\" href=\"18.html\" alt=\"ПолиИнструменты\">
   <area shape=\"rect\" COORDS=\"42, 665, 250, 689\" href=\"19.html\" alt=\"Гидрография\">
   <area shape=\"rect\" COORDS=\"42, 696, 250, 720\" href=\"20.html\" alt=\"Оформление\">
   <area shape=\"rect\" COORDS=\"42, 727, 250, 751\" href=\"21.html\" alt=\"Формат\">
   <area shape=\"rect\" COORDS=\"42, 758, 250, 782\" href=\"22.html\" alt=\"Настройки\">
   </map></p>"
		 "</body>"
		 "</html>"
		 )
	      )
	)
      )
    (file_write_list_to_file
      (strcat tool-palettes-dir "\\" (if (< num-palettes 10)"0" "")(itoa num-palettes) ".html")
      list-pallete-strings)
    (setq list-pallete-strings nil)
    )
  (setq in-instr-dir (strcat *geo_tools_root_dir* "\\helps\\HTML\\Instructions"))
  (setq in-instr-dir-txt (strcat *geo_tools_root_dir* "\\helps\\txt\\Instructions"))
  (folder-p-and-create in-instr-dir)
  (foreach file(vl-directory-files in-instr-dir-txt "*.html" 1)
    (create-html-article (strcat in-instr-dir-txt "\\" file ) in-instr-dir 1 T)
    )
  (foreach file(vl-directory-files txt-dir "*.html" 1)
    (if (and (/= "index.html" (substr file 4))
	     (/= "download_geo_tools.html" (substr file 4))
	     (/= "instructions.html" (substr file 4))
	     (/= "ToolPalettes.html" (substr file 4)))
      (create-html-article (strcat txt-dir "\\" file ) html-dir 0 T)
      )
    )
  
  (princ)
  )