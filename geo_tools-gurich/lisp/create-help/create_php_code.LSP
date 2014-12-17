(defun C:create_php_code (/ button-list list-menu palletes php-dir php-in-detail-dir php-instr-palettes-dir php-tool-palettes-dir
			temp txt-dir txt-in-detail-dir txt-instr-palettes-dir txt-tool-palettes-dir php-php-dir txt-php-dir)
  (setq temp (tool-pallete-to-ass-list))
  (setq palletes (nth 0 temp))
  (setq button-list (nth 1 temp))
  (setq php-dir (strcat *geo_tools_root_dir* "\\helps\\PHP"))
  (setq txt-dir (strcat *geo_tools_root_dir* "\\helps\\txt"))
  (setq php-php-dir (strcat *geo_tools_root_dir* "\\helps\\PHP\\php"))
  (setq txt-php-dir (strcat *geo_tools_root_dir* "\\helps\\txt\\php"))
  (setq php-tool-palettes-dir (strcat *geo_tools_root_dir* "\\helps\\PHP\\ToolPalettes"))
  (setq txt-tool-palettes-dir (strcat *geo_tools_root_dir* "\\helps\\txt\\ToolPalettes"))
  (setq php-instr-palettes-dir (strcat *geo_tools_root_dir* "\\helps\\PHP\\Instructions"))
  (setq txt-instr-palettes-dir (strcat *geo_tools_root_dir* "\\helps\\txt\\Instructions"))
  (setq php-in-detail-dir (strcat *geo_tools_root_dir* "\\helps\\PHP\\ToolPalettes\\InDetail"))
  (setq txt-in-detail-dir (strcat *geo_tools_root_dir* "\\helps\\txt\\ToolPalettes\\InDetail"))
  (foreach x (list php-dir php-php-dir php-tool-palettes-dir php-instr-palettes-dir php-in-detail-dir)(folder-p-and-create x))
  (foreach x (list "\\style.css" "\\mail.png")
  (if (file-p (strcat txt-dir x))
    (file-p-and-copy (strcat php-dir x)
      (strcat txt-dir x)))
    )
  (foreach x (vl-directory-files txt-php-dir "*.*" 1)
    (file-p-and-copy (strcat php-php-dir "\\" x)
      (strcat txt-php-dir "\\" x))
    )
  (foreach gif (list "acad" "macr" "user")
    (if (file-p (strcat txt-in-detail-dir "\\" gif ".gif"))
      (file-p-and-copy (strcat php-in-detail-dir "\\" gif ".gif")
	(strcat txt-in-detail-dir "\\" gif ".gif"))))
  (file-p-and-copy (strcat php-tool-palettes-dir "\\01.png")(strcat txt-tool-palettes-dir "\\01.png"))
  (setq list-menu (get-menu-by-dir txt-dir 0))
  (foreach file(vl-directory-files txt-dir "*.html" 1)
    (create-php-article (strcat txt-dir "\\" file ) php-dir 0 T list-menu palletes))
  (setq list-menu (get-menu-by-dir txt-dir 1))
  (foreach file(vl-directory-files txt-instr-palettes-dir "*.html" 1)
    (create-php-article (strcat txt-instr-palettes-dir "\\" file ) php-instr-palettes-dir 1 T list-menu nil))
  (create-php-tool-palettes-and-detail txt-dir php-tool-palettes-dir php-in-detail-dir txt-in-detail-dir palletes button-list)
  (foreach x (list "draw_layout" "draw_profile" "dwt" "errors" "install_geo_tools" "user_data" )(create-php-redirect x php-dir))
  (princ)
  )

(defun get-menu-by-dir (input-dir deep / list-php str-deep)
  ; сканирует доректорию и на основе файлов создает каталог меню
    (setq str-deep "")
  (if (> deep 0)
    (repeat deep
      (setq str-deep (strcat str-deep "../"))
      )
    )
  (setq list-php (list "<div class=\"menu\">"))
  (foreach x(vl-directory-files input-dir "*.html" 1)
    (setq list-php (append list-php
			   (list
			     (strcat "<p><a href=\""str-deep(substr(vl-filename-base x)4)".php\" >"(car (file_read_to_list (strcat input-dir "\\" x)))"</a></p>")
			     )))
    )
  (append list-php(list"</div>"))
  )

(defun create-php-article (txt-put-file ohtput-put deep flag-cut-name list-menu palletes / list-php-file list-txt-file str-deep list-temp num-palettes x)
;;;  (terpri)(princ (if flag-cut-name (substr(vl-filename-base txt-put-file)4)(vl-filename-base txt-put-file)))
  (setq list-txt-file (file_read_to_list txt-put-file))
  (setq str-deep "")
  (if (> deep 0)
    (repeat deep
      (setq str-deep (strcat str-deep "../"))
      )
    )
  (setq list-php-file
	 (append
	   (list
	     (strcat"<?php include \"" str-deep "php/lib.int.php\";?>")
	     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
	     "<html>"
	     "<head>"
	     "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1251\">"
	     "<title>"
	     (car list-txt-file)
	     "</title>"
	     
	     (strcat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" str-deep "style.css\">")
	     "</head>"
	     "<body>"
	     "<div class=\"all\">"
	     "<?php draw_header();?>"
	     "<div class=\"text\">"
	     (strcat"<h3>"(car list-txt-file)"</h3>")
	     )
	   
	   (cond
	     ((=(if flag-cut-name (substr(vl-filename-base txt-put-file)4)(vl-filename-base txt-put-file))"instructions")
	      (setq list-temp (list "<div class=\"links\">"))
	      (foreach x(vl-directory-files (strcat(vl-filename-directory txt-put-file)"\\Instructions") "*.html" 1)
		(setq list-temp (append list-temp
					(list
					  (strcat "<p><a href=\"""Instructions/"(substr(vl-filename-base x)4)".php\" >"
						  (car (file_read_to_list (strcat(vl-filename-directory txt-put-file)"\\Instructions\\" x)))"</a></p>")
					  )))
		)
	      (append list-temp(list"</div>"))
	      )
	     ((=(if flag-cut-name (substr(vl-filename-base txt-put-file)4)(vl-filename-base txt-put-file))"ToolPalettes")
	      (setq list-temp (list "<div class=\"links\">"))
	      (setq num-palettes 0)
	      (foreach y palletes
		(setq num-palettes (1+ num-palettes))
		(setq list-temp (append list-temp
					(list
					  (strcat "<p><a href=\"""ToolPalettes/" (if (< num-palettes 10)"0" "")(itoa num-palettes) ".php\" >"
						  (get-item-by-name-in-list y "name-pallete")"</a></p>")
					  )))
		)
	      (append list-temp(list"</div>"))
	      )
	     (T
	      (mapcar '(lambda (x) (edit-string-replase-str ".php\"" ".xxx\"" x)) (cdr list-txt-file))
	      )
	     )
	   
	   (list
	     "</div>"
	     )
	   list-menu
	   (list
	     (strcat"<div class=\"footer\"><img src=\"" str-deep "mail.png\" width=\"160\" height=\"16\" alt=\"пишите письма\"/></div>")
	     "</div>"
	     "</body>"
	     "</html>"
	     )
	   )
	)
  (if flag-cut-name
    (progn
      (setq txt-put-file (strcat (vl-filename-directory txt-put-file)"\\"(substr(vl-filename-base txt-put-file)4) (vl-filename-extension txt-put-file)))
      )
    )
  (file_write_list_to_file(strcat ohtput-put "\\" (vl-filename-base txt-put-file) ".php")list-php-file)
  (copy-imege-file-by-fun txt-put-file ohtput-put)
  )


(defun create-php-tool-palettes-and-detail (txt-dir php-tool-palettes-dir php-in-detail-dir txt-in-detail-dir palletes button-list /
					    commanda fun list-menu-level-1 list-menu-level-2 list-pallete-strings list-txt-file
					    name-pallete num-palettes)
  (setq list-menu-level-1 (get-menu-by-dir txt-dir 1))
  (setq list-menu-level-2 (get-menu-by-dir txt-dir 2))
  (setq num-palettes 0)
  (foreach y palletes
    (setq num-palettes (1+ num-palettes))
    (setq name-pallete (get-item-by-name-in-list y "name-pallete"))
    
    (if (/= num-palettes 1)
      (progn
	(setq list-pallete-strings
	       (list
		 (strcat"<?php include \"../php/lib.int.php\";?>")
		 "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
		 "<html>"
		 "<head>"
		 "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1251\">"
		 "<title>"
		 name-pallete
		 "</title>"
		 (strcat "<link rel=\"stylesheet\" type=\"text/css\" href=\"../style.css\">")
		 "</head>"
		 "<body>"
		 "<div class=\"all\">"
		 "<?php draw_header();?>"
		 "<div class=\"text\">"
		 (strcat"<h3>"name-pallete"</h3>")
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
			   (strcat "<td><IMG class=\"icon\" src=\"" (get-item-by-name-in-list commanda "image")
				   "\" width=\"32\" height=\"32\" alt=\""
				   (get-item-by-name-in-list commanda "name")  "\"></td>")
			   (strcat "<td><p>"
				   (if (= fun "tpnavigate")"<a href=\"01.php\" >" "")
				   "<span class=\"bold\">" (get-item-by-name-in-list commanda "name") "</span>"
				   (if (= fun "tpnavigate")"</a>" "")
				   "</p><p>" (get-item-by-name-in-list commanda "description")
				   " <a href=\"InDetail/" fun ".php\" >Детали</a></p></td>")
			   "</tr>"
			   )
			 )
		)
	  (file-p-and-copy (strcat php-tool-palettes-dir "\\"(get-item-by-name-in-list commanda "image"))
	    (strcat *geo_tools_root_dir* "\\ToolPalette\\Palettes\\Images\\"(get-item-by-name-in-list commanda "image")))
	  (command-chek commanda)
	  ;;;	  (create-html-article (strcat in-detail-dir-txt "\\" fun ".html") in-detail-dir 2 nil)
	  (create-php-article (strcat txt-in-detail-dir "\\" fun ".html") php-in-detail-dir 2 nil list-menu-level-2 nil)
	  )
	(setq list-pallete-strings
	       (append list-pallete-strings
		       (list
			 "</table>"
			 "</div>")
		       list-menu-level-1
		       (list
			 (strcat"<div class=\"footer\"><img src=\"../mail.png\" width=\"160\" height=\"16\" alt=\"пишите письма\"/></div>")
			 "</div>"
			 "</body>"
			 "</html>"
			 )
		       )
	      )
	)
      (progn
	(setq list-pallete-strings
	       (list
		 (strcat"<?php include \"../php/lib.int.php\";?>")
		 "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
		 "<html>"
		 "<head>"
		 "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1251\">"
		 "<title>"
		 name-pallete
		 "</title>"
		 (strcat "<link rel=\"stylesheet\" type=\"text/css\" href=\"../style.css\">")
		 "</head>"
		 "<body>"
		 "<div class=\"all\">"
		 "<?php draw_header();?>"
		 "<div class=\"text\">"
		 (strcat"<h3>"name-pallete"</h3>")
		 "<img src=\"01.png\" alt=\"Главная палитра\""
		 "usemap=\"#Navigation\">"
		 "<p><map id=\"Navigation\" name=\"Navigation\">"
		 "<area shape=\"rect\" COORDS=\"42, 14, 250, 38\" href=\"02.php\" alt=\"Пункты геодезические\">"
		 "<area shape=\"rect\" COORDS=\"42, 45, 250, 69\" href=\"03.php\" alt=\"Дороги\">"
		 "<area shape=\"rect\" COORDS=\"42, 76, 250, 100\" href=\"04.php\" alt=\"Здания, строения\">"
		 "<area shape=\"rect\" COORDS=\"42, 107, 250, 162\" href=\"05.php\" alt=\"Заборы, границы\">"
		 "<area shape=\"rect\" COORDS=\"42, 169, 250, 224\" href=\"06.php\" alt=\"Шрифты, подписи\">"
		 "<area shape=\"rect\" COORDS=\"42, 231, 250, 286\" href=\"07.php\" alt=\"Столбы\">"
		 "<area shape=\"rect\" COORDS=\"42, 293, 250, 317\" href=\"08.php\" alt=\"Люки\">"
		 "<area shape=\"rect\" COORDS=\"42, 324, 250, 348\" href=\"09.php\" alt=\"Коммуникации и сооружения при них\">"
		 "<area shape=\"rect\" COORDS=\"42, 355, 250, 410\" href=\"10.php\" alt=\"Расстительность\">"
		 "<area shape=\"rect\" COORDS=\"42, 417, 250, 441\" href=\"11.php\" alt=\"Рельеф\">"
		 "<area shape=\"rect\" COORDS=\"42, 448, 250, 472\" href=\"12.php\" alt=\"Поверхность\">"
		 "<area shape=\"rect\" COORDS=\"42, 479, 250, 503\" href=\"13.php\" alt=\"Импорт, Экспорт\">"
		 "<area shape=\"rect\" COORDS=\"42, 510, 250, 534\" href=\"14.php\" alt=\"Прочие\">"
		 "<area shape=\"rect\" COORDS=\"42, 541, 250, 565\" href=\"15.php\" alt=\"Проект\">"
		 "<area shape=\"rect\" COORDS=\"42, 572, 250, 596\" href=\"16.php\" alt=\"Предпрофильные кнопки\">"
		 "<area shape=\"rect\" COORDS=\"42, 603, 250, 627\" href=\"17.php\" alt=\"Профиль\">"
		 "<area shape=\"rect\" COORDS=\"42, 634, 250, 658\" href=\"18.php\" alt=\"ПолиИнструменты\">"
		 "<area shape=\"rect\" COORDS=\"42, 665, 250, 689\" href=\"19.php\" alt=\"Гидрография\">"
		 "<area shape=\"rect\" COORDS=\"42, 696, 250, 720\" href=\"20.php\" alt=\"Оформление\">"
		 "<area shape=\"rect\" COORDS=\"42, 727, 250, 751\" href=\"21.php\" alt=\"Формат\">"
		 "<area shape=\"rect\" COORDS=\"42, 758, 250, 782\" href=\"22.php\" alt=\"Настройки\">"
		 "</map></p>"
		 "</div>"
		 "<div class=\"menu\">"
		 "<p><a href=\"../index.php\" >Зачем нужен этот сайт</a></p>"
		 "<p><a href=\"../what_it.php\" >Что такое Geo_Tools?</a></p>"
		 "<p><a href=\"../realization.php\" >Реализация Geo_Tools</a></p>"
		 "<p><a href=\"../ver.php\" >Версия Geo_Tools</a></p>"
		 "<p><a href=\"../instructions.php\" >Инструкции</a></p>"
		 "<p><a href=\"../download_geo_tools.php\" >Скачать</a></p>"
		 "<p><a href=\"../university.php\" >Мои университеты</a></p>"
		 "<p><a href=\"../ToolPalettes.php\" >Палитры инструментов</a></p>"
		 "</div>"
		 (strcat"<div class=\"footer\"><img src=\"../mail.png\" width=\"160\" height=\"16\" alt=\"пишите письма\"/></div>")
		 "</div>"
		 "</body>"
		 "</html>"
		 )
	      )
	)
      )
    (file_write_list_to_file
      (strcat php-tool-palettes-dir "\\" (if (< num-palettes 10)"0" "")(itoa num-palettes) ".php")
      list-pallete-strings)
    (setq list-pallete-strings nil)
    )
  )

(defun create-php-redirect (filename php-dir /)
  (file_write_list_to_file
    (strcat php-dir "\\" filename ".php")
    (list
      "<?php"
      (strcat"Header( \"Location: Instructions//"filename".php\" ); ")
      "exit();"
      "?>"
      )
    )
  )

       