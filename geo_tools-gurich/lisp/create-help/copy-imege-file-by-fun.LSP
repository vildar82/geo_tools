(defun copy-imege-file-by-fun (put-txt-file-fun output-folder / file-name list-files put-image-folder)
  ; копирует из txt папки с описаниями функций относящиеся к ней картинки
  (setq put-image-folder (vl-filename-directory put-txt-file-fun))
  (setq file-name (vl-filename-base put-txt-file-fun))
  (setq list-files (vl-directory-files put-image-folder (strcat file-name "*.*") 1))
  (foreach x list-files
    (if (/=(vl-filename-extension x)".html")
      (file-p-and-copy (strcat output-folder "\\" x)
	(strcat put-image-folder "\\" x))
      )
    )
  )