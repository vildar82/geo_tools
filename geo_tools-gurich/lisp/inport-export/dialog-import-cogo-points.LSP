(defun dialog-import-cogo-points (/ dcl_id last-selekt result last-selekt list-dialog-import-cogo-points last-selekt)
  ; диалог параметров импортируемого файла и настроек вставки
  (defun mode_tile-flag (/)
    (setq list-toggle(list "upload" "input" "calculation" "unknown" "layer-name"))
    (if (= (get_tile "format") "0")
      (foreach x list-toggle
	(mode_tile x 0))
      (foreach x list-toggle
	(mode_tile x 1)
	)))
  (setq dcl_id (geo-tools-load-dialog "d_import_cogo_points"))
  (setq template
	 (list
	   (list "upload" "0")
	   (list "input" "0")
	   (list "calculation" "1")
	   (list "unknown" "1")
	   (list "layer-name" "1")
	   (list "format" "0")))
  (if (not(setq list-dialog-import-cogo-points (vlax-ldata-get "geo_tools-data-dialogs" "dialog-import-cogo-points")))
    (setq list-dialog-import-cogo-points template)
    (if(not(get-item-by-name-in-list list-dialog-import-cogo-points "layer-name"))
      (setq list-dialog-import-cogo-points template)))
  (new_dialog "d_import_cogo_points" dcl_id)
  (foreach x list-dialog-import-cogo-points
    (set_tile (car x) (cadr x)))
  (mode_tile-flag)
  (action_tile "format"(vl-prin1-to-string'(mode_tile-flag)))
  (action_tile "accept"
    (vl-prin1-to-string
      '(progn
	(foreach x list-dialog-import-cogo-points
	 (setq result(append result (list (list (car x)(get_tile (car x))))))
	 )
	(done_dialog 1)
	)))
  (start_dialog)
  (unload_dialog dcl_id)
  (if result(vlax-ldata-put "geo_tools-data-dialogs" "dialog-import-cogo-points" result))
  result
  )