(defun get-server-auto-update-flag ()
  (if (=(get-item-by-name-in-list(get-geo_tools_options-list) "server-auto-update-flag")"1")
    t
    nil)
  )