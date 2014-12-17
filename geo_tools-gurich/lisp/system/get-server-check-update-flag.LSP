(defun get-server-check-update-flag ()
  (if (=(get-item-by-name-in-list(get-geo_tools_options-list) "server-check-update-flag")"1")
    t
    nil)
  )