(defun convert-data-format-acssos-list-to-string (acssos-list / )
  (strcat
    (itoa(get-item-by-name-in-list acssos-list "year"))
    "."
    (if(<(get-item-by-name-in-list acssos-list "month")10)"0""")
    (itoa(get-item-by-name-in-list acssos-list "month"))
    "."
    (if(<(get-item-by-name-in-list acssos-list "day")10)"0""")
    (itoa(get-item-by-name-in-list acssos-list "day"))
    )
  )