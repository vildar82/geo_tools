(defun convert-intersection-lines-type-list-to-xml (/)
  (file_xml_write_be_list (strcat *geo_tools_user_dat_dir* "\\data\\intersection-lines-list.xml")(profile-get-intersection-lines-type-list))
  )