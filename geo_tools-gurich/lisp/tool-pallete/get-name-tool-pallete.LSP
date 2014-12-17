(defun get-name-tool-pallete (tool-pallete / name)
  (if (=(setq name (XML-Get-Child-Value (XML-Get-Child tool-pallete nil "Properties") nil "ItemName"))nil)(setq name ""))
  name
  )