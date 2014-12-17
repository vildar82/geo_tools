(defun move-child-be-number-to-end (parent number / ChildList)
  (XML-Add-Child parent "zaglushka")
  (setq ChildList(XML-Get-ChildList parent))
  (vlax-invoke-method parent 'replaceChild
    (nth number ChildList)
    (nth (1-(length ChildList)) ChildList))
  )