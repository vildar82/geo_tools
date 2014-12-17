(defun convert-punkt-recorg-to-string (punkt-recorg /)
  (convert-list-to-string-be-delimiter
    (list
      (if(XML-Get-Attribute punkt-recorg "name" "")(XML-Get-Attribute punkt-recorg "name" "")"")
      (if(XML-Get-Attribute punkt-recorg "N" "")(XML-Get-Attribute punkt-recorg "N" "")"")
      (if(XML-Get-Attribute punkt-recorg "E" "")(XML-Get-Attribute punkt-recorg "E" "")"")
      (if(XML-Get-Attribute punkt-recorg "Z" "")(XML-Get-Attribute punkt-recorg "Z" "")"")
      (if(XML-Get-Attribute punkt-recorg "type" "")(XML-Get-Attribute punkt-recorg "type" "")"")
      (if(XML-Get-Attribute punkt-recorg "address" "")(XML-Get-Attribute punkt-recorg "address" "")"")
      (if(XML-Get-Attribute punkt-recorg "year" "")(XML-Get-Attribute punkt-recorg "year" "")"")
      (if(XML-Get-Attribute punkt-recorg "comment" "")(XML-Get-Attribute punkt-recorg "comment" "")"")
      )"|" )
  )