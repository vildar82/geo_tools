(defun format_check_text_style (); проверяет присутствие текстовых стилей в рисунке
  (setvar "CMDECHO" 0)
  (setvar "NOMUTT" 1)
  (if
    (not (tblobjname "dimstyle" "OTI"))
    (command "_style" "OTI" "P131.TTF" "0" "1" "0" "_n" "_n")
    )
  (if
    (not (tblobjname "dimstyle" "OTI"))
    (command "_style" "OTI" "D431.TTF" "0" "1" "0" "_n" "_n")
    )
  (if
    (not (tblobjname "dimstyle" "OTI"))
    (command "_style" "OTI" "BM431.TTF" "0" "1" "0" "_n" "_n")
    )
  (if
    (not (tblobjname "dimstyle" "OTI"))
    (command "_style" "OTI" "ESKDU.shx" "0" "1" "0" "_n" "_n")
    )
  (setvar "NOMUTT" 0)
  (setvar "CMDECHO" 1)
  ); format_check_text_style
