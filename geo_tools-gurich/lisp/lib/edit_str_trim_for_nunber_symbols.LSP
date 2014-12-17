(defun edit_str_trim_for_nunber_symbols (str nunber nil-left/-T-right /)
  ; уменьшает выбранную строку до указанного числа символов, может работать слева и справа
  (while (< nunber (strlen str))
    (if nil-left/-T-right
      (setq str (substr str 1 (1-(strlen str))))
      (setq str (substr str 2 (strlen str)))
      )
    )
  str
  )