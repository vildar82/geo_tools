(defun get-civil-or-not ( / var-acad-ver)
  ; определяет текущий AutoCad Civil или нет, отпределяет "своеобразно" по присутствию в пути к acad.exe слова "Civil"
  (if
    (vl-string-search "Civil" (vla-get-Path (vlax-get-acad-object)))
    T
    nil
    )
  )