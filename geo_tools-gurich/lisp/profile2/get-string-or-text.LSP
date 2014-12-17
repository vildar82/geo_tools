(defun get-string-or-text(/ flag obj string)
  (load_global_variable)
  (setq flag T)
  (while flag
    (setq obj (car(entsel "\nУкажи объект типа текст<ввести>: ")))
    (cond
      ((eq obj nil)
       (setq string (getstring t "\nВведи текст пересечки: "))
       (setq flag nil)
       )
      ((eq (cdr(assoc 0 (entget obj))) "TEXT")
       (setq string(cdr(assoc 1(entget obj))))
       (setq flag nil)
       )
      (T
       (alert "Ошибка! Выбран не объект не допустимого типа. Допускаются однострочный текст и точки COGO. Попробуй снова.")
       )
      )
    )
  string
  )