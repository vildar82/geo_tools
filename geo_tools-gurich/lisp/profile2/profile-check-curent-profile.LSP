(defun profile-check-curent-profile (curent-profile / result)
  (if curent-profile
    (if (vlax-erased-p (cdr curent-profile))
      (alert "Ошибка! Текущий профиль был удален.\nВыбрать другой текущий профиль можно с помощью команды \"Выбрать текущий профиль\"")
      (setq result T)
      )
    (alert "Ошибка! Текущий профиль не определен.\nВыбрать текущий профиль можно с помощью команды \"Выбрать текущий профиль\"")
    )
  result
  )