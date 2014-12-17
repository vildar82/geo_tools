  (vlax-invoke-method
    (setq ws (vlax-get-or-create-object "wscript.shell"))
    "run"
    (strcat "notepad.exe " "test.txt")
    1
    :vlax-true ;_дождаться завершения сеанса
  ) ;_ end of vlax-invoke-method
  (vlax-release-object ws)