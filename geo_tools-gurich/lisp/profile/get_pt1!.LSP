(defun get_pt1! (/ t1!_); запрос на ввод точки на условном горизонте
  (if
    (eq t1! nil)
    (progn
      (initget)(setq t1! (getpoint "\n”кажи точку на условном горизонте: "))
      )
    (progn
      (setq t1!_ t1!)
      (initget)
      (setq t1! (getpoint "\n”кажи точку на условном горизонте <ѕ–≈∆Ќяя>: "))
      (if (eq t1! nil)
	(setq t1! t1!_)
	)
      )
    )
  t1!
  )
