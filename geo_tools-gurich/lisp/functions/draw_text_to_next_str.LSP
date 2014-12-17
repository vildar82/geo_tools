(defun C:draw_text_to_next_str ( / text str ttt t1); продолжает выбранный текст следующей строкой
(geo_tools-help "draw_text_to_next_str")
  (while (= text nil)
    (setq text (car (entsel "\nВыбери текст: ")))
    (if
      (not (= text nil))
      (if
	(not (or (eq (cdr (assoc 0 (entget text))) "TEXT")(eq (cdr (assoc 0 (entget text))) "MTEXT")))
	(progn
	  (setq text nil)
	  (princ "\nОШИБКА! Выбран объект неподдерживаемого типа!")
	  ); progn
	); if
      (princ "\nОШИБКА! Текст не выбран!")
      ); if
    ); while
  (setq ttt (entget text))
      (if
	(eq (cdr(assoc 0 ttt)) "MTEXT")
	(setq p (+ (/ (cdr (assoc 40 ttt)) 2) (cdr (assoc 40 ttt))))
	(setq p (calculation_text_long_be_dxf_code ttt))
	)
  (setq str "1")
  (setq t1 (polar (cdr (assoc 10 ttt)) (- (cdr (assoc 50 ttt))(/ pi 2)) (+(cdr (assoc 40 ttt))(/(cdr (assoc 40 ttt))2))))
  (while (not (= str nil))
    (initget)(setq str (getstring T "\nВведи текст: "))
    (if (= str "")
      (progn
      (setq str nil)
      );progn
      (progn
        (entmake (list '(0 . "TEXT")
		       '(6 . "Continuous")
		       (cons 8 (cdr (assoc 8 ttt)))
		       (cons 10 t1)
		       (cons 40 (cdr (assoc 40 ttt)))
		       (cons 1 str)
		       (cons 50 (cdr (assoc 50 ttt)))
		       (cons 7 (cdr (assoc 7 ttt)))
		       )
		 )
        (setq t1 (polar
		   t1
		   (- (cdr (assoc 50 ttt))(/ pi 2))
		   (+(cdr (assoc 40 ttt))(/(cdr (assoc 40 ttt))2))
		   )
	      )
	);progn
      );if
    );while
  (princ)
  )
