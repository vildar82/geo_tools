(defun draw_lines (list_granic n / p1 p2 granica)
  (repeat (length list_granic)
    (setq granica (car list_granic))
    (setq list_granic (cdr list_granic))
    (setq p1 (list(car granica)(+(nth 1 t1!)(/ (+ 10 n) (/ 1000 mg)))))
    (setq p2 (list(cadr granica)(+(nth 1 t1!)(/ (+ 10 n) (/ 1000 mg)))))
    (entmake (list '(0 . "LINE") '(6 . "Continuous") (cons 10 p1) (cons 11 p2)))
    )
  );defun
