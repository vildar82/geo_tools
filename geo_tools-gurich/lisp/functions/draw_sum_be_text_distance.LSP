(defun C:draw_sum_be_text_distance (/ text Summ S )
(geo_tools-help "draw_sum_be_text_distance")
  (initget)(setq text (car (entsel "\n������ ����� � �����������: ")))
  
  (while (not (eq text nil))
    (setq S (atof(edit_text_clean_for_number(cdr (assoc 1 (entget text))))))
    (if
      (eq summ nil)
      (setq summ 0)
      )
    (setq summ (+ summ S))
    (initget)(setq text (car (entsel (strcat "\n����� = " (rtos summ 2 2)  "�. ������ ����� � ����������� <Enter - ����������> : "))))
    )
  (initget)(setq t1(getpoint (strcat "\n����� = " (rtos summ 2 2) "�. ����� ���� �������� ����� <����������> : ")))
  (if
    (not (eq t1 nil))
    (entmake (list '(0 . "TEXT")  (cons 10 t1) (cons 40 (/ 3.4 (getvar "CANNOSCALEVALUE"))) (cons 1 (strcat "\U+03A3=" (rtos summ 2 2)"�")) (cons 7 "Standard")))
    )
  (princ)
  )
