(defun get_object (/ object)
  (while (= object nil)
    (setq object (car (entsel "\n������ ������: ")))
    ); while
  object
  )
