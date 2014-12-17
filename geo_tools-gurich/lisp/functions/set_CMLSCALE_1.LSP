(defun C:set_CMLSCALE_1 ()
  (setvar "CMLSCALE" (/ 1 (getvar "CANNOSCALEVALUE")))
  (princ)
  )