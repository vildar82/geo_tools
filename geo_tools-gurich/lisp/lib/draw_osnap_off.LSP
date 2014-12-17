(defun draw_osnap_off ()
  (setq *gt_old_osnap* (getvar "osmode"))
  (setvar "osmode" 0)
  )