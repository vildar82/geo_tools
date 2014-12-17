(defun draw_osnap_on ()
  (if *gt_old_osnap*
  (setvar "osmode" *gt_old_osnap*))
  )