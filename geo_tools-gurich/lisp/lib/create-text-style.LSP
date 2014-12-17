(defun create-text-style (name-text-style font-file / )
  (if (not(tblsearch "STYLE" name-text-style))
    (progn
      (setq text-style  (vla-add (vla-get-TextStyles(vla-get-activedocument(vlax-get-acad-object))) name-text-style))
      (vla-put-fontfile text-style font-file)
      (vla-put-height text-style 0.0)
      (vla-put-obliqueangle text-style 0)
      (vla-put-width text-style 1.0)

      T
      )
    nil
    )
  )