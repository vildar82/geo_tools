(defun file-p (patch /)
  (not
    (vl-catch-all-error-p
      (vl-catch-all-apply
	'vlax-invoke-method
	(list
	  *file-system-object*
	  'GetFile
	  patch
	  )))
    )
  )