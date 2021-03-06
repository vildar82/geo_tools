(defun folder-p-and-create (patch /)
  (if
    (vl-catch-all-error-p
      (vl-catch-all-apply
	'vlax-invoke-method
	(list
	  *file-system-object*
	  'GetFolder
	  patch
	  )))
    (not
      (vl-catch-all-error-p
	(vl-catch-all-apply
	  'vlax-invoke-method
	  (list
	    *file-system-object*
	    'CreateFolder
	    patch
	    ))))
    )
  )