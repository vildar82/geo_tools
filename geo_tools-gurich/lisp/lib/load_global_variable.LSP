(defun load_global_variable (/ var-acad-ver); начало работы с расширением lisp'а
  (setq var-acad-ver (getvar "ACADVER"))
  (setq *file-system-object*(vlax-get-or-create-object "Scripting.FileSystemobject"))
  (setq acad_application (vlax-get-acad-object))
  (if
    (vl-catch-all-error-p
      (setq C3D_Application (vl-catch-all-apply
			      'vla-getInterfaceObject
			      (list
				acad_application
				(cond
				  ((eq var-acad-ver "17.1s (LMS Tech)")"AeccXUiLand.AeccApplication.5.0"); Acad 2008
				  ((eq var-acad-ver "17.2s (LMS Tech)")"AeccXUiLand.AeccApplication.6.0"); Acad 2009
				  ((eq var-acad-ver "18.0s (LMS Tech)")"AeccXUiLand.AeccApplication.7.0"); Acad 2010
				  ((eq var-acad-ver "18.1s (LMS Tech)")"AeccXUiLand.AeccApplication.8.0"); Acad 2011
				  ((eq var-acad-ver "18.2s (LMS Tech)")"AeccXUiLand.AeccApplication.9.0"); Acad 2012
				  ((eq var-acad-ver "19.0s (LMS Tech)")"AeccXUiLand.AeccApplication.10.0"); Acad 2013
				  )
				)))
      )
    (progn
      (setq C3D_Application nil)
      )
    (progn
      (setq C3D_active_document (vla-get-ActiveDocument C3D_Application))
      (setq C3D_points (vlax-get C3D_active_document 'points))
      )
    )
  (setq active_document (vla-get-ActiveDocument acad_application))
  (setq layouts_document (vla-get-Layouts active_document))
  (setq model_spece (vla-get-ModelSpace active_document))
  (setq paper_spece (vla-get-PaperSpace active_document))
  (setq blocks_spece (vla-get-Blocks active_document))
  (princ)
  )