(defun draw_mtext_vla (spece str Height Width point AttachmentPoint / mtext_obj)
;;;  (setq mtext_obj (vla-AddMText spece (vlax-3D-point point) Width str))
;;;  (vla-put-AttachmentPoint mtext_obj AttachmentPoint)
;;;  (vla-put-InsertionPoint mtext_obj (vlax-3D-point point))
;;;  (vla-put-Height mtext_obj Height)
;;;  mtext_obj
  (draw-mtext-vla-by-list-property(list (cons "spece" spece)
					(cons "TextString" str)
					(cons "Height" Height)
					(cons "Width" Width)
					(cons "InsertionPoint" point)
					(cons "AttachmentPoint" AttachmentPoint)
					))
  )