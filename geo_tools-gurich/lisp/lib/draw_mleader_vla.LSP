(defun draw_mleader_vla (spece str ht t1 t2 / mleader_obj pt_array); создает мультивыноску
  (load_global_variable)
  (setq pt_array (convert_coords_list_from_safearray_mleader (list t1 t2)))
  (setvar "CMLEADERSTYLE" "Подписи")
  (setq mleader_obj (vla-addmleader model_spece pt_array 0))
  (vla-put-StyleName mleader_obj "Подписи")
  (vla-put-textstring mleader_obj str)
  (vla-put-TextHeight mleader_obj ht)
  mleader_obj
  )
