(defun _ed-blocks->norm   ()
  (defun _kpblc-ent-modify-autoregen (ent    bit       value
                  ext_regen    /       ent_list
                  old_dxf    new_dxf    layer_dxf70
                  )
    (if   (not
     (and
       (or
         (= (cdr (assoc 0 (entget ent))) "STYLE")
         (= (cdr (assoc 0 (entget ent))) "DIMSTYLE")
         (= (cdr (assoc 0 (entget ent))) "LAYER")
         ) ;_ end of or
       (= bit 100)
       ) ;_ end of and
     ) ;_ end of not
      (progn
   (setq ent_list (entget ent)
         new_dxf  (cons bit
              (if (and (= bit 62) (= (type value) 'str))
                (if (= (strcase value nil) "BYLAYER")
             256
             0
             ) ;_ end of if
                value
                ) ;_ end of if
              ) ;_ end of cons
         ) ;_ end of setq
   (if (not (equal new_dxf (setq old_dxf (assoc bit ent_list))))
     (progn
       (entmod (if   old_dxf
            (subst new_dxf old_dxf ent_list)
            (append ent_list (list new_dxf))
            ) ;_ end of if
          ) ;_ end of entmod
       (if   ent_regen
         (entupd ent)
         (redraw ent)
         ) ;_ end of if
       ) ;_ end of progn
     ) ;_ end of if
   ) ;_ end of progn
      ) ;_ end of if
    ent
    ) ;_ end of defun
  (if (not *kpblc-activedoc*)
    (setq *kpblc-activedoc* (vla-get-activedocument (vlax-get-acad-object)))
    ) ;_ end of if
  (vlax-for item (vla-get-layers *kpblc-activedoc*)
    (if   (equal (vla-get-lock item) :vlax-true)
      (vla-put-lock item :vlax-false)
      ) ;_ end of if
    ) ;_ end of vlax-for
  (vla-startundomark *kpblc-activedoc*)
  (vlax-for item (vla-get-blocks *kpblc-activedoc*)
    (if   (not (wcmatch (vla-get-name item) "*Space*"))
      (progn
   (vlax-for item2   item
     (vla-put-layer item2 "0")
     (vla-put-linetype item2 "Byblock")
     (vla-put-lineweight item2 -2)
     (_kpblc-ent-modify-autoregen
       (vlax-vla-object->ename item2)
       62
       "byblock"
       nil
       ) ;_ end of _kpblc-ent-modify-autoregen
     ) ;_ end of vlax-for
   ) ;_ end of progn
      ) ;_ end of if
    ) ;_ end of vlax-for
  (vla-regen *kpblc-activedoc* acallviewports)
  (vla-endundomark *kpblc-activedoc*)
  (princ)
  ) ;_ end of defun