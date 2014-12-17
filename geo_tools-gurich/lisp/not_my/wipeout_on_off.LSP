(defun C:wipeout_on_off    (/ wipeout_frame_key wipeout_frame_on pr)
  (setq    wipeout_frame_key
     (cdr (assoc 70
             (dictsearch
               (namedobjdict)
               "ACAD_WIPEOUT_VARS"
             )
          )
     )
  );;setq
  (cond    ((= 1 wipeout_frame_key)
     (progn
       (setq pr (dictsearch (namedobjdict) "ACAD_WIPEOUT_VARS")
         pr (subst (cons 70 0) (assoc 70 pr) pr)
       )
       (entmod pr)
       (setq wipeout_frame_on "ВЫКЛ")
     )
    );;1
    ((= 0 wipeout_frame_key)
     (progn
       (setq pr (dictsearch (namedobjdict) "ACAD_WIPEOUT_VARS")
         pr (subst (cons 70 1) (assoc 70 pr) pr)
       )
       (entmod pr)
       (setq wipeout_frame_on "ВКЛ")
     )
    );;2
    ((eq nil wipeout_frame_key)
     (setq wipeout_frame_on "Неуст.")
    );;3
  )
  (setvar "MODEMACRO" (strcat "Маскировка " wipeout_frame_on))
  (vl-cmdf "_regen")
  (princ)
)
;;end defun