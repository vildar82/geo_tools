(defun C:Nvx (/ ent n lst_point sys_var x)
  (setq sys_var (mapcar 'getvar '("osmode" "cmdecho")))
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)
  (if (not (numberp *NPoint*))(setq *NPoint* 1))
  (princ "\nНачальный номер <")(princ *NPoint*)(princ ">: ")
  (if (null (setq n (getint)))(setq n *NPoint*))
  (and
  (setq ent (car (entsel "\nВыберите полилинию: ")))
  (= (cdr(assoc 0 (entget ent))) "LWPOLYLINE")
  (setq  lst_point (mapcar 'cdr
        (vl-remove-if-not
          '(lambda (x) (= (car x) 10))
          (entget ent)
        ) ;_ end of vl-remove-if-not
      ) ;_ end of mapcar
  ) ;_ end of setq
  (mapcar '(lambda (x)
       (vl-cmdf "_circle" x "_d" 0.8)
       (vl-cmdf "_text" (polar x (/ pi 4) 1.) 2. 90 (itoa n))
       (setq n (1+ n))
     ) ;_ end of lambda
    lst_point
  ) ;_ end of mapcar
  (setq *NPoint* n)
  (mapcar 'setvar '("osmode" "cmdecho") sys_var)
  )
  (princ)
) ;_ end of defun
(princ "\nНаберите Nvx в командной строке")