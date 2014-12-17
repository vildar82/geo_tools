(defun C:test (/ object)
  (setq object (car (entsel "\nВыбери объект: ")))
  (textscr)
  (entget object)
  
  
  )

(defun C:dump (/ object)
  (setq object (car (entsel "\nВыбери объект: ")))
  (textscr)
  
  (vlax-dump-object (vlax-ename->vla-object object) T)
;;;  (vla-get-color (vlax-ename->vla-object object))
  )

(defun C:ldata (/ object)
  (setq object (car (entsel "\nВыбери объект: ")))
  (textscr)
  
  (vlax-ldata-list (vlax-ename->vla-object object))
  )