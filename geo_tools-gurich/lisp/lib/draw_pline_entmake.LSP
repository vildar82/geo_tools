(defun draw_pline_entmake (points is_closed is_3d width lineweight / elst ENTL)
;;; создает полилинию
;;; (draw_pline_entmake (список_вершин) флаг_замкнутости
;;; флаг_делать_3М_ПЛИНИЮ)
;;; возвращает имя примитива — полилинии или NIL, если что-то не
;;; вышло.
;|
Пример:

(draw_pline_entmake
 (list
 (list 220.65 345.001)
 (list 332.622 546.525)
 (list 846.376 320.906)
(list 719.035 121.572)
) T nil 0 50)
|;
  (if is_3d
    (progn
      (setq entl
       (list
         '(0 . "POLYLINE")
         '(100 . "AcDbEntity")
         '(100 . "AcDb3dPolyline")
         '(66 . 1)
         '(10 0.0 0.0 0.0)
         (cons 70
         (logior 8
           (if is_closed
             1
             0
           ) ;_ end of if
           (if (= 1 (getvar "PLINEGEN"))
             128
             0
           ) ;_ end of if
         ) ;_ end of logior
         ) ;_ end of cons
       ) ;_ end of list
      ) ;_ end of setq
      (if (entmake entl)
  (progn
    (foreach v points
      (progn
        (setq entl
         (list
           '(0 . "VERTEX")
           '(100 . "AcDbEntity")
           '(100 . "AcDbVertex")
           '(100 . "AcDb3dPolylineVertex")
           (append '(10) v)
           '(70 . 32)
         ) ;_ end of list
        ) ;_ end of setq
        (entmake entl)
      ) ;_ end of progn
    ) ;_ end of foreach
    (if (entmake '((0 . "SEQEND") (100 . "AcDbEntity")))
      (entupd (entlast))
      nil
    ) ;_ end of if
  ) ;_ end of progn
      ) ;_ end of if
    ) ;_ end of progn
    (progn
      (setq elst (append
       (list
         '(0 . "LWPOLYLINE")
         '(100 . "AcDbEntity")
         '(100 . "AcDbPolyline")
        ;(cons 8 (getvar "CLAYER"))
         (cons 90 (length points))
         (cons 43 width)
                     (cons 370 lineweight)
         (cons 70
         (logior (if is_closed
             1
             0
           ) ;_ end of if
           (if (= 1 (getvar "PLINEGEN"))
             128
             0
           ) ;_ end of if
         ) ;_ end of logand
         ) ; _ end of cons;;;
       ) ;_ end of list
       (mapcar '(lambda (p)
            (list 10 (car p) (cadr p))
          ) ;_ end of lambda
         points
       ) ;_ end of mapcar
     ) ;_ end of append
      ) ;_ end of setq
      (if (entmake elst)
  (entupd (entlast))
  nil
      ) ;_ end of if
    ) ;_ end of progn
  ) ;_ end of if
)
