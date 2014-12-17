;;; *****************************************  Сервисные ф-ции *******************************
  (vl-load-com) 

;;;Ф-ция проверяет установки UNDO и устанавливает All control
(defun MIP_UNDO_CHECK (/ m)
  (setq m (getvar "UNDOCTL"))
  (cond
    ((= m 0) (command "_.UNDO" "_All"))
    ((= m 3) (command "_.UNDO" "_Control" "_All"))
    (t nil)
  ) ;_ end of cond
  (princ)
) ;_ end of defun

;|===========================================================================
*    Разблокировка текущего слоя активного документа. Состояние
* сохраняется в глобальном списке *kpblc-list-layer-status* вида
* '(vla-указатель ("freeze" . :vlax-true) ("lock" . :vlax-false))
*    Параметры вызова:
*      нет
*    Примеры вызова:
(_mip-clayer-status-save)
=============================================================================|;
(defun _mip-clayer-status-save  ( / item)
   (or *kpblc-activedoc*
       (setq *kpblc-activedoc* (vla-get-activedocument (vlax-get-acad-object)))
   )
  (setq item (vla-item (vla-get-layers *kpblc-activedoc*)(getvar "CLAYER")))
  (setq *kpblc-list-layer-status* (vl-remove (assoc item *kpblc-list-layer-status*) *kpblc-list-layer-status*))
    (setq *kpblc-list-layer-status*
     (append *kpblc-list-layer-status*
       (list
         (list item
         (cons "freeze" (vla-get-freeze item))
         (cons "lock" (vla-get-lock item))
         (cons "on" (vla-get-layeron item))
         ) ;_ end of list
         ) ;_ end of list
       ) ;_ end of append
    ) ;_ end of setq
       (vla-put-layeron item :vlax-true)
       (vla-put-lock item :vlax-false)
  ) ;_ end of defun
;|================================================================
*    Сохраняется текущее значение системных переменных. Список
глобальный (*kpblc-sysvar-list*)
* При условии, что заданы значения, они устанавливаются.
*    Поскольку список *kpblc-sysvar-list* не обнуляется, в нем
хранится история изменения значений переменных.
*    Параметры вызова:
*  *kpblc-sysvar-list*  список системных переменных, состояние
которых надо сохранить.
*      Список состоит из подсписков (Переменная Значение)
*      В списке могут повторяться Переменные. В таком случае будет
*      установлено последнее значение.
*      Если в качестве второго параметра используется nil, то
значение
*      системной переменной просто сохраняется.
*    Примеры вызова:
(kpblc-error-sysvar-list (list '("cmdecho" 0) '("blipmode")
'("osmode" 503)))
(kpblc-error-sysvar-list (kpblc-get-all-sysvar-list))
==============================================================|;
(defun kpblc-error-save-sysvar (sysvar-list)
  (foreach item  sysvar-list
    (setq *kpblc-sysvar-list*
     (cons
       (list (strcase (car item)) (getvar (car item)))
       *kpblc-sysvar-list*
       ) ;_ end of cons
    ) ;_ end of setq
    (if  (cadr item)      ; передано устанавливаемое значение
      (setvar (car item) (cadr item))
      ) ;_ end of if
    ) ;_ end of foreach
  ) ;_ end of defun
;;;================================================================================
;;;Функция отключения режима объектной привязки
(defun disable_osmode ()
  (if (and (< (getvar "OSMODE") 16000)
	   (/= (getvar "OSMODE") 0)
	   )
	  (setvar "OSMODE" (+ (getvar "OSMODE") 16384))
	))
;;;================================================================================
;;;Функция включения режима объектной привязки
(defun enable_osmode ()
  (if (> (getvar "OSMODE") 16000)
    (setvar "OSMODE" (- (getvar "OSMODE") 16384))
  ) ;_if
) ;_ End of defun
;;;================================================================================
;;;Ф-ция изменяет поле БД
;;;ed - список, возвращаемый entget
;;;p - изменяемое поле
;;;a - новое значение
;;; цвет по слою - 256
(defun izmpole (ed p a)
  (if (assoc p ed)
    (progn
      (setq ed (subst (cons p a) (assoc p ed) ed))
    ) ;_progn
    (progn
      (setq ed (append ed (list (cons p a))))
    ) ;_ End of progn
  ) ;_if
  (entmod ed)
) ;_ End of defun
;|==============================================================
*    Восстанавливаются системные переменные. Значения системных
переменных
* должны храниться в глобальном списке *kpblc-sysvar-list*. Если
списка нет
* (nil), происходит просто выход.
*    Параметры вызова:
*  Нет
*    Примеры вызова:
(kpblc-error-restore-sysvar)
==============================================================|;
(defun kpblc-error-restore-sysvar ()
  (if *kpblc-sysvar-list*
    (foreach item *kpblc-sysvar-list*
      (setvar (car item) (cadr item))
      ) ;_ end of foreach
    ) ;_ end of if
  (setq *kpblc-sysvar-list* nil)
  (gc)
  ) ;_ end of defun

  ;|=============================================================================
*    Восстановление состояния слоев из глобального списка
* *kpblc-list-layer-status*
*    Параметры вызова:
*  нет
*    Примеры вызова:
(_kpblc-layer-status-restore)
=============================================================================|;
(defun _kpblc-layer-status-restore (/ item)
     (or *kpblc-activedoc*
       (setq *kpblc-activedoc* (vla-get-activedocument (vlax-get-acad-object)))
   )
  (if *kpblc-list-layer-status*
    (progn
      (foreach item *kpblc-list-layer-status*
  (vla-put-layeron (car item) (cdr (assoc "on" (cdr item))))
  (vla-put-lock (car item) (cdr (assoc "lock" (cdr item))))
  (if (not (equal (strcase (getvar "CLAYER"))(strcase (vla-Get-Name (car item)))))
    (vla-put-freeze (car item) (cdr (assoc "freeze" (cdr item))))
    ) ;_ end of if
  ) ;_ end of foreach
      (setq *kpblc-list-layer-status* nil)
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of defun
;|==============================================================
*    Стандартный обработчик ошибок AutoCAD
==============================================================|;
(defun mip-error (message)
 (or *kpblc-activedoc*
   (setq *kpblc-activedoc* (vla-get-activedocument (vlax-get-acad-object)))
)
;;;Прерываем активную команду  
(while (> (getvar "CMDACTIVE") 0)(command))  
  (if (member message
        '("console break"      "Function cancelled"
    "Функция отменена"    "quit / exit abort"
    "выйти прервать"
    ) ;_list
        ) ;_member
    (princ "\nКоманда прервана пользователем")
    (princ
      (strcat "\ERRNO # "
        (itoa (getvar "ERRNO")) ;_itoa
        ": "
        message
        "\n"
        ) ;_strcat
      ) ;_princ
    ) ;_if
  (mapcar '(lambda ( item)
	      (cond
		((and (= (type item) 'ENAME)
		      (entget item)
		      )
		 (entdel item)
		 )
		((and (= (type item) 'VLA-OBJECT)
		      (vlax-read-enabled-p item)
		      )
		 (vl-catch-all-apply 'vla-Delete (list item))
		 )
		(t nil)
		)
	      )
	  *MIP-DELETE-ITEM*
    )
  (setq *MIP-DELETE-ITEM* nil)
  ;;;Обнуляем все глобальные переменные
  (mapcar '(lambda (x)(set x nil)) *DWGRU_RESET_LIST*)
  (if *MIP-UCS-PREV*   (command "_UCS" "_p"))
  (setq *MIP-UCS-PREV* nil)
  (kpblc-error-restore-sysvar)
  (_kpblc-layer-status-restore)
  (vla-endundomark *kpblc-activedoc*)
  (princ)
  ) ;_ end of defun
;;; *****************************************  END Сервисные ф-ции *******************************

;;; *****************************************  КОМАНДЫ *******************************

(defun c:Zebra (/	width	old_snap	pt1	pt2	pt11
		pt12	pt21	pt22	ug	dist	i	E-list
		sps_l1	sps_l2	c_pt11	c_pt21	c_pt22	U_M	str msh initstr *error*
	       )
   (or *kpblc-activedoc*
       (setq *kpblc-activedoc* (vla-get-activedocument (vlax-get-acad-object)))
   )
    (setq *error* mip-error)
    (mip_undo_check)
    (_MIP-CLAYER-STATUS-SAVE)
    (kpblc-error-save-sysvar
      (list
	'("osmode")
	'("CLAYER")
	'("DIMZIN" 0)
        '("QAFLAGS" 0)
        '("CMDECHO" 0)
        '("EXPERT" 5)
        '("plinewid"  0)
        '("highlight" 1)
        '("limcheck"  0)
        '("pickstyle" 0)
	'("ANGBASE" 0)
	'("ANGDIR" 0)
      )
      )
  (vla-endundomark *kpblc-activedoc*)
  (vla-startundomark *kpblc-activedoc*)
  (setq	width 1.
;;;Длина блока в мм  опред в ф-ле zebra.lin
  ) ;_ end of setq
  (setq	e-list nil
	sps_l1 nil
	sps_l2 nil
	PICK1  nil
	PICK1  (ssadd)
  ) ;_ end of setq
  (setvar "CMDECHO" 0)
  (setq U_M (getvar "UNDOMARKS"))
  (setq pt1 (getpoint "\nОт точки <выход>: "))
  (setq msh (fix (/ 500.0 width)))
  (setq str (strcat "\n(Масштаб M1:" (itoa msh) ") К точке [ширина (W)/ M500 / M1000 / M2000] <выход> : "))
  (setq initstr "U W M500 M1000 M2000")
  (initget initstr)

  (while (and pt1 (setq pt2 (getpoint pt1 str)))
    (cond ((member pt2 '("U" "u" "О")) ;_Отмени
	   (if (< U_M (getvar "UNDOMARKS"))
	     (progn
	       (command "_.UNDO" "_B")
	       (setq pt1 (list (getvar "USERR1") (getvar "USERR2")))
	       (setq e-list (cdr e-list))
	       (setq sps_l1 (reverse (cdr (reverse sps_l1))))
	       (setq sps_l2 (reverse (cdr (reverse sps_l2))))
	     ) ;_ end of progn
	     (progn
	       (alert "Отменять больше нечего")
	     ) ;_ end of progn
	   ) ;_ end of if
	  )
	  ((= pt2 "M500")(setq width 1.0))
	  ((= pt2 "M1000")(setq width 0.5))
	  ((= pt2 "M2000")(setq width 0.25))
	  
	  ((member pt2 '("W" "w")) ;_Ширина
	   (initget 6)
	   (princ "\nНовая ширина линии <")
	   (princ width)
	   (princ ">: ")
	   (setq pt11 (getdist))
	   (if (null pt11)
	     (setq pt11 width)
	   ) ;_ End of if
	   (setq width pt11)
	  )
	  ((listp pt2)
	   (setvar "USERR1" (car pt1))
	   (setvar "USERR2" (cadr pt1))

	   (command "_.UNDO" "_M")
	   (setq ug (angle pt1 pt2))
	   (setq pt11 (polar pt1 (+ ug (* 0.5 PI)) width))
	   (setq pt12 pt1)
	   (setq pt21 (polar pt2 (+ ug (* 0.5 PI)) width))
	   (setq pt22 pt2)

	   (if e-list
	     (progn
	       (setq dist (car e-list))
	       (setq c_pt11 (cdr (assoc 10 (entget dist)))
		     c_pt21 (cdr (assoc 11 (entget dist)))
	       ) ;_ end of setq
	       (setq c_pt22 (inters pt11 pt21 c_pt11 c_pt21 nil))
	       (if c_pt22
		 (setq pt11 c_pt22)
	       ) ;_ end of if
	       (izmpole (entget dist) 11 pt11)
	       (setq sps_l1 (cdr (reverse sps_l1)))
	       (setq sps_l1 (reverse sps_l1))
	       (setq c_pt11 (list (car pt11) (cadr pt11)))
	       (setq sps_l1 (append sps_l1 (list c_pt11)))
	     ) ;_ end of progn
	   ) ;_ end of if
	   (setq old_snap (getvar "OSMODE"))
	   (setvar "OSMODE" 0)
	   (command "_.LINE" pt11 pt21 "")
	   (setq e-list (cons (entlast) e-list))
	   (ssadd (entlast) PICK1)
	   (command "_.LINE" pt12 pt22 "")
	   (ssadd (entlast) PICK1)

	   (if (null sps_l1)
	     (progn
	       (setq sps_l1 (append sps_l1 (list pt11)))
	     ) ;_ end of progn
	   ) ;_ end of if
	   (setq sps_l1 (append sps_l1 (list pt21)))

	   (if (null sps_l2)
	     (progn
	       (setq sps_l2 (append sps_l2 (list pt12)))
	     ) ;_ end of progn
	   ) ;_ end of if
	   (setq sps_l2 (append sps_l2 (list pt22)))


	   (setq pt1 pt2)
	   (setvar "OSMODE" old_snap)
	  )
	  (t nil)
    ) ;_cond
    (setq msh (fix (/ 500.0 width)))
    (if (listp pt2)
      (progn
        (setq str "\nК точке [отмени (U)] <выход(ENTER)> : ")
        (setq initstr "U")
      )
      (progn
	  (setq str (strcat "\n(Масштаб M1:" (itoa msh) ") К точке [ширина (W)/ M500 / M1000 / M2000] <выход> : "))
	  (setq initstr "U W M500 M1000 M2000")
      )
    )   
    (initget initstr)
  ) ;_ end of while
  (if (and sps_l1 sps_l2)
    (progn
      (setq e-list nil
	    i 0
      ) ;_ end of setq
      (command "_.LINE" (car sps_l1) (car sps_l2) "")
      (ssadd (entlast) PICK1)
      (command "_.LINE" (last sps_l1) (last sps_l2) "")
      (ssadd (entlast) PICK1)

      (repeat (length sps_l1)
	(setq pt1 (nth i sps_l1))
	(setq pt2 (nth i sps_l2))
	(setq
	  pt11 (polar pt1 (angle pt1 pt2) (* 0.5 (distance pt1 pt2)))
	) ;_ end of setq
	(setq pt11 (list (car pt11) (cadr pt11)))
	(setq e-list (append e-list (list pt11)))
	(setq i (1+ i))
      ) ;_ end of repeat

      (setq old_snap (getvar "OSMODE"))
      (setvar "OSMODE" 0)
;;; Загpузка типа линии ZEBRA
      (if (null (tblsearch "LTYPE" "ZEBRA"))
	(progn
	  (setq pt1 (findfile "zebra.lin"))
	  (if (null pt1)
	    (progn
	      (alert "Невозможно найти файл\nzebra.lin")
	      (setvar "OSMODE" old_snap)
	      (setq PICK1 nil)
	      (exit)
	    ) ;_ end of progn
	  ) ;_ end of if
	  (command "_.LINETYPE" "_L" "ZEBRA" "ZEBRA" "")
	) ;_ end of progn
      ) ;_ end of if

      (command "_.PLINE" (car e-list) "_W" width "")
      (foreach i (cdr e-list)
	(command i)
      ) ;_ end of foreach
      (command "")
      (command "_.CHANGE"
	       (entlast)
	       ""
	       "_P"
	       "_LT"
	       "zebra"
	       "_S"
	       width
	       ""
      ) ;_ End of command
      (command "_.PEDIT" "_L" "_L" "_OFF" "")
      (command "_.ERASE" PICK1 "")
      (command)
      (command)
      (setvar "OSMODE" old_snap)

    ) ;_ end of progn
  ) ;_ end of if
  (setq PICK1 nil)
  (kpblc-error-restore-sysvar)
  (_kpblc-layer-status-restore)
  (vla-endundomark *kpblc-activedoc*)
  (princ)
)

(Defun C:ZEBRA2 (/     ed    e2	  ang	col   dH    DL	  DLN	e1
		H_put i	    nev	  Param	pt0   pt1   pt11  pt2	pt21
		S_spal	    Vobj  e-lay	e-col e-lt  e-lw osm sps TU EL1 EL2 *error*
	       )
;;;H_put - ширина пути
;;;S_spal - длина сегмента
;;;DL- на сколько шпалы выступают за путь с одной стороны
  (setq	H_put 0.4 ;_ширина пути 3 мм
	S_spal 2 ;_шаг шпал 6 мм
	DL 0.1				
  ) ;_ End of setq
  (mip_undo_check)
   (or *kpblc-activedoc*
       (setq *kpblc-activedoc* (vla-get-activedocument (vlax-get-acad-object)))
   )
    (setq *error* mip-error)
    (_MIP-CLAYER-STATUS-SAVE)
    (kpblc-error-save-sysvar
      (list
	'("osmode")
	'("CLAYER")
	'("DIMZIN")
        '("QAFLAGS" 0)
        '("CMDECHO" 0)
        '("EXPERT" 5)
        '("plinewid"  0)
        '("highlight" 1)
        '("cecolor"   "bylayer")
	'("CELTYPE"  "bylayer")
        '("limcheck"  0)
        '("pickstyle" 0)
	'("ANGBASE" 0)
	'("ANGDIR" 0)
	'("UCSFOLLOW" 0)
        '("LUNITS"  2)
        '("AUNITS"  1)
	(list "LUPREC" (max 2 (getvar "LUPREC")))
	(list "AUPREC" (max 3 (getvar "AUPREC")))
        (list "ucsicon"   (getvar "ucsicon"))
      )
      )
  (vla-endundomark *kpblc-activedoc*)
  (vla-startundomark *kpblc-activedoc*)
  (setq pt1 t)
  (while pt1
    (initget 1 "S")
    (setq TU nil)
    (princ "\nТекущие настройки: Ширина=")(princ H_put)(princ " Длина блока=")(princ S_spal)
    (setq e1 (entsel "\nВыберите ось путей [настройки(S)]: "))
    (cond
      ((= e1 "S")
       (princ "\nШирина блоков в ед.черт <")
       (princ H_put)
       (princ "> : ")
       (initget 6)
       (setq pt2 (getdist))
       (if pt2
	 (setq H_put pt2)
       ) ;_ End of if
       (princ "\nДлина блока в ед.черт <")
       (princ S_spal)
       (princ "> : ")
       (initget 6)
       (setq pt2 (getdist))
       (if pt2
	 (setq S_spal pt2)
       ) ;_ End of if
      )
      ((null e1)
       (alert "Ничего не выбрано")
      )
      (t
       (setq TU (cadr e1))
       (setq e1 (car e1))
       (setq pt2 (cdr (assoc 0 (entget e1))))
       (setq vobj (vlax-ename->vla-object e1))
       (cond
	 ((member
	    pt2
	    '("LINE" "ARC" "SPLINE" "LWPOLYLINE" "POLYLINE" "ELLIPSE")
	  ) ;_ End of member
	  (setq pt21 (vlax-curve-isClosed vobj))
	  (if pt21
	    (alert (strcat "\nОбъект " pt2 " не должен быть замкнут"))
	    (setq pt1 nil)
	  ) ;_ End of if
	 )
	 (t
	  (alert
	    (strcat
	      "Объект "
	      pt2
	      " не может быть осью пути\nОсь пути - полилиния, линия, дуга, сплайн, эллиптическая дуга"
	    ) ;_ End of strcat
	  ) ;_ End of alert
	 )
       ) ;_cond  
      )
    ) ;_ End of cond
  ) ;_while pt1
					;e-col e-lt e-lw
  (setq pt1 (entget e1))
  (setq e-lay (cdr (assoc 8 pt1))) ;_Слой
  (setq e-col (cdr (assoc 62 pt1))) ;_Цвет
  (if (null e-col)
    (setq e-col 256)
  ) ;_ End of if
  (setq e-lt (cdr (assoc 6 pt1))) ;_Тип лин
  (if (null e-lt)
    (setq e-lt "ByLayer")
  ) ;_ End of if
  (setq dH (+ (* H_put 0.5) DL))
  (setq osm (getvar "OSMODE"))
  (disable_osmode)
  (setq vobj (vlax-ename->vla-object e1))
  (setq e-lw (vlax-get-property vobj 'Lineweight))
  (setq pt1 (vlax-curve-getStartPoint vobj))
  (setq pt2 (vlax-curve-getEndPoint vobj))
  (setq DLN (vlax-curve-getDistAtPoint vobj pt2))
  (setq col (fix (/ dln s_spal 1.0)))
  (setq nev (- DLN (* col S_spal)))
  (setq col ( abs (- col 1)))
  (setq nev (+ nev S_spal))
  (setq nev (* nev 0.5))
  (if (< nev 0.1)
    (setq nev 0)
  ) ;_ End of if
  (setq	PICK1 nil
	PICK1 (ssadd)
  ) ;_ end of setq
  (setq	PICK2 nil
	PICK2 (ssadd)
  ) ;_ end of setq
  (setq param (vlax-curve-getParamAtPoint vobj pt1))
  (setq ang (vlax-curve-getFirstDeriv vobj param))
  (setq pt11 (list (+ (car pt1) (car ang)) (+ (cadr pt1) (cadr ang))))
  (setq ang (angle pt1 pt11))
  (setq ang (- ang (* PI 0.5)))
  (setq pt11 (polar pt1 ang dH))
  (setq pt21 (polar pt1 (+ ang PI) dH))
  (command "_OFFSET" (* H_put 0.5) e1 (trans pt11 0 1) "") ;_ End of command
  (setq EL1 (vlax-ename->vla-object (entlast)))
  (ssadd (entlast) PICK2)
  (command "_OFFSET" (* H_put 0.5) e1 (trans pt21 0 1) "") ;_ End of command
  (setq EL2 (vlax-ename->vla-object (entlast)))
  (ssadd (entlast) PICK2)

 (foreach aa (list pt1 pt2)
    (setq pt0 aa)
    (setq param (vlax-curve-getParamAtPoint vobj pt0))
    (setq ang (vlax-curve-getFirstDeriv vobj param))
    (setq pt11 (list (+ (car pt0) (car ang)) (+ (cadr pt0) (cadr ang))))
    (setq ang (angle pt0 pt11))
    (setq ang (- ang (* PI 0.5)))
    (setq pt11 (polar pt0 ang dH))
    (setq pt21 (polar pt0 (+ ang PI) dH))
    (command "_.line" (trans pt11 0 1) (trans pt21 0 1) "")
    (setq e2 (entlast)
	  ed (entget e2)
    ) ;_ End of setq
    (ssadd e2 PICK1)
    (setq pt11 (polar pt0 ang (- dH DL)))
    (setq e2 (vlax-curve-getclosestpointto EL1 pt11))
    (if e2 (setq pt11 e2))
    (setq pt21 (polar pt0 (+ ang PI) (- dH DL)))
    (setq e2 (vlax-curve-getclosestpointto EL2 pt21))
    (if e2 (setq pt21 e2))
    (command "_.line" (trans pt11 0 1) (trans pt21 0 1) "")
    (setq e2 (entlast)
	  ed (entget e2)
    ) ;_ End of setq
    (ssadd e2 PICK2)
    (setq ed (izmpole ed 8 e-lay))
    (setq ed (izmpole ed 62 e-col))
    (setq ed (izmpole ed 6 e-lt))
    (setq ed (vlax-ename->vla-object e2))
    (vlax-put-property ed 'Lineweight e-lw)
 )  
  (setq i '-1)
  (repeat (+ col 1)
    (setq i (1+ i))
    (setq pt0 (vlax-curve-getPointAtDist vobj (+ (* S_spal i) nev)))
    (setq param (vlax-curve-getParamAtPoint vobj pt0))
    (setq pt11 (vlax-curve-getPointAtDist vobj (+ (* S_spal i) (* 0.5 nev))))
    (setq sps (append sps (list pt11)))
    (setq ang (vlax-curve-getFirstDeriv vobj param))
    (setq pt11 (list (+ (car pt0) (car ang)) (+ (cadr pt0) (cadr ang))))
    (setq ang (angle pt0 pt11))
    (setq ang (- ang (* PI 0.5)))
    (setq pt11 (polar pt0 ang dH))
    (setq pt21 (polar pt0 (+ ang PI) dH))
    (command "_.line" (trans pt11 0 1) (trans pt21 0 1) "")
    (setq e2 (entlast)
	  ed (entget e2)
    ) ;_ End of setq
    (ssadd e2 PICK1)
    (setq pt11 (polar pt0 ang (- dH DL)))
    (setq e2 (vlax-curve-getclosestpointto EL1 pt11))
    (if e2 (setq pt11 e2))
    (setq pt21 (polar pt0 (+ ang PI) (- dH dL)))
    (setq e2 (vlax-curve-getclosestpointto EL2 pt21))
    (if e2 (setq pt21 e2))
    
    (command "_.line" (trans pt11 0 1) (trans pt21 0 1) "")
    (setq e2 (entlast)
	  ed (entget e2)
    ) ;_ End of setq
    (ssadd e2 PICK2)
    (setq ed (izmpole ed 8 e-lay))
    (setq ed (izmpole ed 62 e-col))
    (setq ed (izmpole ed 6 e-lt))
    (setq ed (vlax-ename->vla-object e2))
    (vlax-put-property ed 'Lineweight e-lw)
  ) ;_ End of repeat
(setq pt0 (vlax-curve-getPointAtDist vobj (- DLN (* nev 0.5))))
    (setq sps (append sps (list pt0)))
;;; вычисляем min высоту экрана  чтобы сработала bhatch для 0.4 не менее 200 для 1 - 500
  (setq pt11 (getvar "VIEWSIZE"))
  (setq pt21 (* H_put 500.0))
  (if (> pt11 pt21)
    (progn
      (command "_ZOOM" "_C" TU pt21)
      (setq TU "ZOOM")
    )
  )  
(if acet-ui-progress-init (acet-ui-progress-init "Обработка ..." (+ (length sps) 5)))
  
  (command "_-bhatch" "_A" "_B" "_N" PICK1 PICK2 "" "" "_P" "_S")
   (setq i 0)
   (foreach aa sps
     (if (zerop (logand i 1))(command (trans aa 0 1)))
     (setq i (1+ i))
     (if acet-ui-progress-safe (acet-ui-progress-safe i))
   )  
  (command "")
  (setq ang "Гидроизоляция")
  (if (null (tblsearch "LAYER" ang))
    (command "_.-LAYER" "_N" ang "")
  ) ;_ end of if
  (izmpole (entget e1) 8 ang)
  (ssadd (entlast) PICK2)
  (setq i (1+ i))
  (if acet-ui-progress-safe (acet-ui-progress-safe i))
  (command "_CHANGE" PICK2 "" "_P" "_LA" ang "_C" "_ByLayer" "_LW" "_ByLayer" "_LT" "_ByLayer" "")
  (setq i (1+ i))
  (if acet-ui-progress-safe (acet-ui-progress-safe i))
  (entdel e1)
  (command "_.-group"	    "_C"	     "*"
	   "Блоки ФБС"	     PICK2
	   ""
	  ) ;_ end of command

  (if (= TU "ZOOM")(command "_ZOOM" "_P"))  
  (setq i (+ i 2))
  (if acet-ui-progress-safe (acet-ui-progress-safe i))
  (if acet-ui-progress-done (acet-ui-progress-done))
  (command "_ERASE" PICK1 "")
  
  (setq PICK1 nil PICK2 nil)
  (setvar "OSMODE" osm)
(kpblc-error-restore-sysvar)
(_kpblc-layer-status-restore)    
(vla-endundomark *kpblc-activedoc*)

  (princ)
) ;_ End of Defun
(Defun C:ZEBRA3 (/     ed    e2	  ang	col   dH    DL	  DLN	e1 color
		H_put i	    nev	  Param	pt0   pt1   pt11  pt2	pt21
		S_spal	    Vobj  e-lay	e-col e-lt  e-lw osm sps TU EL1 EL2 *error*
	       )
;;;H_put - ширина пути
;;;S_spal - длина сегмента
;;;DL- на сколько шпалы выступают за путь с одной стороны
  (setq	H_put 0.4 ;_ширина пути 3 мм
	S_spal 2 ;_шаг шпал 6 мм
	DL 0.1				
  ) ;_ End of setq
  (mip_undo_check)
   (or *kpblc-activedoc*
       (setq *kpblc-activedoc* (vla-get-activedocument (vlax-get-acad-object)))
   )
    (setq *error* mip-error)
    (_MIP-CLAYER-STATUS-SAVE)
    (kpblc-error-save-sysvar
      (list
	'("osmode")
	'("CLAYER")
	'("DIMZIN")
        '("QAFLAGS" 0)
        '("CMDECHO" 0)
        '("EXPERT" 5)
        '("plinewid"  0)
        '("highlight" 1)
        '("cecolor"   "bylayer")
	'("CELTYPE"  "bylayer")
        '("limcheck"  0)
        '("pickstyle" 0)
	'("ANGBASE" 0)
	'("ANGDIR" 0)
	'("UCSFOLLOW" 0)
        '("LUNITS"  2)
        '("AUNITS"  1)
	(list "LUPREC" (max 2 (getvar "LUPREC")))
	(list "AUPREC" (max 3 (getvar "AUPREC")))
        (list "ucsicon"   (getvar "ucsicon"))
      )
      )
  (vla-endundomark *kpblc-activedoc*)
  (vla-startundomark *kpblc-activedoc*)
  (setq pt1 t)
  (while pt1
    (initget 1 "S")
    (setq TU nil)
    (princ "\nТекущие настройки: Ширина=")(princ H_put)(princ " Длина блока=")(princ S_spal)
    (setq e1 (entsel "\nВыберите ось путей [настройки(S)]: "))
    (cond
      ((= e1 "S")
       (princ "\nШирина блоков в ед.черт <")
       (princ H_put)
       (princ "> : ")
       (initget 6)
       (setq pt2 (getdist))
       (if pt2
	 (setq H_put pt2)
       ) ;_ End of if
       (princ "\nДлина блока в ед.черт <")
       (princ S_spal)
       (princ "> : ")
       (initget 6)
       (setq pt2 (getdist))
       (if pt2
	 (setq S_spal pt2)
       ) ;_ End of if
      )
      ((null e1)
       (alert "Ничего не выбрано")
      )
      (t
       (setq TU (cadr e1))
       (setq e1 (car e1))
       (setq pt2 (cdr (assoc 0 (entget e1))))
       (setq vobj (vlax-ename->vla-object e1))
       (cond
	 ((member
	    pt2
	    '("LINE" "ARC" "SPLINE" "LWPOLYLINE" "POLYLINE" "ELLIPSE")
	  ) ;_ End of member
	  (setq pt21 (vlax-curve-isClosed vobj))
	  (if pt21
	    (alert (strcat "\nОбъект " pt2 " не должен быть замкнут"))
	    (setq pt1 nil)
	  ) ;_ End of if
	 )
	 (t
	  (alert
	    (strcat
	      "Объект "
	      pt2
	      " не может быть осью пути\nОсь пути - полилиния, линия, дуга, сплайн, эллиптическая дуга"
	    ) ;_ End of strcat
	  ) ;_ End of alert
	 )
       ) ;_cond  
      )
    ) ;_ End of cond
  ) ;_while pt1
					;e-col e-lt e-lw
  (setq pt1 (entget e1))
  (setq e-lay (cdr (assoc 8 pt1))) ;_Слой
  (setq e-col (cdr (assoc 62 pt1))) ;_Цвет
  (if (null e-col)
    (setq e-col 256)
  ) ;_ End of if
  (setq e-lt (cdr (assoc 6 pt1))) ;_Тип лин
  (if (null e-lt)
    (setq e-lt "ByLayer")
  ) ;_ End of if
  (setq dH (+ (* H_put 0.5) DL))
  (setq osm (getvar "OSMODE"))
  (disable_osmode)
  (setq vobj (vlax-ename->vla-object e1))
  (setq e-lw (vlax-get-property vobj 'Lineweight))
  (setq pt1 (vlax-curve-getStartPoint vobj))
  (setq pt2 (vlax-curve-getEndPoint vobj))
  (setq DLN (vlax-curve-getDistAtPoint vobj pt2))
  (setq col (fix (/ dln s_spal 1.0)))
  (setq nev (- DLN (* col S_spal)))
  (setq col ( abs (- col 1)))
  (setq nev (+ nev S_spal))
  (setq nev (* nev 0.5))
  (if (< nev 0.1)
    (setq nev 0)
  ) ;_ End of if
  (setq	PICK1 nil
	PICK1 (ssadd)
  ) ;_ end of setq
  (setq	PICK2 nil
	PICK2 (ssadd)
  ) ;_ end of setq
  (setq param (vlax-curve-getParamAtPoint vobj pt1))
  (setq ang (vlax-curve-getFirstDeriv vobj param))
  (setq pt11 (list (+ (car pt1) (car ang)) (+ (cadr pt1) (cadr ang))))
  (setq ang (angle pt1 pt11))
  (setq ang (- ang (* PI 0.5)))
  (setq pt11 (polar pt1 ang dH))
  (setq pt21 (polar pt1 (+ ang PI) dH))
  (command "_OFFSET" (* H_put 0.5) e1 (trans pt11 0 1) "") ;_ End of command
  (setq EL1 (vlax-ename->vla-object (entlast)))
  (ssadd (entlast) PICK2)
  (command "_OFFSET" (* H_put 0.5) e1 (trans pt21 0 1) "") ;_ End of command
  (setq EL2 (vlax-ename->vla-object (entlast)))
  (ssadd (entlast) PICK2)

 (foreach aa (list pt1 pt2)
    (setq pt0 aa)
    (setq param (vlax-curve-getParamAtPoint vobj pt0))
    (setq ang (vlax-curve-getFirstDeriv vobj param))
    (setq pt11 (list (+ (car pt0) (car ang)) (+ (cadr pt0) (cadr ang))))
    (setq ang (angle pt0 pt11))
    (setq ang (- ang (* PI 0.5)))
    (setq pt11 (polar pt0 ang dH))
    (setq pt21 (polar pt0 (+ ang PI) dH))
    (command "_.line" (trans pt11 0 1) (trans pt21 0 1) "")
    (setq e2 (entlast)
	  ed (entget e2)
    ) ;_ End of setq
    (ssadd e2 PICK1)
    (setq pt11 (polar pt0 ang (- dH DL)))
    (setq e2 (vlax-curve-getclosestpointto EL1 pt11))
    (if e2 (setq pt11 e2))
    (setq pt21 (polar pt0 (+ ang PI) (- dH DL)))
    (setq e2 (vlax-curve-getclosestpointto EL2 pt21))
    (if e2 (setq pt21 e2))
    (command "_.line" (trans pt11 0 1) (trans pt21 0 1) "")
    (setq e2 (entlast)
	  ed (entget e2)
    ) ;_ End of setq
    (ssadd e2 PICK2)
    (setq ed (izmpole ed 8 e-lay))
    (setq ed (izmpole ed 62 e-col))
    (setq ed (izmpole ed 6 e-lt))
    (setq ed (vlax-ename->vla-object e2))
    (vlax-put-property ed 'Lineweight e-lw)
 )  
  (setq i '-1)
  (repeat (+ col 1)
    (setq i (1+ i))
    (setq pt0 (vlax-curve-getPointAtDist vobj (+ (* S_spal i) nev)))
    (setq param (vlax-curve-getParamAtPoint vobj pt0))
    (setq pt11 (vlax-curve-getPointAtDist vobj (+ (* S_spal i) (* 0.5 nev))))
    (setq sps (append sps (list pt11)))
    (setq ang (vlax-curve-getFirstDeriv vobj param))
    (setq pt11 (list (+ (car pt0) (car ang)) (+ (cadr pt0) (cadr ang))))
    (setq ang (angle pt0 pt11))
    (setq ang (- ang (* PI 0.5)))
    (setq pt11 (polar pt0 ang dH))
    (setq pt21 (polar pt0 (+ ang PI) dH))
    (command "_.line" (trans pt11 0 1) (trans pt21 0 1) "")
    (setq e2 (entlast)
	  ed (entget e2)
    ) ;_ End of setq
    (ssadd e2 PICK1)
    (setq pt11 (polar pt0 ang (- dH DL)))
    (setq e2 (vlax-curve-getclosestpointto EL1 pt11))
    (if e2 (setq pt11 e2))
    (setq pt21 (polar pt0 (+ ang PI) (- dH dL)))
    (setq e2 (vlax-curve-getclosestpointto EL2 pt21))
    (if e2 (setq pt21 e2))
    
    (command "_.line" (trans pt11 0 1) (trans pt21 0 1) "")
    (setq e2 (entlast)
	  ed (entget e2)
    ) ;_ End of setq
    (ssadd e2 PICK2)
    (setq ed (izmpole ed 8 e-lay))
    (setq ed (izmpole ed 62 e-col))
    (setq ed (izmpole ed 6 e-lt))
    (setq ed (vlax-ename->vla-object e2))
    (vlax-put-property ed 'Lineweight e-lw)
  ) ;_ End of repeat
(setq pt0 (vlax-curve-getPointAtDist vobj (- DLN (* nev 0.5))))
    (setq sps (append sps (list pt0)))
;;; вычисляем min высоту экрана  чтобы сработала bhatch для 0.4 не менее 200 для 1 - 500
  (setq pt11 (getvar "VIEWSIZE"))
  (setq pt21 (* H_put 500.0))
  (if (> pt11 pt21)
    (progn
      (command "_ZOOM" "_C" TU pt21)
      (setq TU "ZOOM")
    )
  )  
(if acet-ui-progress-init (acet-ui-progress-init "Обработка ..." (+ (length sps) 5)))
 (setq PICK3 nil PICK3 (ssadd))
   (setq i 0)
   (foreach aa sps
     (command "_-bhatch" "_A" "_B" "_N" PICK1 PICK2 "" "" "_P" "_S" (trans aa 0 1) "")
     (command)
     (ssadd (entlast) PICK3)
     (setq i (1+ i))
     (if acet-ui-progress-safe (acet-ui-progress-safe i))
   )
     (setq i -1)
  (repeat (sslength PICK3)
    (if (eq (cdr(assoc 0 (setq ed (entget(ssname PICK3 (setq i (1+ i))))))) "HATCH")
        (izmpole ed 62 (if (zerop (logand i 1)) 256 255))
        )
    )
  (setq PICK3 nil ang "Железная дорога")
  (if (null (tblsearch "LAYER" ang))
    (command "_.-LAYER" "_N" ang "")
  ) ;_ end of if
  (izmpole (entget e1) 8 ang)
  (ssadd (entlast) PICK2)
  (setq i (1+ i))
  (if acet-ui-progress-safe (acet-ui-progress-safe i))
  (command "_CHANGE" PICK2 "" "_P" "_LA" ang "_C" "_ByLayer" "_LW" "_ByLayer" "_LT" "_ByLayer" "")
  (setq i (1+ i))
  (if acet-ui-progress-safe (acet-ui-progress-safe i))
  (entdel e1)
  (command "_.-group"	    "_C"	     "*"
	   "ЖД"	     PICK2
	   ""
	  ) ;_ end of command

  (if (= TU "ZOOM")(command "_ZOOM" "_P"))  
  (setq i (+ i 2))
  (if acet-ui-progress-safe (acet-ui-progress-safe i))
  (if acet-ui-progress-done (acet-ui-progress-done))
  (command "_ERASE" PICK1 "")
  
  (setq PICK1 nil PICK2 nil)
  (setvar "OSMODE" osm)
(kpblc-error-restore-sysvar)
(_kpblc-layer-status-restore)    
(vla-endundomark *kpblc-activedoc*)

  (princ)
)
(Defun C:ZEBRA4 (/     ed    e2	  ang	col   dH    DL	  DLN	e1
		H_put i	    nev	  Param	pt0   pt1   pt11  pt2	pt21
		S_spal	    Vobj  e-lay	e-col e-lt  e-lw osm sps TU EL1 EL2 *error*
	       )
;;;H_put - ширина пути
;;;S_spal - длина сегмента
;;;DL- на сколько шпалы выступают за путь с одной стороны
  (setq	H_put 0.4 ;_ширина пути 3 мм
	DL 0.1				
  ) ;_ End of setq
  (mip_undo_check)
   (or *kpblc-activedoc*
       (setq *kpblc-activedoc* (vla-get-activedocument (vlax-get-acad-object)))
   )
    (setq *error* mip-error)
    (_MIP-CLAYER-STATUS-SAVE)
    (kpblc-error-save-sysvar
      (list
	'("osmode")
	'("CLAYER")
	'("DIMZIN")
        '("QAFLAGS" 0)
        '("CMDECHO" 0)
        '("EXPERT" 5)
        '("plinewid"  0)
        '("highlight" 1)
        '("cecolor"   "bylayer")
	'("CELTYPE"  "bylayer")
        '("limcheck"  0)
        '("pickstyle" 0)
	'("ANGBASE" 0)
	'("ANGDIR" 0)
	'("UCSFOLLOW" 0)
        '("LUNITS"  2)
        '("AUNITS"  1)
	(list "LUPREC" (max 2 (getvar "LUPREC")))
	(list "AUPREC" (max 3 (getvar "AUPREC")))
        (list "ucsicon"   (getvar "ucsicon"))
      )
      )
  (vla-endundomark *kpblc-activedoc*)
  (vla-startundomark *kpblc-activedoc*)
  (setq pt1 t)
  (while pt1
    (initget 1 "S")
    (setq TU nil)
    (princ "\nТекущие настройки: Ширина=")(princ H_put)(princ " Длина блока=")(princ S_spal)
    (setq e1 (entsel "\nВыберите ось путей [настройки(S)]: "))
    (cond
      ((= e1 "S")
       (princ "\nШирина блоков в ед.черт <")
       (princ H_put)
       (princ "> : ")
       (initget 6)
       (setq pt2 (getdist))
       (if pt2
	 (setq H_put pt2)
       ) ;_ End of if
      )
      ((null e1)
       (alert "Ничего не выбрано")
      )
      (t
       (setq TU (cadr e1))
       (setq e1 (car e1))
       (setq pt2 (cdr (assoc 0 (entget e1))))
       (setq vobj (vlax-ename->vla-object e1))
       (cond
	 ((member
	    pt2
	    '("LINE" "ARC" "SPLINE" "LWPOLYLINE" "POLYLINE" "ELLIPSE")
	  ) ;_ End of member
	  (setq pt21 (vlax-curve-isClosed vobj))
	  (if pt21
	    (alert (strcat "\nОбъект " pt2 " не должен быть замкнут"))
	    (setq pt1 nil)
	  ) ;_ End of if
	 )
	 (t
	  (alert
	    (strcat
	      "Объект "
	      pt2
	      " не может быть осью пути\nОсь пути - полилиния, линия, дуга, сплайн, эллиптическая дуга"
	    ) ;_ End of strcat
	  ) ;_ End of alert
	 )
       ) ;_cond  
      )
    ) ;_ End of cond
  ) ;_while pt1
					;e-col e-lt e-lw
  (setq pt1 (entget e1))
  (setq e-lay (cdr (assoc 8 pt1))) ;_Слой
  (setq e-col (cdr (assoc 62 pt1))) ;_Цвет
  (if (null e-col)
    (setq e-col 256)
  ) ;_ End of if
  (setq e-lt (cdr (assoc 6 pt1))) ;_Тип лин
  (if (null e-lt)
    (setq e-lt "ByLayer")
  ) ;_ End of if
  (setq dH (+ (* H_put 0.5) DL))
  (setq osm (getvar "OSMODE"))
  (disable_osmode)
  (setq vobj (vlax-ename->vla-object e1))
  (setq e-lw (vlax-get-property vobj 'Lineweight))
  (setq pt1 (vlax-curve-getStartPoint vobj))
  (setq pt2 (vlax-curve-getEndPoint vobj))
  (setq DLN (vlax-curve-getDistAtPoint vobj pt2))
  (setq	PICK1 nil
	PICK1 (ssadd)
  ) ;_ end of setq
  (setq	PICK2 nil
	PICK2 (ssadd)
  ) ;_ end of setq
  (setq param (vlax-curve-getParamAtPoint vobj pt1))
  (setq ang (vlax-curve-getFirstDeriv vobj param))
  (setq pt11 (list (+ (car pt1) (car ang)) (+ (cadr pt1) (cadr ang))))
  (setq ang (angle pt1 pt11))
  (setq ang (- ang (* PI 0.5)))
  (setq pt11 (polar pt1 ang dH))
  (setq pt21 (polar pt1 (+ ang PI) dH))
  (command "_OFFSET" (* H_put 0.5) e1 (trans pt11 0 1) "") ;_ End of command
  (setq EL1 (vlax-ename->vla-object (entlast)))
  (ssadd (entlast) PICK2)
  (command "_OFFSET" (* H_put 0.5) e1 (trans pt21 0 1) "") ;_ End of command
  (setq EL2 (vlax-ename->vla-object (entlast)))
  (ssadd (entlast) PICK2)

 (foreach aa (list pt1 pt2)
    (setq pt0 aa)
    (setq param (vlax-curve-getParamAtPoint vobj pt0))
    (setq ang (vlax-curve-getFirstDeriv vobj param))
    (setq pt11 (list (+ (car pt0) (car ang)) (+ (cadr pt0) (cadr ang))))
    (setq ang (angle pt0 pt11))
    (setq ang (- ang (* PI 0.5)))
    (setq pt11 (polar pt0 ang dH))
    (setq pt21 (polar pt0 (+ ang PI) dH))
    (command "_.line" (trans pt11 0 1) (trans pt21 0 1) "")
    (setq e2 (entlast)
	  ed (entget e2)
    ) ;_ End of setq
    (ssadd e2 PICK1)
    (setq pt11 (polar pt0 ang (- dH DL)))
    (setq e2 (vlax-curve-getclosestpointto EL1 pt11))
    (if e2 (setq pt11 e2))
    (setq pt21 (polar pt0 (+ ang PI) (- dH DL)))
    (setq e2 (vlax-curve-getclosestpointto EL2 pt21))
    (if e2 (setq pt21 e2))
    (command "_.line" (trans pt11 0 1) (trans pt21 0 1) "")
    (setq e2 (entlast)
	  ed (entget e2)
    ) ;_ End of setq
    (ssadd e2 PICK1)
    (setq ed (izmpole ed 8 e-lay))
    (setq ed (izmpole ed 62 e-col))
    (setq ed (izmpole ed 6 e-lt))
    (setq ed (vlax-ename->vla-object e2))
    (vlax-put-property ed 'Lineweight e-lw)
 )  
  
;;; вычисляем min высоту экрана  чтобы сработала bhatch для 0.4 не менее 200 для 1 - 500
  (setq pt11 (getvar "VIEWSIZE"))
  (setq pt21 (* H_put 500.0))
  (if (> pt11 pt21)
    (progn
      (command "_ZOOM" "_C" TU pt21)
      (setq TU "ZOOM")
    )
  )  
  (command "_-bhatch" "_A" "_B" "_N" PICK1 PICK2 "" "" "_P" "_S" "_S" PICK1 PICK2 "" "")
  (setq ang "Железная дорога")
  (if (null (tblsearch "LAYER" ang))
    (command "_.-LAYER" "_N" ang "")
  ) ;_ end of if
  (izmpole (entget e1) 8 ang)
  (ssadd (entlast) PICK2)
  (command "_CHANGE" PICK2 "" "_P" "_LA" ang "_C" "_ByLayer" "_LW" "_ByLayer" "_LT" "_ByLayer" "")
  (entdel e1)
  (command "_.-group"	    "_C"	     "*"
	   "Ж.Д"	     PICK2
	   ""
	  ) ;_ end of command

  (if (= TU "ZOOM")(command "_ZOOM" "_P"))  
  (command "_ERASE" PICK1 "")
  (setq PICK1 nil PICK2 nil)
  (setvar "OSMODE" osm)
(kpblc-error-restore-sysvar)
(_kpblc-layer-status-restore)    
(vla-endundomark *kpblc-activedoc*)
  (princ)
)

(Defun C:RW ( /  ang col dH DLN e1 i nev Param pt0 pt11 pt21 Vobj *error* adoc Flg CL)
  ;;; Рисует рельсы-рельсы шпалы-шпалы 
(defun *error* (msg)(vla-endundomark adoc)(setq PICK1 nil)(if CL (setvar "CLAYER" CL)))
(or adoc
  (setq adoc (vla-get-activedocument (vlax-get-acad-object))))
  (vla-startundomark adoc)
(setq CL (getvar "CLAYER"))  
;;;H_put - ширина пути
;;;S_spal - шаг шпал
;;;DL- на сколько шпалы выступают за путь с одной стороны
  (or *H_put* (setq *H_put* 3)) ;_ширина пути 3 мм
  (or *S_spal* (setq *S_spal* 6)) ;_шаг шпал 6 мм
  (or *DL* (setq *DL* 0.5))  ;выступают на 0.5 мм 
  (setq Flg t)
  (while Flg
    (initget "S")
    (princ "\nШаг-")(princ (rtos *S_spal* 2 2))
    (princ " Ширина-")(princ (rtos *H_put* 2 2))
    (princ " Выступ-")(princ (rtos *DL* 2 2))
    (setq e1 (entsel "\nВыберите ось путей [настройки S] <выход>: "))
    (cond
      ((= e1 "S")
       (princ "\nШирина пути в ед <")(princ *H_put*)(princ "> : ")
       (initget 6)(setq pt0 (getdist))
       (if pt0 (setq *H_put* pt0)) ;_ End of if
       (princ "\nШаг шпал в ед <")(princ *S_spal*)(princ "> : ")
       (initget 6)(setq pt0 (getdist))
       (if pt0 (setq *S_spal* pt0)) ;_ End of if
       (princ "\nВыступ шпал в ед <")(princ *DL*)(princ "> : ")
       (initget 6)(setq pt0 (getdist))
       (if pt0 (setq *DL* pt0)) ;_ End of if
      )
      ((null e1)(if (= (getvar "ERRNO") 52)(setq Flg nil e1 nil vobj nil)(princ " *мимо*")))
      (t
       (setq e1 (car e1) vobj (vlax-ename->vla-object e1))
       (cond
	 ((null(vlax-write-enabled-p vobj))(alert "На блокированном слое!"))
	 ((wcmatch (cdr(assoc 0 (entget e1))) "LINE,ARC,SPLINE,*POLYLINE,ELLIPSE,CIRCLE")(setq Flg nil))
	 (t (alert (strcat "Объект " (cdr(assoc 0 (entget e1)))
	    " не может быть осью пути\nОсь пути - полилиния, линия, дуга, сплайн, эллиптическая дуга, круг"))))))
  ) ;_while 
(if vobj (progn
  (setq dH (+ (* *H_put* 0.5) *DL*)  
       DLN (vlax-curve-getDistAtParam vobj (vlax-curve-getEndParam vobj))
        col (fix (/ dln *s_spal* 1.0))
        nev (- DLN (* col *S_spal*))
        nev (* nev 0.5)
	)
  (setq	PICK1 nil PICK1 (ssadd)) ;_ end of setq
  (setvar "CLAYER" (vla-get-Layer vobj))
  (setq i '-1)
  (repeat (+ col 1)
    (setq i (1+ i)
        pt0 (vlax-curve-getPointAtDist vobj (+ (* *S_spal* i) nev))
          param (vlax-curve-getParamAtPoint vobj pt0)
          ang (vlax-curve-getFirstDeriv vobj param)
          pt11 (mapcar '+ pt0 ang)
          ang (- (angle pt0 pt11)(* PI 0.5))
          pt11 (polar pt0 ang dH)
          pt21 (polar pt0 (+ ang PI) dH)
          )
    (vla-AddLine (vla-ObjectIDToObject adoc (vla-get-OwnerID vobj))
      (vlax-3d-point pt11)(vlax-3d-point pt21))
    (ssadd (entlast) PICK1)
  ) ;_ End of repeat
  (vla-Offset vobj (* *H_put* 0.5))(ssadd (entlast) PICK1)(vla-Offset vobj (- 0 (* *H_put* 0.5)))(ssadd (entlast) PICK1)
  (setq pt0 (mapcar '(lambda (x)(vlax-get-property vobj x)) '(Linetype LineWeight Color Layer)))
  (setq pt11 (mapcar 'vlax-ename->vla-object(vl-remove-if 'listp (mapcar 'cadr (ssnamex PICK1)))))
  (mapcar '(lambda (vla)
	     (mapcar '(lambda (x y)(vlax-put-property vla x y))
		'(Linetype LineWeight Color Layer) pt0)) pt11)
  (setvar "CLAYER" CL)(command "_.-group" "_C" "*" "Трамвайные (ж.д.) пути" PICK1 "")
  (initget "Yes No")(if (= (getkword "\nУдалять ось путей? [Yes/No] <No> : ") "Yes")(vla-erase vobj))
    )
  )
(setq PICK1 nil)
(vla-endundomark adoc)
 (princ)
)
(princ "\nКоманды")
(princ "\nRW - рисование ж.д. методом рельсы-рельсы, шпалы-шпалы")
(princ "\nZebra - рисование ж.д. полилинией")
(princ "\nZebra2 - рисование ж.д. черной штриховкой и пропуском")
(princ "\nZebra3 - рисование ж.д. черной и белой штриховкой")
(princ "\nZebra4 - рисование ж.д. одной черной штриховкой")
