;;======================================================================;;
;;    FILLING A CONTOUR WITH EQUALLY SPACED BLOCKS    ;;
;;======================================================================;;

;;====================  HELPER FUNCTIONS :  ========================;;

;;======================================================================;;
;;  DETERMINING IF A POINT LIES ON THE INTERIOR OF A POLYGON  ;;
;;======================================================================;;

;;  Idea was stoled from Eugeny Kalney
;;  http://www.k-prof.com.ru/
;;  written by Fatty The Old Horse
;;  9/29/05 edited: 9/30/05

(defun insidep (pt ent / big flag obj1 obj2 obj3 p1 p2 small)
  (vl-load-com)
  (if (and pt ent)
    (progn
      (setq obj1 (vlax-ename->vla-object (car ent)))
      (setq obj2 (car (vlax-invoke obj1 'Offset 0.001))
      obj3 (car (vlax-invoke obj1 'Offset -0.001)))
      (if (> (vla-get-area obj2)(vla-get-area obj3))
  (progn
    (set 'big obj2)
    (set 'small obj3))
  (progn
    (set 'big obj3)
    (set 'small obj2)))
      (setq p1 (vlax-curve-getClosestPointTo big pt)
      p2 (vlax-curve-getClosestPointTo small pt))
      (if (> (distance pt p1)(distance pt p2))
  (setq flag T)
  (setq flag nil))
      (mapcar (function (lambda (x)
        (progn
          (vla-delete x)
          (vlax-release-object x))))
        (list big small))
      )
    )
  flag
   )
;;======================================================================;;
;;  CALCULATING COLUMN VALUE BY TEST ON CHECKS ORDER FILLING  ;;
;;======================================================================;;

(defun checksp (flag lst arg / i ret tmp)
(if flag
(progn
(setq i 1)
(foreach a lst (if (= (rem i 2) 0)
(setq tmp (mapcar (function (lambda (x)(list (+ (car x)(/ arg 2))(cadr x)(caddr x)))) a))
(setq tmp a))
(setq ret (cons tmp ret))
  (setq i (1+ i)))))
(reverse ret)
)
;;======================================================================;;
;;    CREATING ARRAY OF POINTS IN SPECIFIED WINDOW    ;;
;;======================================================================;;

(defun grid-window-points (p1 p2 ischecks / cnt col col_list grid_list len
        next num_col num_row row wid)
(if (and p1 p2)
  (progn
    (initget 7)
    (setq col (getdist "\nEnter the distance between columns : "))
        (initget 7)
    (setq row (getdist "\nEnter the distance between rows : ")
          len (abs (- (car p1 )(car p2)))
          wid (abs (- (cadr p1 )(cadr p2)))
          num_col (fix (/ (- len (rem len col)) col))
          num_row (fix (/ (- wid (rem wid row)) row))
          col_list (list p1)
          cnt 1)
    (prompt "\n\t\t***\tЗАПОЛНЯЮ КОНТУР, ЖДИТЕ \t***\n")
    (repeat num_col
      (setq next (list (+ (car p1)(* cnt col))(cadr p1)(caddr p1)))
      (setq col_list (cons next col_list))
      (setq cnt (1+ cnt)))
    (setq col_list (reverse col_list))

    (setq cnt 1)
    (setq grid_list (list col_list))
    (repeat num_row
      (setq next (mapcar (function (lambda (x)
      (list (car x)(+ (cadr x)(* cnt row))(caddr x)))) col_list))
      (setq grid_list (cons next grid_list))
      (setq cnt (1+ cnt)))
    (setq grid_list (reverse grid_list))
    (if ischecks (apply 'append (setq grid_list (checksp T grid_list col)))
    (apply 'append grid_list))))
  )
;;======================================================================;;
;;    CREATING ARRAY OF POINTS IN SPECIFIED WINDOW1    ;;
;;======================================================================;;

(defun grid-window-points1 (p1 p2 ischecks col row / cnt col_list grid_list len
        next num_col num_row wid)
(if (and p1 p2)
  (progn
    (setq len (abs (- (car p1 )(car p2)))
          wid (abs (- (cadr p1 )(cadr p2)))
          num_col (fix (/ (- len (rem len col)) col))
          num_row (fix (/ (- wid (rem wid row)) row))
          col_list (list p1)
          cnt 1)
    (prompt "\n\t\t***\tЗАПОЛНЯЮ КОНТУР, ЖДИТЕ \t***\n")
    (repeat num_col
      (setq next (list (+ (car p1)(* cnt col))(cadr p1)(caddr p1)))
      (setq col_list (cons next col_list))
      (setq cnt (1+ cnt)))
    (setq col_list (reverse col_list))

    (setq cnt 1)
    (setq grid_list (list col_list))
    (repeat num_row
      (setq next (mapcar (function (lambda (x)
      (list (car x)(+ (cadr x)(* cnt row))(caddr x)))) col_list))
      (setq grid_list (cons next grid_list))
      (setq cnt (1+ cnt)))
    (setq grid_list (reverse grid_list))
    (if ischecks (apply 'append (setq grid_list (checksp T grid_list col)))
    (apply 'append grid_list))))
  )

;;======================================================================;;
;;    CREATE ARRAY OF POINTS IN SPECIFIED POLYGON    ;;
;;======================================================================;;

(defun grid-polygon-points (/ ent maxpt minpt p1 p2 point_list polyg)
(vl-load-com)
(defun good-ent (clas ask)
(while (and
(setq ent (entsel (strcat "\n" ask ": "))
)
(not (wcmatch
(cdr (assoc 0 (entget (car ent))))
clas
)
)
)
(prompt (strcat "\nObject is not a " clas " !"))
)
)
        (initget 7)
    (setq col (/ (getdist "\nВведи рассояние между колонками в мм плана (для шахматного порядка увелич это число в двое) : ") (getvar "CANNOSCALEVALUE")))
        (initget 7)
    (setq row (/ (getdist "\nВведи рассояние между строками в мм плана : ") (getvar "CANNOSCALEVALUE")))
(good-ent "*POLYLINE" "Выбери ЗАМКНУТУЮ полилинию для штриховки: ")
(if ent
  (progn
    (setq polyg (vlax-ename->vla-obJect (car ent)))
    (if (eq (vlax-get-property polyg 'Closed) :vlax-true)
      (progn
  (vla-getboundingbox polyg 'minpt 'maxpt)
(setq p1 (trans (vlax-safearray->list minpt) 0 1)
      p2 (trans (vlax-safearray->list maxpt) 0 1))

(setq point_list (grid-window-points1 p1 p2 ischecks col row)
      point_list (vl-remove-if-not (function (lambda (x)(insidep x ent))) point_list)))
      (prompt "\nPolygon is opened, there wiil be incorrect filling"))))
(vlax-release-object polyg)
  point_list
  )
;;======================================================================;;
;;    CHOOSING OF CONTOUR TYPE TO FILLING WITH OBJECTS  ;;
;;======================================================================;;

(defun get-map-points (ischecks / ans p1 p2 pts)
  (initget "R P")
  (setq ans (getkword "\nChoose object type [R]ectang or [P]olygon <R> : "))
  (if (not ans)(setq ans "R"))
  (cond ((eq ans "R")
       (progn
   (if (and (or (initget 1)
       (setq p1 (getpoint "\nPick LOWER LEFT corner of window ")))
       (or (initget 1)
       (setq p2 (getcorner p1 "\nPick UPPER RIGHT corner "))))
  
       (setq pts (grid-window-points p1 p2 ischecks)))))
  ((eq ans "P")
   
       (setq pts (grid-polygon-points)))
  (T nil))
pts  
)
;;======================================================================;;
;;        ERROR FUNCTION        ;;
;;======================================================================;;
(defun fil-err (msg)
  (if
    (vl-position
      msg
      '("console break"
  "Function cancelled"
  "quit / exit abort"
       )
    )
     (princ "Error!")
     (princ msg)
  )
  (while (> (getvar "cmdactive") 0) (command))
  (command "._undo" "_end")
  (command "._u")
  (setvar "cmdecho" cmde)
  (setvar "osmode" osm)
  (setq *error* olderror)
  (princ "\nSYSTEM VARIABLES have been reset\n")
  (princ)
)
;;======================================================================;;
;;        MAIN PROGRAM        ;;
;;======================================================================;;
(vl-load-com)
;;======================================================================;;
;;;(prompt "\n\t\t***\tType FIL to execute \t***\n")
;;======================================================================;;
(defun C:fil (/ *error* ans bname cmde fil-err ischecks olderr osm pts put angle-blok ss)

  (command "._undo" "_end")
  (command "._undo" "_be")
  (setq cmde (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq osm (getvar "osmode"))
  (setvar "osmode" 0)
  (setq olderr *error*)
  (setq *error* fil-err)

  (initget "Yes No")
  (setq ans (getkword "\nБудем заполнять контур в шахматном порядке? (Yes/No) <N> : "))
  (if (not ans)(setq ans "No"))
  (cond ((eq ans "Yes")(setq ischecks 'T))
  ((eq ans "No")(setq ischecks nil)))
  (setq bname (getstring T "\nВведи имя блока для штриховки : "))
  (if
	(not (tblobjname "block" bname))
	(load_annotative_bloks)
	)
  (setq pts (get-map-points ischecks))
  
  (if (tblsearch "block" bname)
  (progn
  (command "._undo" "_m")
  (setq ss (ssadd))
  (mapcar (function (lambda (x)
		      (insert_my_blok bname x)
		      (ssadd (entlast) ss)
		      ))
	  pts)
  (vl-cmdf "-objectscale" ss "" "_Add" (getvar "CANNOSCALE") "")
  (command)
;;;  (if
;;;	  (not (= (getvar "CANNOSCALEVALUE") 1))
;;;	  (progn
;;;	    (setq sk (getvar "CANNOSCALEVALUE"))
;;;	    (command "CANNOSCALE" "1:1000")
;;;	    (setq 1/m (rtos (* (/ 1 sk) 1000) 2 0))
;;;	    (setq 1/m (strcat "1:" 1/m ))
;;;	    (command "CANNOSCALE" 1/m)
;;;	    )
;;;	  (progn
;;;	    (command "CANNOSCALE" "1:500")
;;;	    (command "CANNOSCALE" "1:1000")
;;;	    )
;;;	  )
  )
   (prompt (strcat "\nBlock " bname " not found\n")))
  (setq  *error*  olderr
  fil-err nil
  )
  (setvar "CMDECHO" cmde)
  (setvar "osmode" osm)
  (command "._undo" "_end")
  (princ)
)
;TesT:
;;;(C:fil)
;;;(princ)
;;============================  EOF  ================================;;
(defun insert_my_blok (block_name t1 / dxf41 dxf42 dxf43 dxf2 dxf50)
  (if (not angle-blok) (setq angle-blok 0.0))
  (if (= block_name "Штриховка Кустарнички")(setq angle-blok (+ angle-blok (/ pi 3.0))))
	(setq dxf41 (vl-list* 41 1))
        (setq dxf42 (vl-list* 42 1))
        (setq dxf43 (vl-list* 43 1))
        (setq dxf2 (vl-list* 2 block_name))
        (setq dxf50 (vl-list* 50 angle-blok))
	(entmake (list '(0 . "INSERT") dxf2 (append '(10) t1) dxf41 dxf42 dxf43 dxf50))
  )