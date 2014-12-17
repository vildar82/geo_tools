;;;(defun C:proba ( / nabor n object kol-vo)
;;;  (setq nabor(ssget "_X"))
;;;  (setq n 0)
;;;  (setq kol-vo (sslength nabor))
;;;  (repeat kol-vo
;;;    (if (=(rem n 100)0)(grtext -1 (strcat"Обработка:"(itoa n)"/"(itoa kol-vo))))
;;;    (setq object (ssname nabor n))
;;;;;;    (vla-put-color (vlax-ename->vla-object object)256)
;;;    (vla-put-layer (vlax-ename->vla-object object)"Сит.план")
;;;    (setq n (1+ n))
;;;    )
;;;  (grtext -1 "")
;;;  (princ)
;;;  )
;;;(defun C:proba ( / )
;;;(draw_blok_ramka
;;;	"ГОСТ А1 594x841"; строка формата вида "ГОСТ А1 594x841"
;;;	"Нет"           ; автоматическая вставка? если "Нет" будет предложено указать точку вставки блока рамкт иначе блок создастся но не вставится
;;;	"Нет"          ; какой штамп необходимо вставить в рамку "ГОСТ" "Большой" "Маленький" "Нет"
;;;	T ; флаг вставки маленького штампа с инвентарным номером: T или nil
;;;	T      ; флаг отключения вспомогательных рамок в листе: T или nil
;;;	)
;;;  (princ)
;;;  )

;;;(defun C:proba ( / ALL-LIST LIST-STR NEW-LIST NEW-LIST-STR)
;;;  (setq all-list (file_read_to_list "d:\\temp\\cutlines.txt"))
;;;  (foreach x all-list
;;;    (if (= (ascii x) 45)
;;;      (progn
;;;	(setq list-str (convert-string-to-list-be-delimiter x ","))
;;;	(setq new-list-str (list (rtos(+ 360.0 (atof(nth 0 list-str)))2 15) (nth 1 list-str) (nth 2 list-str)))
;;;	(setq new-str (convert-list-to-string-be-delimiter new-list-str ","))
;;;	(setq new-list (append new-list (list new-str)))
;;;	)
;;;      (setq new-list (append new-list (list x)))
;;;      )
;;;    )
;;;  (file_write_list_to_file "d:\\temp\\cutlines_out.txt" new-list)
;;;  (princ)
;;;  )

;;;(defun C:test (/ av viewportcenter)
;;;;;;  (setq av (vla-get-ActiveViewport(vla-get-ActiveDocument(vlax-get-acad-object))))
;;;;;;  (vla-put-Target av (vlax-3D-point (list 0.0 0.0 0.0 )))
;;;;;;  (princ (setq viewportcenter
;;;;;;	 (vlax-safearray->list
;;;;;;	   (vlax-variant-value
;;;;;;	     (vla-get-center
;;;;;;	       (vla-get-activepviewport
;;;;;;		 (vla-get-activedocument
;;;;;;		   (vlax-get-acad-object))))))
;;;;;;	))
;;;;;;  (getvar "VIEWCTR")
;;;  (princ)
;;;  (vlax-dump-object(vla-item(vla-get-Viewports(vla-get-ActiveDocument(vlax-get-acad-object)))0)T)
;;;  (vlax-dump-object(vla-get-ActiveDocument(vlax-get-acad-object))T)
;;;  (vlax-dump-object(vla-get-Viewports(vla-get-ActiveDocument(vlax-get-acad-object)))T)
;;;  )





;;;(defun C:proba (/ ass)
;;;;;;  (setq adoc (vla-get-ActiveDocument (vlax-get-acad-object))
;;;;;;	snam "geo_tools-selection"
;;;;;;	sels (vla-get-SelectionSers adoc))
;;;;;;  (if (vl-catch-all-error-p
;;;;;;	(setq sel (vl-catch-all-apply (function vla-item)
;;;;;;		    (list sels snam))))
;;;;;;    (setq sel (vla-add sels snam)))
;;;;;;  
;;;;;;  (setq ass (vla-get-ActiveSelectionSet
;;;;;;	      
;;;;;;	      )
;;;;;;	)
;;;;;;  (princ(vla-get-count ass))
;;;;;;  (vla-clear ass)
;;;;;;  (vlax-dump-object ass T)
;;;  
;;;  (princ)
;;;  )

;;;(defun C:proba (/ DIST LINE-OBJ TEXT-OBJ VLA-LINE VLA-TEXT)
;;;  (if (setq line-obj(car(entsel "\nВыбери линию<Выход>:")))
;;;    (if (eq (cdr(assoc 0 (entget line-obj)))"LINE"); контроль
;;;      (progn
;;;	(setq vla-line (vlax-ename->vla-object line-obj))
;;;	(setq dist (vla-get-Length vla-line))
;;;	(if
;;;	  (setq text-obj(car(entsel (strcat "\nИмеренная длинна " (rtos dist 2 2)" выбери текст вставки<Выход>:"))))
;;;	  (if (eq (cdr(assoc 0 (entget text-obj)))"TEXT")
;;;	    (progn
;;;	      (setq vla-text (vlax-ename->vla-object text-obj))
;;;	      (vla-put-TextString vla-text (rtos dist 2 2))
;;;	      (vla-put-color vla-text 1)
;;;	      )
;;;	    (princ"\nВыбран недопустимый объект! Выбирать надо однострочный текст!")
;;;	    )
;;;	  )
;;;	
;;;	)
;;;      (princ"\nВыбран недопустимый объект! Выбирать надо линию!")
;;;      )
;;;    (princ"\nНичего не выбрано!")
;;;    )
;;;  (princ)
;;;  )
;;;(command "_-PAN"  "0,0,0" (getvar "VIEWCTR"))
;;;(command "_ZOOM" "_C" "0,0,0" (rtos (/ 200(getvar "CANNOSCALEVALUE")) 2 0))



;;;(defun C:proba (/ COL PIK-POINT ROW TABLE_OBJ TEMP-LIST VLA-TABLE)
;;;  (initget 1)(setq temp-list (entsel "\nВыбери существующую таблицу с координатами: "))
;;;  (setq table_obj (nth 0 temp-list))
;;;  (setq pik-point (nth 1 temp-list))
;;;  (setq vla-table (vlax-ename->vla-object table_obj))
;;;  (vla-HitTest vla-table (vlax-3d-point (trans pik-point 1 0)) (vlax-3d-point (trans (getvar "VIEWDIR") 1 0)) 'row 'col)
;;;;;;  (vla-select vla-table (vlax-3D-point pik-point)(vlax-3D-point '(0.0 0.0 1.0)) 0 20 5 :vlax-true 'row 'col)
;;;  (terpri)
;;;  (princ row)
;;;  (terpri)
;;;  (princ col)
;;;  (princ)
;;;  )

(defun C:proba (/ DIST ELEV-1 ELEV-2 UKLON)
  ;вычисляет уклон по указанным превышению(по разнице двух отметок) и расстоянию
  (initget 1)(setq elev-1 (car(entsel "\nВыбери левую отметку: ")))
  (if elev-1
    (progn
      (initget 1)(setq elev-2 (car(entsel "\nВыбери правую отметку: ")))
      (if elev-2
	(progn
	  (initget 1)(setq dist (car(entsel "\nВыбери расстояние: ")))
	  (if dist
	    (progn
	      (setq uklon
		     (/
		       (-(atof(vla-get-TextString(vlax-ename->vla-object elev-1)))
			 (atof(vla-get-TextString(vlax-ename->vla-object elev-2))))
		       (atof(vla-get-TextString(vlax-ename->vla-object dist)))
		       ))
	      (princ (strcat "\nВычисленный уклон" (rtos uklon 2 3)))
	      (initget 1)(setq dist (car(entsel "\nВыбери расстояние: ")))
	      (if dist (vla-put-TextString(vlax-ename->vla-object dist) (rtos uklon 2 3)))
	      )
	    )
	  )
	)
      )
    )
  (princ)
  )

;;;(defun C:test ()
;;;  
;;;;;;  (princ(vlax-get-or-create-object "Excel.Application.11"))
;;;  (princ)
;;;  )

(defun Kr_DeleteAllObjectScale ( / ss pr cmd)
  (vl-load-com)
  (if (setq ss (ssget))
    (progn
      (setq cmd (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (vl-cmdf "-objectscale" ss "" "_Add" (getvar "CANNOSCALE") "")
      (command)
      (foreach pr (dictsearch (namedobjdict) "acad_scalelist")
	(if (and (= (car pr) 350)
		 (not (eq (getvar "CANNOSCALE") (cdr (assoc 300 (entget (cdr pr))))))
		 )
	  (vl-cmdf "-objectscale" ss "" "_Delete" (cdr (assoc 300 (entget (cdr pr)))))
	  );if
	(command)
	(princ)
	);foreach
      (setvar "CMDECHO" cmd)
      );progn
    );if
  (princ)
  );defun



;;;(if
;;;  (not (= (getvar "CANNOSCALEVALUE") 1))
;;;  (progn
;;;    (setq sk (getvar "CANNOSCALEVALUE"))
;;;    (command "CANNOSCALE" "1:1000")
;;;    (setq 1/m (rtos (* (/ 1 sk) 1000) 2 0))
;;;    (setq 1/m (strcat "1:" 1/m ))
;;;    (command "CANNOSCALE" 1/m)
;;;    )
;;;  (progn
;;;    (command "CANNOSCALE" "1:500")
;;;    (command "CANNOSCALE" "1:1000")
;;;    )
;;;  )