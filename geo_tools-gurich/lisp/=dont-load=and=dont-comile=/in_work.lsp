;;;(defun C:proba (/ t1 t2 t4 mtext_obj str temp)
;;;  
;;;  (if (or(eq str1 nil)(eq str1 ""))
;;;    (setq str "\n����� ������� ������: ")
;;;    (setq str (strcat "\n����� ������� ������<" str1 "> : ")))
;;;  (initget)(setq temp (getstring T str))
;;;  (if (/= temp "")
;;;    (setq str1 temp))
;;;  (while (/= str1 "")
;;;    (if(setq t1(getpoint "\n����� ������ �����: "))
;;;      (if(setq t2(getpoint t1 "\n����� ������ �����: "))
;;;	(progn
;;;	  (setq t4(polar t1 (angle t1 t2) (/ (distance t1 t2) 2)))
;;;	  (setq mtext_obj (vla-AddMText model_spece (vlax-3D-point t4) 12 (strcat"\\pxqc;"str1"\\P"(rtos(*(distance t1 t2)1000)2 0))))
;;;	  (vla-put-AttachmentPoint mtext_obj 5)
;;;	  (vla-put-InsertionPoint mtext_obj (vlax-3D-point t4))
;;;	  (vla-put-Height mtext_obj (/ 2.4 (getvar "CANNOSCALEVALUE")))
;;;	  (vla-put-Rotation mtext_obj (analysis_angle_be_long_distance (angle t1 t2)))
;;;	  )
;;;	(setq  str1 "")
;;;	)
;;;      (setq  str1 "")
;;;      )
;;;    )
;;;  (progn)
;;;  )

(defun C:edit_text_by_summ_text (/ text Summ S )
  (initget)(setq text (car (entsel "\n������ ����� � ������: ")))
  (while (not (eq text nil))
    (setq S (atof(edit_text_clean_for_number(cdr (assoc 1 (entget text))))))
    (if
      (eq summ nil)
      (setq summ 0)
      )
    (setq summ (+ summ S))
    (initget)(setq text (car (entsel (strcat "\n����� = " (rtos summ 2 2)  "�. ������ ����� � ����������� <Enter - ����������> : "))))
    )
  (setq t1(car (entsel (strcat "\n����� = " (rtos summ 2) "�. ����� ���� �������� ����� <����������> : "))))
  (if
    (not (eq t1 nil))
    (entmod (list (cons -1 (cdr (assoc -1 (entget t1)))) (cons 1 (rtos summ 2 0)) ))
    )
  (princ)
  )

(defun edit-ldata-delete-all (object / list-ldata)
  (if object
    (progn
      (if (eq (type object) 'ENAME)
	(setq object (vlax-ename->vla-object object)))
      (setq list-ldata(vlax-ldata-list object))
      (foreach x list-ldata
	(vlax-ldata-delete object (car x))
	)
      T
      )
    )
  )

(defun C:proba= ( / OBJ1 OBJ2 TEXT1 TEXT2 flag)
  (while (not flag)
    (if(setq obj1 (car (entsel "\n������ ����� 1: ")))
      (progn
	(setq obj2 (car (entsel "\n������ ����� 2: ")))
	(if (and obj1 obj2)
	  (progn
	    (setq text-list (replese-exts (list (cdr (assoc 1 (entget obj1))) (cdr (assoc 1 (entget obj2))))))
	    (setq text1 (nth 0 text-list)
		  text2 (nth 1 text-list))
	    (entmod (list (cons -1 (cdr (assoc -1 (entget obj1)))) (cons 1 text1)))
	    (entmod (list (cons -1 (cdr (assoc -1 (entget obj2)))) (cons 1 text2)))
	    (entmod (list (cons -1 (cdr (assoc -1 (entget obj1)))) (cons 62 1)))
	    (entmod (list (cons -1 (cdr (assoc -1 (entget obj2)))) (cons 62 1)))
	    )
	  (setq flag T)))
      (setq flag T)
      )
    )
  (princ)
  )


(defun replese-exts (input-list / POS1 POS2 POSTF1 POSTF2 SUFF1 SUFF2 TEMP TEXT1 TEXT2)
  (setq text1 (nth 0 input-list)
	text2 (nth 1 input-list))
  (setq pos1(vl-string-position (ascii "=") text1))
  (setq pos2(vl-string-position (ascii "=") text2))
  (if(and pos1 pos2)
    (progn
      (setq suff1 (substr text1 1 pos1))
      (setq postf1 (substr text1 (+ pos1 2)))
      (setq suff2 (substr text2 1 pos2))
      (setq postf2 (substr text2 (+ pos2 2)))
      (setq text1 (strcat suff1 "=" postf2))
      (setq text2 (strcat suff2 "=" postf1))
      )
    (progn
      (setq temp text1)
      (setq text1 text2)
      (setq text2 temp)
      )    
    )
  (list text1 text2)
  )

(defun C:proba- ( / put-file point list-points)
  (load_global_variable)
  (setq put-file (getfiled "����� �����" (getvar "DWGPREFIX")  "txt" 0))
  (if put-file
    (progn
      (foreach x (file_read_to_list put-file)
	(setq point (convert-string-to-list-be-delimiter x " "))
	(setq list-points(append list-points(list(list(atof (nth 0 point))(atof (nth 1 point))(atof (nth 2 point))))))
	)
;;;      (princ "\nMax=")
;;;      (princ(apply 'max(apply 'append (mapcar 'list (mapcar 'car list-points)))))
;;;      (princ "\nMin=")
;;;      (princ(apply 'min(apply 'append (mapcar 'list (mapcar 'car list-points)))))
      (foreach x list-points
	(draw-circle-vla-by-list-property
	  (list
	    (cons "spece" model_spece)
	    (cons "InsertionPoint" (list  (nth 2 x) (nth 1 x)0))
	    (cons "Radius" (/(nth 0 x)5000.0))
;;;	    (cons "Color" 1)
	    ))
	)
      )
    )
  (princ)
  )

(defun C:proba+ ( / OBJ1 OBJ2 TEXT1 TEXT2 flag)
  (while (not flag)
    (if(setq obj1 (car (entsel "\n������ ������� 1: ")))
      (progn
	(setq obj2 (car (entsel "\n������ ������� 2: ")))
	(if (and obj1 obj2)
	  (progn
	    (setq obj3 (car (entsel "\n������ ����������: ")))
	    (setq elevation1 (atof(cdr (assoc 1 (entget obj1))))
		  elevation2 (atof(cdr (assoc 1 (entget obj2))))
		  dist (atof(cdr (assoc 1 (entget obj3))))
		  )
	    (terpri)
	    (princ elevation1)
	    (terpri)
	    (princ elevation2)
	    (terpri)
	    (setq elevation1 (atof(cdr (assoc 1 (entget obj1))))
		  elevation2 (atof(cdr (assoc 1 (entget obj2)))))
	    (setq pro(rtos (abs(/ (- elevation1 elevation2) dist)) 2 3))
	    (princ pro)
	    (setq obj3 (car (entsel "\n������ �����: ")))
	    (entmod (list (cons -1 (cdr (assoc -1 (entget obj3)))) (cons 1 pro)))
	    )
	  (setq flag T)))
      (setq flag T)
      )
    )
  
  (princ)
  )

(defun C:proba ( / ELEVATION ITEM-LIST N OBJ RESULT-LIST RESULT-STR)
  (setq item-list (list "���" "� ���" "��� ��" "��� ���"))
  (foreach x item-list
    (setq obj (car (entsel (strcat  "\n������ " x ": "))))
    (if obj
      (progn
      (setq elevation(atof(edit_text_clean_for_number(cdr(assoc 1(entget obj))))))
      (setq result-list (append result-list(list (list x elevation))))
      )
      )
    )
  (if result-list
    (progn
      (setq n 0)
      (setq result-str "")
      (foreach x result-list
	(setq result-str (strcat result-str(if (> n 0) "\\P" "") (rtos(nth 1 x)2 3) " " (nth 0 x)))
	(setq n (1+ n))
	)
      (draw-mleader-vla-by-list-property
	(list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	     (cons "LeaderLineCoor" (list (getpoint "\n����� ����� �������: ")(getpoint "\n����� ������ �����: ")))
	     (cons "StyleName" "�������")
	     (cons "TextHeight" 1)
	     (cons "TextString" result-str)
	     ))
      )
    )
  
  (princ)
  )