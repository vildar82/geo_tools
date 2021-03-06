(defun edit-block-set-attributes-by-list (obj att_list / txt lst)
  ; �������� �������� ��������� �� ������, ����� � ���������, ������������
  ;; obj - Ename or Vla object of block
  ;; att_list - list ((Tag_Name1 . Value1)(Tag_Name2 . Value2) ...)
  ;;                 Tag_Name - string
  ;;                    Value - string
  (if (= (type obj) 'ENAME)(setq obj (vlax-ename->vla-object obj)))
  (setq att_list (mapcar '(lambda(x)(cons (strcase (edit-convert-to-string(car x)))(edit-convert-to-string(cdr x)))) att_list))
  (if (and obj
	   (not(vlax-erased-p obj))
	   (= (vla-get-ObjectName obj) "AcDbBlockReference")
	   (eq :vlax-true (vla-get-HasAttributes obj))
	   (vlax-property-available-p obj 'Hasattributes)
	   (vlax-write-enabled-p obj)
	   )
    (vl-catch-all-apply
      (function
	(lambda	()
	  (foreach at (vlax-invoke obj 'Getattributes)
	    (if (setq lst (assoc(strcase(vla-get-TagString at)) att_list))
	      (vla-put-TextString at (cdr lst))
	      )
	    )
	  )
	)
      )
    )
  );defun

(defun edit-convert-to-string (dat)
  (cond ((= (type dat) 'INT)(setq dat (itoa dat)))
	((= (type dat) 'REAL)(setq dat (rtos dat 2 12)))
	((null dat)(setq dat ""))
	(t (setq dat (vl-princ-to-string dat)))))

;;;������ �������������
;;;(defun C:TEST ( )
;;;  (setq tag1 "TAG1") ;_ ��� 1-�� ��������
;;;  (setq tag2 "DESK1") ;_ ��� 2-�� ��������
;;;  (setq str1 (getstring "\n������� �������� ��� �������� TAG1: "))
;;;  (setq str2 (getstring "\n������� �������� ��� �������� DESK1: "))
;;;  (if (setq blk (car(entsel "\n������ ���� :")))
;;;    (edit-block-set-attributes-by-list blk (list (cons tag1 str1)(cons tag2 str2)))
;;;    )
;;;  (princ)
;;;  )
;;;(edit-block-set-attributes-by-list (car(entsel "\n������ ���� :")) (list (cons "NAME" "123")))