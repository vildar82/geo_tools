;;;����� ������ �������� #103-09-2007 16:33:17 
;;;������ ������ ������� ��������� � ������� ������� 
;;;���������� �� ����� �������. ���� ���������� �����
;;;�������� �� �� ����������������� ������� ���������� � ���������� � �������.


;;;������� �������������� ����� �� ����������������� � ���������� ������� ����������
;;;��������� :
;;;� - ����� � ����������������� ������� ���������� , ��� string
;;;����������:
;;;listst - ������ �������� ����� 16. ����� ������ ����� ���������� �������� ���������
(defun H-dec (a / i listst)
  (setq i (strlen a))
  (repeat i
    (setq listst (cons (expt 16 (1- i)) listst))
    (setq i (1- i))
    )
  (rtos
    (apply
      '+
      (mapcar
	'*
	(reverse
	  (mapcar
	    '(lambda (x)
	       (cond
		 (
		  (vl-symbolp (read (chr x)))
		  (+ (vl-position (chr x) '("A" "B" "C" "D" "E" "F"))
		     10
		     )
		  )
		 (t
		  (atoi (chr x))
		  )
		 )
	       )
	    (vl-string->list a)
	    )
	  )
	(mapcar 'float listst)
	)
      )
    )
  )
;;;������� �������������� ����� �� ���������� � ����������������� ������� ����������.
;;;���������:
;;;b- ����� � ���������� ������� ���������� , ��� integer
;;;����������:
;;;blist - ����� � ����������������� ������� ���������� , ��� string
(defun Dec-H (b / blist list16)
  (setq   list16 '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D"
		   "E" "F")
	  )
  (while (>= b 16)
    (setq blist   (cons (fix (rem b 16)) blist)
	  b   (fix (/ b 16.00))
	  )
    )
  (apply 'strcat
	 (mapcar '(lambda (x)
		    (nth x list16)
		    )
		 (setq blist (cons b blist))
		 )
	 )
  )
;;; ������� �������� ���� ������ ������� � ��������� ����. ���������� ����� ����������� � �� ����������� ���������� � ���� �������
;;;��������� ����� � 1 �� ����� ���������� ���������� ��������� , ������� �������� ���������� � ��������� ����
;;; � ���� :  "����� �������"  "�����"  "����� � ���������� �������������" "��� �������" (��� ��� �������)
;;; ���������� ���
;;; ���������� :
;;;filename - ��� ����� ������ ������
;;;dsk - ���������� �����
;;;lp - ��� ���������� ���������� ���������
;;;m - ����� ���������
;;;lm - ������ �����
(defun es_export_database (/ lm filename dsk lp m)
  (setq i 1)
  (if (and
	(setq filename (getfiled "���� ������ " "C:\\" "xls" 1))
	;��� ����� ������
	(setq dsk (open filename "a"))   ;���������� �����
	(setq lp (entmakex '((0 . "point") (10 0 0 0))))
	; ������� �������� ��� ����������� ���������� �������� ����� �������� �����
	)
    (progn
      (repeat (1- (atoi (h-dec (cdr (assoc 5 (entget lp))))))
	;���� �������� �����
	(if
	  (setq m (handent (Dec-H i)))
	  (setq lm (cons m lm))   ; ������ ���������� ����� ������������ � ������� ����������
	  )
	(setq i   (1+ i)
	      )
	)
      (entdel lp)         ; ������� ������� ��������
      (mapcar
	'(lambda (x / nam prn)
	   (setq prn (strcat (vla-get-objectname x)
			     "\t"
			     (vla-get-handle x)
			     "\t"
			     (h-dec (vla-get-handle x))
			     "\t"
			     (rtos (vla-get-objectid x) 2 0)
			     "\t"
			     )
		 )
	   (if
	     (and
	       (vlax-property-available-p x 'Name)
	       (not (vl-catch-all-error-p
		      (setq
			nam (vl-catch-all-apply 'vla-get-name (list x))
			)
		      )
		    )
	       )
	     (setq prn (strcat prn nam "\t"))
	     (setq prn (strcat prn "\t"))
	     )
	   (princ (strcat prn "\n") dsk)
	   )
	(vl-remove 'nil
	  (mapcar 'vlax-ename->vla-object (reverse lm))
	  )
	)               ; ������� ������ ������ ��������� � ����
      (close dsk)
      )
    )
  )