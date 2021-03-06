;;;�������  ��� �������� (scalelist) �� ������������� � ������-�������
;������� ����� �������, ���� � ����������� ����� ����� �������� ������ ���� foreach 
(defun restore-scale-list ()
  (vl-load-com)
  (foreach sc (dictsearch (namedobjdict) "acad_scalelist")
    (if (and (= (car sc) 350)
             (not (member (cdr (assoc 300 (entget (cdr sc))))
                          '("1:1" "1:2" "1:10" "1:50" "1:100" "2:1") ;_����� ����������� ������ ��������
                  ) ;_ end of member
             ) ;_ end of not
        ) ;_ end of and
      (vl-catch-all-apply
        'vla-delete
        (list (vlax-ename->vla-object (cdr sc)))
      ) ;_ end of vl-catch-all-apply
    ) ;_ end of if
  ) ;_ end of foreach
) ;_ end of defun

;;;(restore-scale-list)


;������� � ����������� ���������
(defun SetScale( / lst pat tmp)
  ;;; pat - ������ �������� ������� �� ������� ����
  ;;;  (("��� � ������ ���������1" �������_�������_�����1 �������_�������_�������1)
  ;;;   ("��� � ������ ���������2"  �������_�������_�����2 �������_�������_�������2)
  ;;;   ...
  ;;;   )
  (setq pat '(("1:1" 1 1)("1:2" 1 2)("1:10" 1 10)
	      ("1:50" 1 50)("1:100" 1 100)("2:1" 2 1)
	      ("1:11" 1 11)("���" 21.5 8.133)))
  ;;;������� �� �������� � ������ �������
  (setq tmp (mapcar 'car pat))
  (foreach item (dictsearch (namedobjdict) "ACAD_SCALELIST")
    (if	(and (= 350 (car item))
	     (not(member(cdr(assoc 300 (entget (cdr item)))) tmp))
	     )
      (vl-catch-all-apply 'entdel (list (cdr item)))
      ) ;_ end of if
    )
  ;;;������ ���������� ���������
  (foreach item (dictsearch (namedobjdict) "ACAD_SCALELIST")
    (if	(= 350 (car item))
      (setq lst (cons (cdr(assoc 300 (entget (cdr item)))) lst))
      ) ;_ end of if
    ) ;_ end of foreach
  ;;;������ �� ��������� ��������� �� ������� pat
  (if (and lst (setq pat (vl-remove-if '(lambda(x)(member (car x) lst)) pat)))
    (progn
      (while (> (getvar "CMDACTIVE") 0)(command))
      (command "_.-scalelistedit")
      (foreach item pat
	(command "_Add" (car item) (strcat (rtos (cadr item)) ":" (rtos (caddr item))))
	) ;_ end of foreach
      (command "_Exit")
      (while (> (getvar "CMDACTIVE") 0)(command))
      )
    )
  (princ)
  )
(defun C:SetScale ()(SetScale))


;������� ���. ��� ��������� �������

;;;======================================================
;;; ������ ��������� SCALELIST SCALE
;;;======================================================

(vl-catch-all-apply
  '(lambda ()
     ((lambda (lst / dict dn)
;;; Purge excess scales
;;; gile
;;; http://www.theswamp.org/index.php?topic=29663.0 
;;;lst - ������ �������� ������� �� ������� ����
;;;  (("��� � ������ ���������1" "������� ������� �����1" "������� ������� �������1")
;;;   ("��� � ������ ���������2" "������� ������� �����2" "������� ������� �������2")
;;;   ...
;;;   )
;;; lst - the pattern scale is made up of lists of species 
;;; (("Name of the Scale 1" Scale_paper_unit_1 Scale_drawing_unit_1) 
;;; ("Name of the Scale 2"  Scale_paper_unit_2 Scale_drawing_unit_2) 
;;; ... 
;;;)
;;; Usage (SetScale)
;;;  (setq pat '(("1:1" 1 1)("1:2" 1 2)("1:10" 1 10) ;_Correct scale here
;;;	      ("1:50" 1 50)("1:100" 1 100)("2:1" 2 1)
;;;	      ))
	(setq dn "A")
        (if (setq dict (dictsearch (namedobjdict) "ACAD_SCALELIST"))
          (progn
            (entmod (vl-remove-if
                      '(lambda (x) (or (= (car x) 3) (= (car x) 350)))
                      dict
                    ) ;_ end of vl-remove-if
            ) ;_ end of entmod
            (setq dict (cdr (assoc -1 dict))
                  n    -1
            ) ;_ end of setq
            (foreach s lst
              (dictadd dict
		       (progn
			 (if (= n 9)
			 (setq dn (chr(1+ (ascii dn)))
			       n -1
			       )
			 )
			 (terpri)
			 (princ
			   (strcat dn (itoa (setq n (1+ n))))
			   )
			 )
                       (entmakex
                         (list
                           '(0 . "SCALE")
                           '(100 . "AcDbScale")
                           (cons 300 (car s))
                           (cons 140 (cadr s))
                           (cons 141 (caddr s))
                         ) ;_ end of list
                       ) ;_ end of entmakex
              ) ;_ end of dictadd
            ) ;_ end of foreach
          ) ;_ end of progn
        ) ;_ end of if
      )
       '(("1:1" 1 1)
	 ("1:2" 1 2)
	 ("1:10" 1 10)
	 ("1:50" 1 50)
	 ("1:100" 1 100)
	 ("2:1" 2 1)
	)
     )
   )
)