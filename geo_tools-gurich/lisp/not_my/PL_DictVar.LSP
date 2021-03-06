;;;*****************************************************************************************
;;; PL_DictVar.LSP
;;;*****************************************************************************************
;;; Copyright �2003 ϸ�� �������� (Alaspher)
;;; e-mail: alaspher@hotmail.com
;;;*****************************************************************************************
;;; ������: 0.95 ���� (������ ��� ������������)
;;;*****************************************************************************************
;;; ����������� ��������
;;; ��������� ���������������� �� �������� "��� ����".
;;; ����� �� ����� �� ���� � �� ������������� �����-���� ����������� ������������.
;;; �� �����������  ��������� �� ���� ����.
;;; ����� �� ����� �� ���� ��������������� �� ������ ������, �����, ������ ������� ��� �����
;;; ������ ������, ������������ �� ����� ������������� ��� ������������� �������������
;;; ������� ������������ �����������.
;;;*****************************************************************************************
;;; �����������   ������������,  ����������,  ��������,  �  ��������������  ���  �����������
;;; ����������� ���������, ���  �������, ��� ����������� �����������, ��������� ��� ��������
;;; ���������� ������ ��, ����� ����������������  �� ���-�� ��������, � ���������  ���� ����
;;; ���������� ����� � ���������� �� ������������ �������� ����� ����������� �� ���� ������.
;;;*****************************************************************************************
;;; ��������.
;;;
;;; ������ �� ������������ ����� �����  ������������ ������� ��������������� ��� ��������  �
;;; ��������  ������  � �����  �������  ������ �  �������  ������ ��������������  ��  ������
;;; �����������  �����  ��������  ����������  ����������,  �  ���  �����  �  ������  �������
;;; �������������� �������.
;;;
;;; �������� ���������� � ������� ��������  ����������������� ������� � ���������� ������  �
;;; �������������� ���� � ������� ����� �������.
;;;
;;; ��� ���������� ������ � ���������� ������������ ������� 'PL:SetD'.
;;; ������ ������: (PL:SetD <NAME> <VAL>)
;;; ���: <NAME> - ������, ��� ����������;
;;;      <VAL>  - �������� ����������.
;;; ���������� ����������(!) ����������� ��������.
;;; � ������ ���������� �������, ������ ���.
;;; �����������.
;;; �������������� ��������� ������� ������:
;;;   INT        - ����� �����;
;;;   REAL       - ����� � ��������� ������;
;;;   STR        - ������;
;;;   ENAME      - ����� ����������, ��������� ������������ ����� �������� ��������������;
;;;   VLA-OBJECT - VLA-�������, �������� � ���� ENAME, ����������������� ��� ��������;
;;;   PICKSET    - ������ ��������, ��������  � ����  ������� ENAME,  ��� ��������  �������
;;;                ����� ����� ���������� ��-�� ���������, �� ��� ������ <Selection set: #>,
;;;                ��������� ������������ �������� �� ��� � ����� �� ����������;
;;;   LIST       - ������, � ��� ����� �������� ����, ����� ��������� ������  ��������������
;;;                ����, �������� ���� ������ ����: (<INT> . <������ ��������������� ����>),
;;;                �������������� ��������� ������.
;;; �����������  ��������������: SYM  - �������,  ������� �����������  ������ �  �����������
;;; ����. ��������� ������ ������ '() ����������� ��� 'nil'.
;;; ��� ����������: "PL_Dictionary_Copyright" ��������������� �������������.
;;;
;;; ��� ������� � ����������� ������ ������������ ������� 'PL:GetD'.
;;; ������ ������: (PL:GetD <NAME>)
;;; ���: <NAME> - ������, ��� ����������.
;;; ���������� ����������� ��������.
;;;
;;; ��� ����������� ���������� ���������� � ������� ������� 'PL:SetD' ��������� ��  ��������
;;; 'nil' ��� � �������������� ������� 'PL:ClearSetD'.
;;; ������ ������: (PL:ClearSetD <NAME>)
;;; ���: <NAME> - ������, ��� ����������.
;;; ���������� ��� ��������� ������������ ������ ��� 'nil' ��� �������.
;;;
;;; ��� ����������� ������� � ���� ���������� ������������ ������� 'PL:ClearAllSetsD'.
;;; ������ ������: (PL:ClearAllSetsD)
;;; ���������� ��� ��������� ������������� ������� ��� 'nil' ��� �������.
;;;
;;; ���  ���������  ������  ��� ����  ��������� �  ������� ����������  ������������ �������
;;; 'PL:LstAllGlobVar'.
;;; ������ ������: (PL:LstAllGlobVar)
;;; ���������� ������ ��� ���� ��������� � ������� ���������� ��� 'nil' � ������ ����������
;;; ���������� �������.
;;;
;;; ���  ���������  �������  ���������� �  �������  �  ���������� ������������  ������������
;;; ������� 'PL:GlobVarCopyr'.
;;; ������ ������: (PL:GlobVarCopyr)
;;; ���������� ������ ����:
;;; (
;;;  "Global Variables Dictionary"
;;;  0.92
;;;  0
;;;  "Limited FreeWare �2003 Peter V. Loskutov"
;;;  "alaspher@hotmail.com"
;;; )
;;; ��� ������ ���� ������ - ������ ����������, � ������ - ������� ������ '0' = ���� ������,
;;; '1' = �����, ��� 'nil' ���  �������. ��-�� ���������� � ���� ���������������  ���� �����
;;; �������� � ������� �������: 'PL:GlobVarAbout'.
;;; ������ ������: (PL:GlobVarAbout)
;;;
;;; ��������� ������� ��������  � ������ ����������  �������� ���������� �  �� �������������
;;; ��� ������� ������.
;;;*****************************************************************************************

(defun PL:SetD (SYM VAL / _UserDict _TMP)
  (setq	_UserDict (if (not (setq _TMP (PL:GetGlobVarDict)))
		    (PL:NewGlobalVarDict (namedobjdict))
		    (cdr (assoc -1 _TMP))
		  ) ;_ end of if
  ) ;_ end of setq
  (if (dictsearch _UserDict SYM)
    (PL:DelGlobalVar _UserDict SYM)
  ) ;_ end of if
  (if VAL
    (PL:SetGlobalVar SYM (PL:NewGlobalVar _UserDict SYM) VAL)
  ) ;_ end of if
  (PL:GetD SYM)
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:DelGlobalVar (Dict Var / _VarName _TMP)
  (if (setq _TMP (dictsearch Dict Var))
    (progn
      (setq _VarName (cdr (assoc -1 _TMP)))
      (dictremove Dict Var)
      (entdel _VarName)
    ) ;_ end of progn
  ) ;_ end of if
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:NewGlobalVarDict (RootDict / _UserDict)
  (dictadd RootDict
	   "PL_Global_Variables_Dictionary"
	   (setq _UserDict (entmakex '((0 . "DICTIONARY")
				       (100 . "AcDbDictionary")
				       (281 . 1)
				      )
			   ) ;_ end of entmakex
	   ) ;_ end of setq
  ) ;_ end of dictadd
  (dictadd _UserDict
	   "PL_Dictionary_Copyright"
	   (entmakex '((0 . "XRECORD")
		       (100 . "AcDbXrecord")
		       (1 . "Global Variables Dictionary")
		       (2 . "Limited FreeWare �2003 Peter V. Loskutov")
		       (3 . "alaspher@hotmail.com")
		       (4 . "www.uniip.ru")
		       (40 . 0.95)
		       (70 . 0)
		      )
	   ) ;_ end of entmakex
  ) ;_ end of dictadd
  _UserDict
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:NewGlobalVar (UserDict VarName / _UserVar)
  (dictadd UserDict
	   VarName
	   (setq _UserVar (entmakex '((0 . "XRECORD")
				      (100 . "AcDbXrecord")
				     )
			  ) ;_ end of entmakex
	   ) ;_ end of setq
  ) ;_ end of dictadd
  _UserVar
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:SetGlobalVar (VarName Var Xdata / _Xlist _XRecLst)
  (setq _XRecLst (entget Var))
  (setq	_Xlist (cons (cons 1 VarName)
		     (cons
		       '(102 . "{PL:Global_Variable")
		       (append
			 (PL:GenGlobalVarList (list Xdata))
			 '((102 . "PL:Global_Variable}"))
		       ) ;_ end of append
		     ) ;_ end of cons
	       ) ;_ end of cons
  ) ;_ end of setq
  (if (entmod (append _XRecLst _Xlist))
    Xdata
    nil
  ) ;_ end of if
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:GenGlobalVarList (Xdata / _dat _type _next _TMP)
  (setq	_dat  (car Xdata)
	_type (type _dat)
	_next (cdr Xdata)
  ) ;_ end of setq
  (while (= _type 'SYM)
    (setq _dat	(eval _dat)
	  _type	(type _dat)
    ) ;_ end of setq
  ) ;_ end of while
  (if (and (= _type 'list) (setq _TMP (type (cdr _dat))) (/= _TMP 'list))
    (setq _type 'PAIR)
  ) ;_ end of if
  (cond
    ((not _dat)
     (if (and (= (type Xdata) 'list) (> (length Xdata) 0))
       (cons '(102 . "{") (cons '(102 . "}") (PL:GenGlobalVarList _next)))
     ) ;_ end of if
    )
    ((= _type 'INT)
     (cons (cons (if (< -32768 _dat 32768)
		   400
		   90
		 ) ;_ end of if
		 _dat
	   ) ;_ end of cons
	   (PL:GenGlobalVarList _next)
     ) ;_ end of cons
    )
    ((= _type 'REAL)
     (cons (cons 40 _dat) (PL:GenGlobalVarList _next))
    )
    ((= _type 'STR)
     (cons (cons 300 _dat) (PL:GenGlobalVarList _next))
    )
    ((= _type 'list)
     (if
       (eval
	 (cons 'and
	       (cons
		 (= (length _dat) 3)
		 (mapcar
		   (function
		     (lambda (_x)
		       (= (type _x) 'REAL)
		     ) ;_ end of lambda
		   ) ;_ end of function
		   _dat
		 ) ;_ end of mapcar
	       ) ;_ end of cons
	 ) ;_ end of cons
       ) ;_ end of eval
	(cons (cons 10 _dat) (PL:GenGlobalVarList _next))
	(append	(cons '(102 . "{")
		      (append (PL:GenGlobalVarList _dat) '((102 . "}")))
		) ;_ end of cons
		(PL:GenGlobalVarList _next)
	) ;_ end of append
     ) ;_ end of if
    )
    ((= _type 'ENAME)
     (cons (cons -2 _dat) (PL:GenGlobalVarList _next))
    )
    ((= _type 'VLA-OBJECT)
     (cons '(402 . 1)
	   (cons (cons -2 (vlax-vla-object->ename _dat)) (PL:GenGlobalVarList _next))
     ) ;_ end of cons
    )
    ((= _type 'PICKSET)
     (append
       (cons '(402 . 2)
	     (cons '(102 . "{")
		   (append (PL:GenGlobalVarList (PL:Pickset->List _dat 0)) '((102 . "}")))
	     ) ;_ end of cons
       ) ;_ end of cons
       (PL:GenGlobalVarList _next)
     ) ;_ end of cons
    )
    ((= _type 'PAIR)
     (cons (cons 401 (car _dat))
	   (append (PL:GenGlobalVarList (list (cdr _dat))) (PL:GenGlobalVarList _next))
     ) ;_ end of cons
    )
    (t nil)
  ) ;_ end of cond
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:GetD (Var / _UserDict _UserVar)
  (if (setq _UserDict (PL:GetGlobVarDict))
    (if	(setq _UserVar (dictsearch (cdr (assoc -1 _UserDict)) Var))
      (car (PL:ReGenGlobalVarList (PL:ClearGlobalVal _UserVar nil) nil 0))
    ) ;_ end of if
  ) ;_ end of if
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:ReGenGlobalVarList (XList _into _bypass / _TMP)
  (cond
    ((= (length Xlist) 0) ;_ ���� ������ ��������...
     nil ;_ ...���������� nil
    )
    ((< 0 _bypass) ;_ ���� ��������� ����� �������...
     (cond
       ((equal (car Xlist) '(102 . "}"))
	(PL:ReGenGlobalVarList (cdr XList) _into (1- _bypass)) ;_ ��������� ������� � ������
       )
       ((equal (car Xlist) '(102 . "{"))
	(PL:ReGenGlobalVarList (cdr XList) _into (1+ _bypass)) ;_ ����������� ������� � ������
       )
       (t (PL:ReGenGlobalVarList (cdr XList) _into _bypass)) ;_ ����� ���������� ������
     ) ;_ end of cond
    )
    ((equal (car Xlist) '(102 . "{"))
     (cons
       (PL:ReGenGlobalVarList (cdr XList) t _bypass)
       (PL:ReGenGlobalVarList (cdr XList) _into (1+ _bypass))
     ) ;_ end of cons
    )
    ((= 401 (caar Xlist)) ;_ ��������� �������� ���
     (if (= 402 (caadr Xlist)) ;_ ��������� �������������� �����
       (cond
	 ((= 1 (cdadr Xlist)) ;_ ���� ��� 1 �� ����������� ENAME -> VLA-OBJECT
	  (cons	(cons (cdar Xlist)
		      (if (entget (setq _TMP (cdaddr Xlist)))
			(vlax-ename->vla-object _TMP)
		      ) ;_ end of if
		) ;_ end of cons
		(PL:ReGenGlobalVarList (cdddr XList) _into _bypass)
	  ) ;_ end of cons
	 )
	 ((= 2 (cdadr Xlist)) ;_ ���� ��� 2 �� ������������ PICKSET
	  (cons	(cons (cdar Xlist)
		      (PL:List->Pickset (car (PL:ReGenGlobalVarList (cddr XList) t 0)))
		) ;_ end of cons
		(PL:ReGenGlobalVarList (cdddr XList) _into (1+ _bypass))
	  ) ;_ end of cons
	 )
	 (t nil)
       ) ;_ end of cons
       (cons ;_ ��������� ����������� �����
	 (cons (cdar Xlist) (cdadr Xlist))
	 (PL:ReGenGlobalVarList (cddr XList) _into _bypass)
       ) ;_ end of cons
     ) ;_ end of cond
    )
    ((= 402 (caar Xlist)) ;_ ��������� �������������� �����
     (cond
       ((= 1 (cdar Xlist)) ;_ ���� ��� 1 �� ����������� ENAME -> VLA-OBJECT
	(cons (if (entget (setq _TMP (cdadr Xlist)))
		(vlax-ename->vla-object _TMP)
	      ) ;_ end of if
	      (PL:ReGenGlobalVarList (cddr XList) _into _bypass)
	) ;_ end of cons
       )
       ((= 2 (cdar Xlist)) ;_ ���� ��� 2 �� ������������ PICKSET
	(cons (PL:List->Pickset (car (PL:ReGenGlobalVarList (cdr XList) t 0)))
	      (PL:ReGenGlobalVarList (cddr XList) _into (1+ _bypass))
	) ;_ end of cons
       )
       (t nil)
     ) ;_ end of cond
    )
    (_into ;_ ���� ��������� ����� ���������� ������...
     (if (equal (car Xlist) '(102 . "}")) ;_ ���� ������ ����� ������...
       nil ;_ ���������� nil
       (cons (cdar Xlist)
	     (PL:ReGenGlobalVarList (cdr XList) _into _bypass)
       ) ;_ ����� ��������� ��������
     ) ;_ end of if
    )
    (t
     (cons (cdar Xlist)
	   (PL:ReGenGlobalVarList (cdr XList) _into _bypass)
     ) ;_ end of cons
    ) ;_ end of T
  ) ;_ end of cond
) ;_ end of PL:ReGenGlobalVarList

;;;*****************************************************************************************

(defun PL:ClearGlobalVal (VarList _Key / _TMP)
  (setq _TMP (car VarList))
  (cond
    ((equal _TMP '(102 . "{PL:Global_Variable"))
     (PL:ClearGlobalVal (cdr VarList) t)
    )
    ((equal _TMP '(102 . "PL:Global_Variable}"))
     nil
    )
    (_Key (cons _TMP (PL:ClearGlobalVal (cdr VarList) _Key)))
    (t (PL:ClearGlobalVal (cdr VarList) _Key))
  ) ;_ end of cond
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:ClearSetD (SYM / _TMP)
  (if (setq _TMP (PL:GetGlobVarDict))
    (PL:DelGlobalVar (cdr (assoc -1 _TMP)) SYM)
  ) ;_ end of if
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:ClearAllSetsD	(/ _NameDict _Dict)
  (setq	_NameDict "PL_Global_Variables_Dictionary"
	_Dict	  (cdr (assoc -1 (PL:GetGlobVarDict)))
  ) ;_ end of setq
  (if _Dict
    (progn
      (dictremove (namedobjdict) _NameDict)
      (entdel _Dict)
    ) ;_ end of progn
  ) ;_ end of if
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:GetSetD (SYM / _TMP)
  (if (setq _TMP (PL:GetGlobVarDict))
    (dictsearch (cdr (assoc -1 _TMP)) SYM)
  ) ;_ end of if
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:GetDictCopyr ()
  (PL:GetSetD "PL_Dictionary_Copyright")
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:GetGlobVarDict ()
  (dictsearch (namedobjdict) "PL_Global_Variables_Dictionary")
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:GlobVarAbout (/ _TMP)
  (setq _TMP (PL:GetDictCopyr))
  (if _TMP
    (alert (strcat "Product: "
		   (cdr (assoc 1 _TMP))
		   "\n"
		   "Version: "
		   (rtos (cdr (assoc 40 _TMP)) 2 2)
		   (if (= (cdr (assoc 70 _TMP)) 0)
		     " (beta)"
		     ""
		   ) ;_ end of if
		   "\n"
		   "\n"
		   (cdr (assoc 2 _TMP))
		   "\n"
		   "\n"
		   "E-mail: "
		   (cdr (assoc 3 _TMP))
	   ) ;_ end of strcat
    ) ;_ end of alert
  ) ;_ end of if
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:GlobVarCopyr (/ _TMP)
  (setq _TMP (PL:GetDictCopyr))
  (if _TMP
    (list (cdr (assoc 1 _TMP))
	  (cdr (assoc 40 _TMP))
	  (cdr (assoc 70 _TMP))
	  (cdr (assoc 2 _TMP))
	  (cdr (assoc 3 _TMP))
    ) ;_ end of list
  ) ;_ end of if
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:LstAllGlobVar	(/ _TMP)
  (setq _TMP (PL:GetGlobVarDict))
  (if _TMP
    (PL:List-Clr (mapcar 'cdr (PL:DXF-Clr _TMP 3 t)) "PL_Dictionary_Copyright" nil)
  ) ;_ end of if
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:DXF-Clr (_LIST _CODE _T)
  (cond
    ((null _LIST) NIL)
    ((/= _T (equal _CODE (caar _LIST))) (PL:DXF-Clr (cdr _LIST) _CODE _T))
    (t (cons (car _LIST) (PL:DXF-Clr (cdr _LIST) _CODE _T)))
  ) ;_ end of cond
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:List-Clr (_LIST _VAL _T)
  (cond
    ((null _LIST) NIL)
    ((/= _T (equal _VAL (car _LIST))) (PL:List-Clr (cdr _LIST) _VAL _T))
    (t (cons (car _LIST) (PL:List-Clr (cdr _LIST) _VAL _T)))
  ) ;_ end of cond
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:Pickset->List	(SEL I / _TMP)
  (if (setq _TMP (ssname SEL I))
    (cons _TMP (PL:Pickset->List SEL (1+ I)))
  ) ;_ end of if
) ;_ end of defun

;;;*****************************************************************************************

(defun PL:List->Pickset	(LST / _TMP)
  (if (setq _TMP (car LST))
    (if	(entget _TMP)
      (ssadd _TMP (PL:List->Pickset (cdr LST)))
      (PL:List->Pickset (cdr LST))
    ) ;_ end of if
    (ssadd)
  ) ;_ end of if
) ;_ end of defun

;;;*****************************************************************************************

;|�Visual LISP� Format Options�
(130 2 72 2 T "end of " 80 9 1 0 0 T T T T)
;*** DO NOT add text below the comment! ***|;