;;;���� �� ���� ���� � �� ������ �������� lisp'�, ��� �� ������ ���� ����� (�����, ���� ���� ������, ����� ��� � �� autolisp.ru)
;;;XML ��� ������ � ������ ������ � AutoCAD. ������������ xml ����������� � �� ����, ����� ����, ��� ��� �������� ����������������������� ������ ���� ������ ����������� �������.
;;;������: ����� �� � xml-����� ���������� ��-��� lisp'a? �� ���� ������ ������ ������, �����������, ����������? �����: ��, �����. �������� ��� �� ����� ������.
;;;��� ������ xml-������ ������� ������ ������� � ������ (open, close) �� ��������. ������, �� ���. ��������. �� �� ������ ���������. �������� ���� ���� xml ������ � ��������� �� ANSI-1251, �, ��������, UTF-8. � ���� ��������� (UTF-8), ��������, ��� ��� � ��������� ����� ������ ������������; � ������ 2006..2009 - ���� ���� � �.�.
;;;�� ���� xml ��� ����� ����� ���������. ��������� ��� ���������� "������". � AutoCAD � ������ 2005 � ������ ������� ������ �� Microsoft. ������ ���� �� ��� ����� (� ���������, �� ��������� �������� ������������ ������ ����� � ��������� UFT-8).
;;;������ ��� ��������� ������, � ������� ���� ������������ �������:

(defun _kpblc-get-msg-norm (msg)
  ;|
*    ������������ ������ ������� ��� ������������� � �������� ���.������
|;
  (vl-string-trim "][ :\n<>" msg)
  ) ;_ end of defun

(defun _kpblc-conv-value-to-string (value /)
  ;|
*    ����������� �������� � ������.
|;
  (cond
    ((= (type value) 'str) value)
    ((= (type value) 'int) (itoa value))
    ((= (type value) 'real) (rtos value 2 14))
    ((not value) "")
    (t (vl-princ-to-string value))
    ) ;_ end of cond
  ) ;_ end of defun

(defun _kpblc-error-catch (protected-function
			   on-error-function
			   /
			   catch_error_result
			   )
  ;|
*** ������� ����� �� ������� ������ ruCAD'a ��� ����� �� �� �� ���� ���������,
*** ����� ��������������.
*    �������� ������ ������.
*    ��������� ������:
* protected-function - "����������" �������
* on-error-function - �������, ����������� � ������ ������
|;
  (setq catch_error_result (vl-catch-all-apply protected-function))
  (if (and (vl-catch-all-error-p catch_error_result)
	   on-error-function
	   ) ;_ end of and
    (apply on-error-function
	   (list (vl-catch-all-error-message catch_error_result))
	   ) ;_ end of apply
    catch_error_result
    ) ;_ end of if
  ) ;_ end of defun

(defun _kpblc-error-print (func-name msg)
  ;|
*    ������� ������ ��������� �� ������ ��� (_kpblc-error-catch)
*    ��������� ������:
* func-name ��� �������, � ������� �������� ������
* msg  ��������� �� ������
|;
  (princ (strcat "\n ** "
		 (_kpblc-get-msg-norm
		   (vl-string-subst
		     ""
		     "error"
		     (strcase (_kpblc-conv-value-to-string func-name) t)
		     ) ;_ end of vl-string-subst
		   ) ;_ end of _kpblc-get-msg-norm
		 " ERROR #"
		 (if msg
		   (strcat
		     (_kpblc-conv-value-to-string (getvar "errno"))
		     ": "
		     (_kpblc-conv-value-to-string msg)
		     ) ;_ end of strcat
		   ": undefined"
		   ) ;_ end of if
		 ) ;_ end of strcat
	 ) ;_ end of princ
  (princ)
  ) ;_ end of defun

(defun _kpblc-string-replace-noreg (str old new / pos)
  ;|
*    ������� ������ ��������� ��������� �� �����. ������������������
*    ��������� ������:
* str �������� ������
* old ������ ������
* new ����� ������
*    ��������� ������ ����������� ������: "str" -> "'_str'"
|;
  (while (and (setq pos (vl-string-search (strcase old) (strcase str)))
	      (not (vl-string-search (strcase new) (strcase str)))
	      ) ;_ end of and
    (setq str (strcat (substr str 1 pos)
		      new
		      (_kpblc-string-replace-noreg
			(substr str (+ (strlen old) pos 1))
			old
			new
			) ;_ end of _kpblc-string-replace-noreg
		      ) ;_ end of strcat
	  ) ;_ end of setq
    ) ;_ end of while
  str
  ) ;_ end of defun

;;;����� ��������� ���� xml (� �������� � ��� ������ ��� �� ����������, � �� ��� � ������������������� ��������), ������� ���� ������� ������ XML-���������:

(defun _kpblc-xml-doc-get (file / doc)
  ;|
*    ��������� ��������� �� xml-DOMDocument
*    ��������� ������:
 file xml-����. ���������� �� �����������
|;
  (if (findfile file)
    (_kpblc-error-catch
      (function
	(lambda ()
	  (setq doc (vlax-get-or-create-object "MSXML2.DOMDocument.3.0"))
	  (vlax-put-property doc 'async :vlax-false)
	  (vlax-invoke-method doc 'load file)
	  ) ;_ end of lambda
	) ;_ end of function
      '(lambda (x)
	 (_kpblc-error-print "_kpblc-xml-doc-get" x)
	 (set doc nil)
	 ) ;_ end of lambda
      ) ;_ end of _kpblc-error-catch
    ) ;_ end of if
  doc
  ) ;_ end of defun

;;;����� ��������� ������� ��������� ��� ���� ��������� �����������, ���� �� ��������� ������� ������:
(defun _kpblc-xml-doc-release (doc)
  ;|
*    ������������ �������� XML-���������
*    ��������� ������:
 doc ��������� �� XML-��������.
*    ������� ������:
(setq obj (_kpblc-xml-get-document (findfile (strcat (_kpblc-dir-path-and-splash(_kpblc-dir-get-root-xml))"tables.xml"))))
<...>
(_kpblc-xml-doc-release obj)
|;
  (vl-catch-all-apply
    (function
      (lambda ()
	(vlax-release-object doc)
	(setq doc nil)
	) ;_ end of lambda
      ) ;_ end of function
    ) ;_ end of vl-catch-all-apply
  ) ;_ end of defun

;;;� ���� � ���� ��������� ���������, �� ���� ����� ������������� ��� ���������:
(defun _kpblc-xml-doc-save (node-or-doc / doc)
  ;|
*    ���������� xml-���������
*    ��������� ������:
 node-or-doc ��������� �� ������ XML_DOMDocument ��� ���� �� �����
   ���������
|;
  (if (setq doc (cond
		  ((_kpblc-property-get node-or-doc 'ownerdocument))
		  (t node-or-doc)
		  ) ;_ end of cond
	    ) ;_ end of setq
    (_kpblc-error-catch
      (function
	(lambda ()
	  (vlax-invoke-method
	    doc
	    'save
	    (_kpblc-string-replace-noreg
	      (vl-string-left-trim
		"file:"
		(_kpblc-string-replace-noreg
		  (_kpblc-property-get doc 'url)
		  "%20"
		  " "
		  ) ;_ end of _kpblc-string-replace-noreg
		) ;_ end of vl-string-left-trim
	      "/"
	      "\\"
	      ) ;_ end of _kpblc-string-replace-noreg
	    ) ;_ end of vlax-invoke-method
	  ) ;_ end of lambda
	) ;_ end of function
      '(lambda (x)
	 (_kpblc-error-print "_kpblc-xml-doc-save" x)
	 ) ;_ end of lambda
      ) ;_ end of _kpblc-error-catch
    ) ;_ end of if
  ) ;_ end of defun

;;;��, � "�� ����" - ���������� � ������������ ��������� ������� xml-���������:
(defun _kpblc-xml-doc-save-and-close (node-or-doc / doc)
  ;|
*    ���������� � �������� xml-���������
*    ��������� ������:
 node-or-doc ��������� �� ������ XML_DOMDocument ��� ���� �� �����
   ���������
|;
  (if (setq doc (cond
		  ((_kpblc-property-get node-or-doc 'ownerdocument))
		  (t node-or-doc)
		  ) ;_ end of cond
	    ) ;_ end of setq
    (progn
      (_kpblc-xml-doc-save doc)
      (_kpblc-xml-doc-release doc)
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of defun

;;;�������� xml-��������� (����� ������) � �� ������������ �������� (�������� ���� ����), ������� ������ ����
(defun _kpblc-xml-doc-create (file root / handle)
  ;|
* ���� ���� �� ����������, ������� ��� "� ����".
* ��������� ������:
file ������ ���� ������������ xml-�����. ���������� �����, �� ��������
root ��� Root-���� ������
* ���������� ���� ���������� ����� ���� nil � ������ ������. ���������� �����
* �� �����������.
|;
  (cond
    ((or (not file) (not root)) nil)
    ((findfile file))
    ((not (vl-directory-files (vl-filename-directory file)))
     (vl-mkdir (vl-filename-directory file))
     (_kpblc-xml-doc-create file root)
     )
    (t
     (setq handle (open file "w"))
     (foreach item (list ""
			 (strcat "<" root ">")
			 (strcat "")
			 ) ;_ end of list
       (write-line item handle)
       ) ;_ end of foreach
     (close handle)
     (findfile file)
     )
    ) ;_ end of cond
  ) ;_ end of defun

;;;� ���������� ����� ���� ����������� ������ � �������� xml-���������. ���������?
;;;
;;;� ���� � �� �������...
;;;
;;;����� ��, �������� ������������ �������:

(defun _kpblc-property-get (obj property / res)
  ;|
*    ��������� �������� �������� �������
|;
  (_kpblc-error-catch
    (function
      (lambda ()
	(if (and obj
		 (vlax-property-available-p
		   (setq obj (_kpblc-conv-ent-to-vla obj))
		   property
		   ) ;_ end of vlax-property-available-p
		 ) ;_ end of and
	  (setq res (vlax-get-property obj property))
	  ) ;_ end of if
	) ;_ end of lambda
      ) ;_ end of function
    nil
    ) ;_ end of _kpblc-error-catch
  res
  ) ;_ end of defun

(defun _kpblc-xml-conv-nodes-to-list (nodes / i res)
;|
*    �������������� ��������� �� ��������� Nodes xml-������� � ������.
*    ����������� �������� �� ����� (�����������, DATA-���� � �.�.)
*    ��������� ������:
 nodes ��������� �� ��������� ����� xml-���������
|;
  
  (_kpblc-error-catch
    (function
      (lambda ()
	(setq i 0)
	(while (< i (_kpblc-property-get nodes 'length))
	  (setq res (cons (vlax-get-property nodes 'item i) res)
		i   (1+ i)
		) ;_ end of setq
	  ) ;_ end of while
	(setq res (vl-remove-if-not
		    (function
		      (lambda (x)
			(member (_kpblc-property-get x 'nodetype) '(1 2))
			) ;_ end of lambda
		      ) ;_ end of function
		    (reverse res)
		    ) ;_ end of vl-remove-if-not
	      ) ;_ end of setq
	) ;_ end of lambda
      ) ;_ end of function
    '(lambda (x)
       (_kpblc-error-print "_kpblc-xml-conv-nodes-to-list" x)
       (setq res nil)
       ) ;_ end of lambda
    ) ;_ end of _kpblc-error-catch
  res
  ) ;_ end of defun

;;;� ������ ��������� ���������� (��� "���������") xml-��������� ����������� ���� ��� ���������� "�������", ��� "���������������" ����. � ����� ���� ����� ���� ������ ����. ��� ���� ����� ���������:
(defun _kpblc-xml-node-get-main (obj / res)
;|
*    ��������� �������� (��������) ���� xml-������. ���������� xml-����� ��
* �����������
*    ��������� ������:
 obj ��������� �� ������ XML-���������
*    ������� ������:
(setq obj (_kpblc-xml-doc-get (findfile (strcat (_kpblc-dir-path-and-splash(_kpblc-dir-get-root-xml))"tables.xml"))))
(_kpblc-xml-node-get-main obj)
|;
  (_kpblc-error-catch
    (function
      (lambda ()
	(setq res (car (_kpblc-xml-conv-nodes-to-list
			 (_kpblc-property-get
			   obj
			   'childnodes
			   ) ;_ end of _kpblc-property-get
			 ) ;_ end of _kpblc-xml-conv-nodes-to-list
		       ) ;_ end of car
	      ) ;_ end of setq
	) ;_ end of lambda
      ) ;_ end of function
    '(lambda (x)
       (_kpblc-error-print "_kpblc-xml-node-get-main" x)
       (setq res nil)
       ) ;_ end of lambda
    ) ;_ end of _kpblc-error-catch
  res
  ) ;_ end of defun

;;;��� ��������� ���� �������� ������������ - ���� �������� ����, ���� ������ �����. �� ����� ���������:

(defun _kpblc-xml-nodes-get-child (parent / node childs res)
;|
*    ��������� ����������� ��������� xml-������
*    ��������� ������
 parent ��������� �� ����, ��� �������� �������� Child
  nil ����������
*    ������� ������:
(setq obj (_kpblc-xml-get-document (findfile (strcat (_kpblc-dir-path-and-splash(_kpblc-dir-get-root-xml))"tables.xml"))))
(_kpblc-xml-get-nodes-child (_kpblc-xml-node-get-main obj))
|;
  (if (and parent
	   (vlax-method-applicable-p parent 'haschildnodes)
	   (equal (vlax-invoke-method parent 'haschildnodes)
		  :vlax-true
		  ) ;_ end of equal
	   (setq childs (_kpblc-property-get parent 'childnodes))
	   ) ;_ end of and
    (_kpblc-xml-conv-nodes-to-list childs)
    ) ;_ end of if
  ) ;_ end of defun

;;;��� ����������:

(defun _kpblc-xml-node-add-child (parent tag save / res)
  ;|
*    ���������� ������������ ����
*    ��������� ������:
 parent ��������� �� ������������ ����, � ������� � ����������� ����������
 tag ��� ������ ����
 save ��������� ��� ��� ���������� ��������� ��� parent'a
|;
  (_kpblc-error-catch
    (function
      (lambda ()
	(setq res (vlax-invoke-method
		    parent
		    'appendchild
		    (vlax-invoke-method
		      (_kpblc-xml-doc-get-by-node parent)
		      'createelement
		      tag
		      ) ;_ end of vlax-invoke-method
		    ) ;_ end of vlax-invoke-method
	      ) ;_ end of setq
	(if save
	  (_kpblc-xml-doc-save (_kpblc-xml-doc-get-by-node node))
	  ) ;_ end of if
	) ;_ end of lambda
      ) ;_ end of function
    '(lambda (x)
       (_kpblc-error-print "_kpblc-xml-node-add-child" x)
       (setq res nil)
       ) ;_ end of lambda
    ) ;_ end of _kpblc-error-catch
  res
  ) ;_ end of defun

;;;�� ���� ������, � � xml ���������� ��� ������� �����, ��������� � ������� :) � ���� ���� �����.