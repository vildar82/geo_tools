;;;Хотя по идее блог и не сильно посвящен lisp'у, все же сделаю пост здесь (потом, если руки дойдут, добью его и на autolisp.ru)
;;;XML все дальше и дальше входит в AutoCAD. Преимущества xml расписывать я не буду, скажу лишь, что для хранения сложноструктурированных данных этот формат практически идеален.
;;;Вопрос: можно ли к xml-файлу обращаться из-под lisp'a? То есть читать данные оттуда, фильтровать, записывать? Ответ: да, можно. Вдобавок это не очень сложно.
;;;Для чтения xml-файлов обычные режимы доступа к файлам (open, close) не работают. Точнее, не так. Работают. Но не всегда корректно. Особенно если файл xml создан в кодировке не ANSI-1251, а, например, UTF-8. В этой кодировке (UTF-8), например, как раз и создаются файлы палитр инструментов; в версии 2006..2009 - файл меню и т.п.
;;;Но файл xml все равно можно прочитать. Используя так называемый "парсер". В AutoCAD с версии 2005 и дальше встроен парсер от Microsoft. Парсер этот не без греха (в частности, он корректно способен воспринимать только файлы в кодировке UFT-8).
;;;Прежде чем двигаться дальше, я приведу коды библиотечных функций:

(defun _kpblc-get-msg-norm (msg)
  ;|
*    Нормализация строки запроса для использования в запросах ком.строки
|;
  (vl-string-trim "][ :\n<>" msg)
  ) ;_ end of defun

(defun _kpblc-conv-value-to-string (value /)
  ;|
*    конвертация значения в строку.
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
*** Функция взята из книжной версии ruCAD'a без каких бы то ни было переделок,
*** кроме переименования.
*    Оболочка отлова ошибок.
*    Параметры вызова:
* protected-function - "защищаемая" функция
* on-error-function - функция, выполняемая в случае ошибки
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
*    Функция вывода сообщения об ошибке для (_kpblc-error-catch)
*    Параметры вызова:
* func-name имя функции, в которой возникла ошибка
* msg  сообщение об ошибке
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
*    Функция замены вхождений подстроки на новую. Регистронезависима
*    Параметры вызова:
* str исходная строка
* old старая строка
* new новая строка
*    Позволяет менять аналогичные строки: "str" -> "'_str'"
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

;;;Чтобы прочитать файл xml (и работать с ним именно как со структурой, а не как с последовательностью символов), сначала надо создать объект XML-документа:

(defun _kpblc-xml-doc-get (file / doc)
  ;|
*    Получение указателя на xml-DOMDocument
*    Параметры вызова:
 file xml-файл. Валидность не проверяется
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

;;;После получения объекта документа его надо научиться освобождать, дабы не загружать попусту память:
(defun _kpblc-xml-doc-release (doc)
  ;|
*    Освобождение ресурсов XML-документа
*    Параметры вызова:
 doc указатель на XML-документ.
*    Примеры вызова:
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

;;;А если в файл вносились изменения, то надо перед освобождением его сохранить:
(defun _kpblc-xml-doc-save (node-or-doc / doc)
  ;|
*    Сохранение xml-документа
*    Параметры вызова:
 node-or-doc указатель на объект XML_DOMDocument или один из узлов
   документа
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

;;;Ну, и "до кучи" - сохранение с моментальным закрытием объекта xml-документа:
(defun _kpblc-xml-doc-save-and-close (node-or-doc / doc)
  ;|
*    Сохранение и закрытие xml-документа
*    Параметры вызова:
 node-or-doc указатель на объект XML_DOMDocument или один из узлов
   документа
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

;;;Создание xml-документа (скажу честно) я не рассматривал подробно (попросту было лень), поэтому сделал тупо
(defun _kpblc-xml-doc-create (file root / handle)
  ;|
* Если файл не существует, создает его "с нуля".
* Параметры вызова:
file полный путь создаваемого xml-файла. Расширение лобое, не меняется
root имя Root-узла дерева
* Возвращает путь созданного файла либо nil в случае ошибки. Содержимое файла
* не проверяется.
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

;;;В предыдущей части была рассмотрена работа с объектом xml-документа. Продолжим?
;;;
;;;А куда ж мы денемся...
;;;
;;;Опять же, публикую библиотечные функции:

(defun _kpblc-property-get (obj property / res)
  ;|
*    Получение значения свойства объекта
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
*    Преобразование указателя на коллекцию Nodes xml-объекта в список.
*    Исключаются описания не узлов (комментарии, DATA-узлы и т.п.)
*    Параметры вызова:
 nodes указатель на коллекцию узлов xml-документа
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

;;;У любого правильно сделанного (или "валидного") xml-документа обязательно есть так называемый "главный", или "прародительский" узел. И такой узел может быть только один. Его тоже можно прочитать:
(defun _kpblc-xml-node-get-main (obj / res)
;|
*    Получение главного (верхнего) узла xml-дерева. Валидность xml-файла не
* проверяется
*    Параметры вызова:
 obj указатель на объект XML-документа
*    Примеры вызова:
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

;;;Все остальные узлы являются подчиненными - либо главному узлу, либо другим узлам. Их можно считывать:

(defun _kpblc-xml-nodes-get-child (parent / node childs res)
;|
*    Получение подчиненных элементов xml-дерева
*    Параметры вызова
 parent указатель на узел, для которого получаем Child
  nil недопустим
*    Примеры вызова:
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

;;;Или записывать:

(defun _kpblc-xml-node-add-child (parent tag save / res)
  ;|
*    Добавление подчиненного узла
*    Параметры вызова:
 parent указатель на родительский узел, в который и выполняется добавление
 tag тэг нового узла
 save выполнять или нет сохранение документа для parent'a
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

;;;Но узлы узлами, а в xml существует еще понятие тэгов, атрибутов и текстов :) С этим чуть позже.