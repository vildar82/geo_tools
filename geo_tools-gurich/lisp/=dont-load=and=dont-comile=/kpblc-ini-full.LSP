(defun _kpblc-ini-datas-read (file / handle str res group tmp)
                             ;|
*    Чтение данных из ini-файла в список
*    Параметры вызова:
*	file	имя файла. Если файла нет, возвращает nil
*    Для файла вида
[group1]
key1=value1
key2=value2
; comment
[group2]
key1=value1
key2=value2
*    Возвращает:
'(("group1" ("key1" . "value1") ("key2" . "value2")) ("group2" ("key1" . "value1") ("key2" . "value2")))
*    Файл не создается!!! Имя группы и ключ всегда в нижнем регистре!!!
|;
  (_kpblc-error-catch
    (function
      (lambda ()
        (setq file
               (strcat
                 (_kpblc-dir-path-and-splash (vl-filename-directory file))
                 (strcat (vl-filename-base file) ".ini")
                 ) ;_ end of strcat
              ) ;_ end of setq
        (if (findfile file)
          (progn
            (setq handle (open file "r"))
            (while (setq str (read-line handle))
              (cond
                ((or (< (strlen str) 2) (= (substr str 1 1) ";"))
                 )
                ((= (substr str 1 1) "[")
                 (setq group (strcase (vl-string-trim "[]" str) t)
                       res   (append res (list (list group)))
                       ) ;_ end of setq
                 )
                (group
                 (setq res
                        (subst
                          (cons
                            group
                            (vl-remove
                              'nil
                              (append
                                (cdr (assoc group res))
                                (list
                                  (cons
                                    (strcase (substr str
                                                     1
                                                     (vl-string-search "=" str)
                                                     ) ;_ end of substr
                                             t
                                             ) ;_ end of strcase
                                    (substr str
                                            (+ 2 (vl-string-search "=" str))
                                            ) ;_ end of substr
                                    ) ;_ end of cons
                                  ) ;_ end of list
                                ) ;_ end of append
                              ) ;_ end of vl-remove
                            ) ;_ end of cons
                          (assoc group res)
                          res
                          ) ;_ end of subst
                       ) ;_ end of setq
                 )
                ) ;_ end of cond
              ) ;_ end of while
            (close handle)
            ) ;_ end of progn
          ) ;_ end of if
        ) ;_ end of lambda
      ) ;_ end of function
    '(lambda (x)
       (vl-catch-all-apply '(lambda () (close handle)))
       (_kpblc-error-print "_kpblc-ini-datas-read" x)
       ) ;_ end of lambda
    ) ;_ end of _kpblc-error-catch
  (vl-remove-if-not 'cdr res)
          ;(vl-remove-if '(lambda (x) (not (cdr x))) res)
  ) ;_ end of defun

(defun _kpblc-ini-datas-write (file datas / handle exist)
                              ;|
*    Запись списка в ini-файл
*    Параметры вызова:
*	file	имя файла. Если файла не существует, то он создается
*	datas	список вида ((group (key . value) (key . value)) <...>)
*    Примеры вызова:
(_kpblc-ini-datas-write "c:\\temp.ini" '(("gr1" ("Key1" . "Val1") ("Key2" . "Val2"))
					 ("gr2" ("key1" . "Val1") ("Key2" . "Val2"))))
|;
  (_kpblc-error-catch
    (function
      (lambda ()
        (or (findfile (setq file
                             (strcat
                               (_kpblc-dir-path-and-splash (vl-filename-directory file))
                               (strcat (vl-filename-base file) ".ini")
                               ) ;_ end of strcat
                            ) ;_ end of setq
                      ) ;_ end of findfile
            ((lambda ()
               (vl-mkdir (vl-filename-directory file))
               (setq handle (open file "w"))
               (close handle)
               ) ;_ end of lambda
             )
            ) ;_ end of or
        (if (not (findfile file))
          (progn
            (if (/= "" (vl-filename-directory file))
              (vl-catch-all-apply
                '(lambda ()
                   (vl-mkdir
                     (_kpblc-dir-path-no-splash
                       (vl-filename-directory file)
                       ) ;_ end of _kpblc-dir-path-no-splash
                     ) ;_ end of vl-mkdir
                   ) ;_ end of lambda
                ) ;_ end of vl-catch-all-apply
              ) ;_ end of if
            (setq handle (open file "w")
                  ) ;_ end of setq
            (close handle)
            ) ;_ end of progn
          ) ;_ end of if
        ;; Объединяем списки datas и имеющиеся данные:
        (setq datas  (mapcar
                       '(lambda (x)
                          (cons
                            (strcase (car x) t)
                            (mapcar '(lambda (y)
                                       (cons (strcase (car y) t) (cdr y))
                                       ) ;_ end of lambda
                                    (cdr x)
                                    ) ;_ end of mapcar
                            ) ;_ end of cons
                          ) ;_ end of LAMBDA
                       datas
                       ) ;_ end of mapcar
              exist  (_kpblc-ini-datas-read file)
              datas  ((lambda (/)
                        (foreach item datas
                          (if (setq gr (cdr (assoc (car item) exist)))
                            (progn
                              (foreach key (cdr item)
                                (if (cdr (assoc (car key) gr))
                                  (setq gr
                                         (subst key
                                                (assoc (car key) gr)
                                                gr
                                                ) ;_ end of subst
                                        ) ;_ end of setq
                                  (setq gr (append gr (list key)))
                                  ) ;_ end of if
                                ) ;_ end of foreach
                              (setq exist (subst (cons (car item) gr)
                                                 (assoc (car item) exist)
                                                 exist
                                                 ) ;_ end of subst
                                    ) ;_ end of setq
                              ) ;_ end of progn
                            (setq exist (append exist (list item)))
                            ) ;_ end of if
                          ) ;_ end of foreach
                        exist
                        ) ;_ end of lambda
                      )
              handle (open file "w")
              ) ;_ end of setq
        (foreach item datas
          (write-line
            (strcat "[" (_kpblc-conv-value-to-string (car item)) "]")
            handle
            ) ;_ end of write-line
          (foreach val (cdr item)
            (write-line
              (strcat
                (vl-string-trim
                  " "
                  (_kpblc-conv-value-to-string (car val))
                  ) ;_ end of vl-string-trim
                "="
                (vl-string-trim
                  " "
                  (_kpblc-conv-value-to-string (cdr val))
                  ) ;_ end of vl-string-trim
                ) ;_ end of strcat
              handle
              ) ;_ end of write-line
            ) ;_ end of foreach
          ) ;_ end of foreach
          ;	(close handle)
        ) ;_ end of lambda
      ) ;_ end of function
    (function
      (lambda (x)
          ;	(vl-catch-all-apply '(lambda () (close handle)))
        (_kpblc-error-print "_kpblc-ini-datas-write" x)
        ) ;_ end of lambda
      ) ;_ end of function
    ) ;_ end of _kpblc-error-catch
  (vl-catch-all-apply '(lambda () (close handle)))
  ) ;_ end of defun

(defun _kpblc-ini-read-value (file section key / lst)
                             ;|
*    Выполняет чтение указанного ключа секции ini-файла
*    Параметры вызова:
*	file	имя файла
*	section	секция
*	key	имя ключа
*    Примеры вызова:
(_kpblc-ini-read-value (strcat (_kpblc-dir-get-root-ini) "user.ini") "user" "name")
*    В случае отсутствия секции, ключа либо файла возвращает пустую строку ("").
* Файл не создается.
|;
  (_kpblc-conv-value-to-string
    (cdr
      (_kpblc-list-assoc
        key
        (cdr
          (_kpblc-list-assoc section (_kpblc-ini-datas-read file))
          ) ;_ end of cdr
        ) ;_ end of _kpblc-list-assoc
      ) ;_ end of cdr
    ) ;_ end of _kpblc-conv-value-to-string
  ) ;_ end of defun

(defun _kpblc-ini-write-value (file section key value / lst)
                              ;|
*    Выполняет запись значения ключа указанной секции ini-файла
*    Параметры вызова:
*	file	имя файла. nil недопустим
*	section	секция. nil недопустим
*	key	ключ. nil недопустим
*	value	записываемое значение. nil -> ""
*    Примеры вызова:
(_kpblc-ini-write-value "c:\\temp.ini" "group132" "key002" 2)
*    Возвращает строковое представление value, если запись была успешной либо
* nil в случае неудачи
|;
  (if (and file section key)
    (progn
      (_kpblc-ini-datas-write
        file
        (list
          (cons section
                (list (cons key (_kpblc-conv-value-to-string value)))
                ) ;_ end of cons
          ) ;_ end of list
        ) ;_ end of _kpblc-ini-datas-write
      (_kpblc-ini-read-value file section key)
      ) ;_ end of progn
    ) ;_ end of if
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
*	protected-function	- "защищаемая" функция
*	on-error-function	- функция, выполняемая в случае ошибки
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
*	func-name	имя функции, в которой возникла ошибка
*	msg		сообщение об ошибке
|;
  (princ
    (strcat "\n ** "
            (vl-string-trim
              "][ :\n<>"
              (vl-string-subst
                ""
                "error"
                (strcase (_kpblc-conv-value-to-string func-name) t)
                ) ;_ end of vl-string-subst
              ) ;_ end of vl-string-trim
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

(defun _kpblc-dir-path-and-splash (path)
                                  ;|
*    Возвращает путь со слешем в конце
*    Параметры вызова:
*	path	- обрабатываемый путь
*    Примеры вызова:
(_kpblc-dir-path-and-splash "c:\\kpblc-cad")	; "c:\\kpblc-cad\\"
|;
  (strcat (vl-string-right-trim "\\" path) "\\")
  ) ;_ end of defun

(defun _kpblc-dir-path-no-splash (path)
                                 ;|
*    Возвращает путь без слеша в конце
*    Параметры вызова:
*	path	- обрабатываемый путь
*    Примеры вызова:
(_kpblc-dir-path-no-splash "c:\\kpblc-cad\\")	; "c:\\kpblc-cad"
|;
  (vl-string-right-trim "\\" path)
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
(defun _kpblc-list-assoc (key lst)
                         ;|
*    Замена стандартному assoc
*    Параметры вызова:
	key	ключ
	lst	обрабатываеымй список
|;
  (if (= (type key) 'str)
    (setq key (strcase key))
    ) ;_ end of if
  (car
    (vl-remove-if-not
      (function
        (lambda (a / b)
          (and (setq b (car a))
               (or (and (= (type b) 'str) (= (strcase b) key))
                   (equal b key)
                   ) ;_ end of or
               ) ;_ end of and
          ) ;_ end of lambda
        ) ;_ end of function
      lst
      ) ;_ end of vl-remove-if-not
    ) ;_ end of car
  ) ;_ end of defun
