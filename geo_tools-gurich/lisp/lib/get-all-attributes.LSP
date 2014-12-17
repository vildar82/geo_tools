(defun get-all-attributes (obj / )
  ;выводит атрибуты выбраного объекта в виде списка точечных пар (тег . значение)
  ; из интернета периименовал
  ;;;Use (get-all-attributes (car(entsel "\nSelect block:")))
  ;;;Returs list  (("TAG1" . "Value1")("TAG2" . "Value2") ...)
  (if (= (type obj) 'ENAME)
    (setq obj (vlax-ename->vla-object obj)))
  (if (and obj
	   (vlax-property-available-p obj 'Hasattributes)
	   (eq :vlax-true (vla-get-HasAttributes obj))
	   )
    (vl-catch-all-apply
      (function
	(lambda	()
	  (mapcar (function (lambda (x)
			      (list (vla-get-TagString x)
				    (vla-get-TextString x)
				    )
			      )
			    )
		  (append (vlax-invoke obj 'Getattributes)
			  (vlax-invoke obj 'Getconstantattributes)
			  )
		  )
	  )
	)
      )
    )
  );defun
;;; ѕример использовани€
;;;(get-all-attributes (car(entsel "\nSelect block:")))