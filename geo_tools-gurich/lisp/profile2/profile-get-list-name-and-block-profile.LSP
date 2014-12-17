(defun profile-get-list-name-and-block-profile (/ ss list-name-block-profile x)
  ; возвращает списоки точечных пар имен и объектов блоков профилей
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (if ss
    (foreach x (convert_ss_to_list ss)
      (if (vlax-ldata-get (vlax-ename->vla-object x) "profile-name")
	(setq list-name-block-profile (append list-name-block-profile (list (cons (vlax-ldata-get (vlax-ename->vla-object x) "profile-name") (vlax-ename->vla-object x)))))
	)
      )
    )
  list-name-block-profile
  )
(defun profile-get-list-name-profile ()
  (mapcar 'car(profile-get-list-name-and-block-profile))
  )
(defun profile-get-list-block-profile ()
  (mapcar 'cdr(profile-get-list-name-and-block-profile))
  )