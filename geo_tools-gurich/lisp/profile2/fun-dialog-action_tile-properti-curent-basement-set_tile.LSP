(defun fun-dialog-action_tile-properti-curent-basement-set_tile
       (sel-list-podval-str-item
	key-list
	profile-string-list
	/ n)
  (setq n 0)
  (repeat 9
    (if (eq (nth n (nth sel-list-podval-str-item profile-string-list)) "-")
      (progn
	(mode_tile (nth n key-list) 1)
	(set_tile (nth n key-list) "")
	)
      (progn
	(if (or (=(nth n key-list) "name")
		(=(nth n key-list) "h-str")
		(=(nth n key-list) "flag-circle")
		(=(nth n key-list) "min-angle"))
	  (mode_tile (nth n key-list) 1)
	  (mode_tile (nth n key-list) 0)
	  )
	(set_tile (nth n key-list) (nth n (nth sel-list-podval-str-item profile-string-list)))
	))
    (setq n (1+ n))
    )
  
  )