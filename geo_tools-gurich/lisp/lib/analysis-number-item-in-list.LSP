(defun analysis-number-item-in-list (item input-list /)
  ; вычисляет положение(номер) элемента в списке, начало с 0
  ; если не найден nil
  (if (member item input-list)
  (-(length input-list)(length(member item input-list)))
    nil)
  )