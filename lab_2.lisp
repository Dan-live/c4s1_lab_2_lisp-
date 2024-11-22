


(defun remove-seconds-and-reverse (lst process-func)
  "Видаляє кожен другий елемент зі списку, застосовує process-func до решти і розгортає результат."
  (labels ((helper (lst result keep)
             (cond
               ((null lst) result) 
               (keep (helper (cdr lst) (cons (funcall process-func (car lst)) result) nil)) 
               (t (helper (cdr lst) result t)))))
    (helper lst '() t)))

(defun identity-func (x) x) 
(print (remove-seconds-and-reverse '(1 2 a b 3 4 d) #'identity-func))


(defun list-set-difference (list1 list2 test-func)
  "Повертає новий список, що містить елементи зі list1, яких немає в list2, використовуючи test-func для перевірки."
  (labels ((helper (lst result)
             (cond
               ((null lst) (reverse result)) 
               ((funcall test-func (car lst) list2)
                (helper (cdr lst) result)) 
               (t (helper (cdr lst) (cons (car lst) result)))))) 
    (helper list1 '()))) 

(defun element-in-list-p (element list)
  (cond
    ((null list) nil)
    ((equal element (car list)) t)
    (t (element-in-list-p element (cdr list)))))

(print (list-set-difference '(1 2 3 4) '(3 4 5 6) #'element-in-list-p))



;тести
(defun check-remove-seconds-and-reverse (name input process-func expected)
  "Тестує `remove-seconds-and-reverse` із заданими вхідними даними та очікуваним результатом."
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (remove-seconds-and-reverse input process-func) expected)
          name))

(defun test-remove-seconds-and-reverse ()
  (check-remove-seconds-and-reverse "test 1" '(1 2 a b 3 4 d) #'identity-func '(d 3 a 1))
  (check-remove-seconds-and-reverse "test 2" '(1 2 3 4 5) #'identity-func '(5 3 1))
  (check-remove-seconds-and-reverse "test 3" '(1) #'identity-func '(1))
  (check-remove-seconds-and-reverse "test 4" '() #'identity-func '()))


(defun check-list-set-difference (name list1 list2 test-func expected)
  "Тестує `list-set-difference` із заданими вхідними даними та очікуваним результатом."
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (list-set-difference list1 list2 test-func) expected)
          name))

(defun test-list-set-difference ()
  (check-list-set-difference "test 1" '(1 2 3 4) '(3 4 5 6) #'element-in-list-p '(1 2))
  (check-list-set-difference "test 2" '(a b c) '(c d e) #'element-in-list-p '(a b))
  (check-list-set-difference "test 3" '() '(1 2 3) #'element-in-list-p '())
  (check-list-set-difference "test 4" '(1 2 3) '() #'element-in-list-p '(1 2 3)))


(test-remove-seconds-and-reverse)
(test-list-set-difference)
