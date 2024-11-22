<p align="center"><b>Національний технічний університет України “Київський політехнічний інститут ім. Ігоря Сікорського”</b></p>
<p align="center"><b>Факультет прикладної математики Кафедра системного програмування і спеціалізованих комп’ютерних систем</b></p>
<p align="center"><b>ЛАБОРАТОРНА РОБОТА №2</b></p>
<p align="center"><b>з дисципліни «Вступ до функціонального програмування»</b></p>

<div align="right">
    <p>Студент: Горбик Данііл</p>
    <p>Група: КВ-13</p>
    <p>Рік: 2024</p>
</div>
## Загальне завдання
Реалізуйте дві рекурсивні функції, що виконують деякі дії з вхідним(и) списком(-ами), за можливості/необхідності використовуючи різні види рекурсії. Функції, які необхідно реалізувати, задаються варіантом (п. 2.1.1). Вимоги до функцій:

1. Зміна списку згідно із завданням має відбуватись за рахунок конструювання нового списку, а не зміни наявного (вхідного).
2. Не допускається використання функцій вищого порядку чи стандартних функцій для роботи зі списками, що не наведені в четвертому розділі навчального посібника.
3. Реалізована функція не має бути функцією вищого порядку, тобто приймати функції в якості аргументів.
4. Не допускається використання псевдофункцій (деструктивного підходу).
5. Не допускається використання циклів. Кожна реалізована функція має бути протестована для різних тестових наборів. Тести мають бути оформленні у вигляді модульних тестів (див. п. 2.3). Додатковий бал за лабораторну роботу можна отримати в разі виконання всіх наступних умов: робота виконана до дедлайну (включно з датою дедлайну) крім основних реалізацій функцій за варіантом, також реалізовано додатковий варіант однієї чи обох функцій, який працюватиме швидше за основну реалізацію, не порушуючи при цьому перші три вимоги до основної реалізації (вимоги 4 і 5 можуть бути порушені), за виключенням того, що в разі необхідності можна також використати стандартну функцію copy-list

## Завдання за варіантом №4

1. Написати функцію remove-seconds-and-reverse , яка видаляє зі списку кожен
   другий елемент і обертає результат у зворотному порядку:

```
CL-USER> (remove-seconds-and-reverse '(1 2 a b 3 4 d))
(D 3 A 1)
```

2. Написати функцію list-set-difference , яка визначає різницю двох множин,
   заданих списками атомів:

```
CL-USER> (list-set-difference '(1 2 3 4) '(3 4 5 6))
(1 2) ; порядок може відрізнятись
```

```lisp
;; Реалізація функції remove-seconds-and-reverse

(defun remove-seconds-and-reverse (lst process-func)
  (labels ((helper (lst result keep)
             (cond
               ((null lst) result)
               (keep (helper (cdr lst) (cons (funcall process-func (car lst)) result) nil))
               (t (helper (cdr lst) result t)))))
    (helper lst '() t)))

(defun identity-func (x) x)
(print (remove-seconds-and-reverse '(1 2 a b 3 4 d) #'identity-func))



;; Реалізація функції list-set-difference

(defun list-set-difference (list1 list2 test-func)
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

;; Функція для тестування remove-seconds-and-reverse

(defun check-remove-seconds-and-reverse (name input process-func expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (remove-seconds-and-reverse input process-func) expected)
          name))

(defun test-remove-seconds-and-reverse ()
  (check-remove-seconds-and-reverse "test 1" '(1 2 a b 3 4 d) #'identity-func '(d 3 a 1))
  (check-remove-seconds-and-reverse "test 2" '(1 2 3 4 5) #'identity-func '(5 3 1))
  (check-remove-seconds-and-reverse "test 3" '(1) #'identity-func '(1))
  (check-remove-seconds-and-reverse "test 4" '() #'identity-func '()))



;; Функція для тестування list-set-difference


(defun check-list-set-difference (name list1 list2 test-func expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (list-set-difference list1 list2 test-func) expected)
          name))

(defun test-list-set-difference ()
  (check-list-set-difference "test 1" '(1 2 3 4) '(3 4 5 6) #'element-in-list-p '(1 2))
  (check-list-set-difference "test 2" '(a b c) '(c d e) #'element-in-list-p '(a b))
  (check-list-set-difference "test 3" '() '(1 2 3) #'element-in-list-p '())
  (check-list-set-difference "test 4" '(1 2 3) '() #'element-in-list-p '(1 2 3)))


;;-----------------------------------------------------------------------------------------------------------------




## Результат виконання програми



(D 3 A 1)
(1 2) passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 1
passed... test 2
passed... test 3
passed... test 4


```
