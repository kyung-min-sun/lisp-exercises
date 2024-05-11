(defun test (x) (* x 2))
(mapcar #'(lambda (x) (+ x 10)) '(1 2 3))
;;; how would you implement a hashmap in lisp?
;;; how would you implement an O(1) access / push array in lisp? 
;;; that's pretty easy actually (use `nth x '(1, 2, 3)`)
(defun range (max) 
  (let ((range-array (make-array max))) 
  (progn
    (dotimes (i max)
      (setf (aref range-array i) i))
    range-array
  )))

(defun time-array-access (n) 
  (let ((x (range n)))
    (time (aref x (- n 1)))))

(defun behave (animal) (funcall (get animal 'behavior)))
(setf (get 'dog 'behavior) #'(lambda () (print "5")))

(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))

(defun custom-adder (x)
  #'(lambda (n &optional t)
      (if t) (setq n x)
      (+ x n)))

(defun count-instances (obj lsts)
  (labels ((instances-in (lst) 
    (if (consp lst) 
      (+ (if (eq (car lst) obj) 1 0)
         (instances-in (cdr lst)))
      0)))
      (mapcar #'instances-in lsts)))